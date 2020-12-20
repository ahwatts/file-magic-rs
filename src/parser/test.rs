use anyhow::anyhow;
use nom::{IResult, branch::alt, bytes::complete::tag, character::complete::{one_of, space1}, combinator::{map, map_res, opt, peek, value}, error::ParseError, sequence::{pair, tuple}};

use crate::{
    data_type::DataType,
    magic::{NumOp, NumericTest, StringOp, StringTest, TestType},
    parser::{number::unsigned_integer, string::escaped_string},
};

pub fn test<I, E>(
    data_type: &DataType,
    opt_mask: Option<Vec<u8>>,
    input: &str,
) -> IResult<&str, TestType>
{
    if *data_type == DataType::String {
        alt((
            value(TestType::AlwaysTrue, tuple((tag("x"), peek(space1)))),
            map(
                pair(opt(string_operator), escaped_string),
                |(opt_op, string_val)| {
                    TestType::String(StringTest::new(
                        opt_op.unwrap_or(StringOp::Equal),
                        string_val,
                    ))
                },
            ),
        ))(input)
    } else {
        alt((
            value(TestType::AlwaysTrue, tuple((tag("x"), peek(space1)))),
            map(
                pair(opt(numeric_operator), unsigned_integer),
                |(opt_op, num_val)| {
                    TestType::Number(NumericTest::new(
                        opt_op.unwrap_or(NumOp::Equal),
                        num_val,
                        opt_mask,
                    ))
                },
            ),
        ))(input)
    }
}

pub fn numeric_operator(input: &str) -> IResult<&str, NumOp> {
    use crate::magic::NumOp::*;
    map_res(one_of("=<>!"), |op_char| match op_char {
        '=' => Ok(Equal),
        '<' => Ok(LessThan),
        '>' => Ok(GreaterThan),
        '!' => Ok(NotEqual),
        _ => Err(anyhow!("Unknown numeric operator: {:?}", op_char)),
    })(input)
}

pub fn string_operator(input: &str) -> IResult<&str, StringOp> {
    use crate::magic::StringOp::*;
    map_res(one_of("=<>"), |op_char| match op_char {
        '=' => Ok(Equal),
        '<' => Ok(LexBefore),
        '>' => Ok(LexAfter),
        _ => Err(anyhow!("Unknown string operator: {:?}", op_char)),
    })(input)
}

#[cfg(test)]
mod tests {
    #[test]
    fn numerical_operators() {
        use crate::magic::NumOp::*;
        assert_eq!(Ok(("", Equal)), super::numeric_operator("="));
        assert_eq!(Ok(("", GreaterThan)), super::numeric_operator(">"));
        assert_eq!(Ok(("", LessThan)), super::numeric_operator("<"));
        assert_eq!(Ok(("", NotEqual)), super::numeric_operator("!"));
    }

    #[test]
    fn string_operators() {
        use crate::magic::StringOp::*;
        assert_eq!(Ok(("", Equal)), super::string_operator("="));
        assert_eq!(Ok(("", LexBefore)), super::string_operator("<"));
        assert_eq!(Ok(("", LexAfter)), super::string_operator(">"));
    }
}
