use anyhow::anyhow;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{one_of, space1},
    combinator::{map, map_res, opt, peek, value},
    sequence::{pair, tuple},
    IResult,
};

use crate::{
    data_type::DataType,
    magic::{NumOp, NumericTest, StringOp, StringTest, TestType},
    parser::{number::unsigned_integer, string::escaped_string},
};

pub fn test<'a>(
    data_type: &DataType,
    opt_mask: Option<Vec<u8>>,
    input: &'a str,
) -> IResult<&'a str, TestType> {
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
                pair(opt(numeric_operator), unsigned_integer::<u32>),
                |(opt_op, num_val)| {
                    TestType::Number(NumericTest::new(
                        opt_op.unwrap_or(NumOp::Equal),
                        num_val,
                        opt_mask.clone(),
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
    use crate::{
        data_type::DataType,
        endian::Endian,
        magic::{NumOp, NumericTest, StringOp, StringTest, TestType},
    };

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

    #[test]
    fn always_true_test_type() {
        let dt = DataType::Byte { signed: false };
        assert_eq!(
            Ok((" ", TestType::AlwaysTrue)),
            super::test(&dt, None, "x ")
        );
    }

    #[test]
    fn numeric_test_values() {
        let dt = DataType::Byte { signed: false };
        assert_eq!(
            Ok(("\t", TestType::AlwaysTrue)),
            super::test(&dt, None, "x\t")
        );

        let dt = DataType::Long {
            endian: Endian::Native,
            signed: true,
        };
        assert_eq!(
            Ok((
                "",
                TestType::Number(NumericTest::new(NumOp::Equal, 305i32, None))
            )),
            super::test(&dt, None, "305")
        );

        let dt = DataType::Quad {
            endian: Endian::Little,
            signed: true,
        };
        assert_eq!(
            Ok((
                "",
                TestType::Number(NumericTest::new(NumOp::Equal, -305i64, None))
            )),
            super::test(&dt, None, "=-305")
        );

        let dt = DataType::Short {
            endian: Endian::Big,
            signed: false,
        };
        assert_eq!(
            Ok((
                "",
                TestType::Number(NumericTest::new(NumOp::GreaterThan, 48_879u16, None))
            )),
            super::test(&dt, None, ">0xBeef")
        );

        let dt = DataType::Long {
            endian: Endian::Native,
            signed: false,
        };
        assert_eq!(
            Ok((
                "",
                TestType::Number(NumericTest::new(NumOp::Equal, 263u32, None))
            )),
            super::test(&dt, None, "0407")
        );
    }

    // #[test]
    // fn numeric_test_value_with_mask() {
    //     let dt = DataType::Long { endian: Endian::Native, signed: true };
    //     assert_eq!(
    //         Ok(("", TestType::Number(NumericTest::new(NumOp::Equal, 305i32, Some(vec![ 0xff, 0xff, 0xff, 0xff ]))))),
    //         super::test_type(&dt, Some(vec![ 0xff, 0xff, 0xff, 0xff ]), "305"));
    // }

    #[test]
    fn string_test_values() {
        let dt = DataType::String;
        assert_eq!(
            Ok((
                "",
                TestType::String(StringTest::new(StringOp::Equal, "fmt "))
            )),
            super::test(&dt, None, "fmt\\x20")
        );
    }
}
