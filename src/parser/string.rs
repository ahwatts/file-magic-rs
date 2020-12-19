use anyhow::anyhow;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{hex_digit1, oct_digit1, one_of, satisfy},
    combinator::map_res,
    multi::fold_many0,
    sequence::preceded,
    IResult,
};

use crate::magic::{NumOp, StringOp};

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

pub fn escape_sequence(input: &str) -> IResult<&str, char> {
    preceded(
        tag("\\"),
        alt((
            map_res(one_of("\\nrtb"), |c| match c {
                '\\' => Ok('\\'),
                'n' => Ok('\n'),
                'r' => Ok('\r'),
                't' => Ok('\t'),
                'b' => Ok('\u{08}'), // backspace
                _ => Err(anyhow!("Unknown escape sequence: \\{}", c)),
            }),
            map_res(preceded(tag("x"), hex_digit1), |hex_str| {
                u32::from_str_radix(hex_str, 16)
                    .map_err(|e| {
                        anyhow!(
                            "Unable to parse escaped hex value from {:?}: {}",
                            hex_str,
                            e
                        )
                    })
                    .and_then(|n| {
                        std::char::from_u32(n)
                            .ok_or(anyhow!("Incorrect unicode scalar value: {:?}", n))
                    })
            }),
            map_res(oct_digit1, |oct_str| {
                u32::from_str_radix(oct_str, 8)
                    .map_err(|e| {
                        anyhow!(
                            "Unable to parse escaped octal value from {:?}: {}",
                            oct_str,
                            e
                        )
                    })
                    .and_then(|n| {
                        std::char::from_u32(n)
                            .ok_or(anyhow!("Incorrect unicode scalar value: {:?}", n))
                    })
            }),
        )),
    )(input)
}

pub fn escaped_string(input: &str) -> IResult<&str, String> {
    fold_many0(
        alt((escape_sequence, satisfy(|c| c != ' ' && c != '\t'))),
        // Use input.len() as a rough guess for how long the resulting string
        // should be, to hopefully avoid doing too many allocations.
        String::with_capacity(input.len()),
        |mut string, c| {
            string.push(c);
            string
        },
    )(input)
}

#[cfg(test)]
mod tests {
    #[test]
    fn escaped_strings() {
        assert_eq!(
            Ok(("", "fmt ".to_string())),
            super::escaped_string("fmt\\x20"),
        );

        assert_eq!(
            Ok((" ", "entry\r".to_string())),
            super::escaped_string("entry\\r "),
        )
    }

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
    fn escaped_chars() {
        assert_eq!(Ok(("", '\n')), super::escape_sequence("\\n"));
        assert_eq!(Ok(("", '\r')), super::escape_sequence("\\r"));
        assert_eq!(Ok(("", '\t')), super::escape_sequence("\\t"));
        assert_eq!(Ok(("", '\\')), super::escape_sequence("\\\\"));
        assert_eq!(Ok(("", '\0')), super::escape_sequence("\\0"));
        assert_eq!(Ok(("", '\x0E')), super::escape_sequence("\\16"));
        assert_eq!(Ok(("", '\x0E')), super::escape_sequence("\\016"));
        assert_eq!(Ok(("", ' ')), super::escape_sequence("\\x20"));
    }
}
