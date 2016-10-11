#![allow(dead_code)]

use entry::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParseError(String);
pub type ParseResult<T> = Result<T, ParseError>;

pub fn parse_entry(input: &str) -> ParseResult<MagicEntry> {
    let (_level, _rest) = try!(level(input));
    unimplemented!();
}

fn level(input: &str) -> ParseResult<(u32, &str)> {
    let mut level = 0;
    let mut rest_start = 0;
    for (i, c) in input.chars().enumerate() {
        if c == '>' {
            level += 1;
        } else {
            rest_start = i;
            break;
        }
    }

    Ok((level, &input[rest_start..]))
}

fn offset(_input: &str) -> ParseResult<(Offset, &str)> {
    unimplemented!();
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_read_levels() {
        assert_eq!(Ok((3, "1234")), super::level(">>>1234"));
        assert_eq!(Ok((0, "1234")), super::level("1234"));
        assert_eq!(Ok((0, "")), super::level(""));
    }
}
