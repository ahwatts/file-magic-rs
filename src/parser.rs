use anyhow::{bail, Result};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{not_line_ending, space0, space1},
    combinator::{eof, map, value},
    multi::many0_count,
    sequence::{pair, terminated, tuple},
    IResult,
};
use std::io::{BufRead, BufReader, Read};

use crate::magic::{MagicEntry, MagicSet, Test};

mod data_type;
mod number;
mod offset;
mod string;
mod test;

pub fn parse_set<R: Read>(filename: String, input: &mut R) -> Result<MagicSet> {
    let mut entries = Vec::new();
    let buf_input = BufReader::new(input);

    for (line_num_minus_one, line_rslt) in buf_input.lines().enumerate() {
        let line_num = line_num_minus_one + 1;
        let input = line_rslt?;

        match line(&input) {
            Ok((rest, Some(mut entry))) => {
                entry.filename = filename.clone();
                entry.line_num = line_num;
                entries.push(entry);
                if !rest.is_empty() {
                    eprintln!(
                        "Parsed an entry on line {}, but had leftover content: {:?}",
                        line_num, rest
                    );
                }
            }
            Ok((rest, None)) => {
                if !rest.is_empty() {
                    eprintln!(
                        "Parsed a comment or blank line on line {}, but had leftover content: {:?}",
                        line_num, rest
                    );
                }
            }
            Err(err) => {
                bail!("Parse error on line {}: {}", line_num, err);
            }
        }
    }

    let mut set = MagicSet::new(filename);
    set.add_entries(entries)?;
    Ok(set)
}

fn line(input: &str) -> IResult<&str, Option<MagicEntry>> {
    alt((
        // Blank line
        value(None, pair(space0, eof)),
        // Comment
        value(None, tuple((space0, tag("#"), not_line_ending, eof))),
        // Actual entry
        map(entry, Some),
    ))(input)
}

fn entry(input: &str) -> IResult<&str, MagicEntry> {
    // use crate::data_type::DataType::*;

    let (rest, level) = many0_count(tag(">"))(input)?;
    let (rest, offset) = offset::offset(rest)?;
    let (rest, _) = space1(rest)?;
    let (rest, data_type) = data_type::data_type(rest)?;
    let (_rest, _) = space1(rest)?;

    match data_type {
        // name_dt @ Name(..) => ???
        // use_dt @ Use(..) => ???
        _ => {
            let (rest, test_type) = test::test(&data_type, None, rest)?;
            let (rest, _) = space1(rest)?;
            let (rest, message) = terminated(not_line_ending, eof)(rest)?;

            Ok((
                rest,
                MagicEntry {
                    filename: String::new(),
                    line_num: 0,
                    level: level as u32,
                    offset,
                    test: Test::new(data_type, test_type),
                    message: message.to_string(),
                    mime_type: None,
                },
            ))
        }
    }
}

// fn test_type(input: &str) -> IResult<&str, TestType> {
//     alt((
//         value(TestType::AlwaysTrue, tuple((tag("x"), peek(space1)))),
//         map(
//             tuple((opt(test::string_operator), string::escaped_string)),
//             |(opt_op, string_val)| {
//                 TestType::String(StringTest::new(
//                     opt_op.unwrap_or(StringOp::Equal),
//                     string_val,
//                 ))
//             },
//         ),
//     ))(input)

//     // if let ok_rslt @ Ok(..) = r#try(token('x').with(look_ahead(space())).map(|_| TestType::AlwaysTrue)).parse(input.clone()) {
//     //     return ok_rslt;
//     // }

//     // if data_type == &data_type::DataType::String {
//     //     let ((op, string), rest) = (optional(parsers::string_operator()), parsers::escaped_string::<String, _>()).parse(input)?;
//     //     Ok((TestType::String(StringTest::new(op.unwrap_or(StringOp::Equal), string)), rest))
//     // } else {
//     //     let ((op, num), rest) = (optional(parsers::numeric_operator()), parsers::integer_bytes(data_type)).parse(input)?;
//     //     Ok((TestType::Number(NumericTest::new_from_bytes(op.unwrap_or(NumOp::Equal), num, opt_mask)), rest))
//     // }
// }

/*
fn entry<I>(line: I) -> CombParseResult<I, MagicEntry>
    where I: Stream<Item = char>
{
    let (level, rest) = many::<String, _>(r#try(token('>'))).parse(line).map(|(lv_str, rst)| (lv_str.len(), rst))?;
    let (offset, rest) = offset(rest)?;
    let (_, rest) = spaces().parse(rest)?;
    let ((data_type, opt_mask), rest) = data_type(rest)?;
    let (_, rest) = spaces().parse(rest)?;

    match data_type {
        mut name_dt @ data_type::DataType::Name(..) => {
            let ((name, _), rest) = (many::<String, _>(r#try(any())), eof()).parse(rest)?;
            name_dt = data_type::DataType::Name(name);

            Ok((
                MagicEntry {
                    filename: String::new(),
                    line_num: 0,
                    level: level as u32,
                    offset,
                    test: Test::new(name_dt, TestType::AlwaysTrue),
                    message: "".to_string(),
                    mime_type: None,
                },
                rest
            ))
        },
        mut use_dt @ data_type::DataType::Use(..) => {
            let ((name, _), rest) = (many::<String, _>(r#try(any())), eof()).parse(rest)?;
            use_dt = data_type::DataType::Use(name.clone());

            Ok((
                MagicEntry {
                    filename: String::new(),
                    line_num: 0,
                    level: level as u32,
                    offset,
                    test: Test::new(use_dt, TestType::UseList(name)),
                    message: "".to_string(),
                    mime_type: None,
                },
                rest
            ))
        },
        _ => {
            let (test_type, rest) = test_type(&data_type, opt_mask, rest)?;
            let (_, rest) = spaces().parse(rest)?;
            let ((message, _), rest) = (many::<String, _>(r#try(any())), eof()).parse(rest)?;
            let test = Test::new(data_type, test_type);

            Ok((
                MagicEntry {
                    filename: String::new(),
                    line_num: 0,
                    level: level as u32,
                    offset,
                    test,
                    message,
                    mime_type: None,
                },
                rest
            ))
        }
    }
}

fn data_type<I>(input: I) -> CombParseResult<I, (data_type::DataType, Option<Vec<u8>>)>
    where I: Stream<Item = char>
{
    let (dt, rest) = parsers::data_type().parse(input)?;
    let (opt_mask, rest) = optional(token('&').with(parsers::integer_bytes(&dt))).parse(rest)?;
    Ok(((dt, opt_mask), rest))
}

fn test_type<I>(data_type: &data_type::DataType, opt_mask: Option<Vec<u8>>, input: I) -> CombParseResult<I, TestType>
    where I: Stream<Item = char>
{
    if let ok_rslt @ Ok(..) = r#try(token('x').with(look_ahead(space())).map(|_| TestType::AlwaysTrue)).parse(input.clone()) {
        return ok_rslt;
    }

    if data_type == &data_type::DataType::String {
        let ((op, string), rest) = (optional(parsers::string_operator()), parsers::escaped_string::<String, _>()).parse(input)?;
        Ok((TestType::String(StringTest::new(op.unwrap_or(StringOp::Equal), string)), rest))
    } else {
        let ((op, num), rest) = (optional(parsers::numeric_operator()), parsers::integer_bytes(data_type)).parse(input)?;
        Ok((TestType::Number(NumericTest::new_from_bytes(op.unwrap_or(NumOp::Equal), num, opt_mask)), rest))
    }
}
*/

#[cfg(test)]
mod tests {
    use crate::{
        data_type::DataType,
        endian::Endian,
        magic::{DirectOffset, MagicEntry, NumOp, NumericTest, Offset, Test, TestType},
    };

    #[test]
    fn ignores_blank_lines() {
        assert_eq!(Ok(("", None)), super::line(""));
        assert_eq!(Ok(("", None)), super::line("    "));
        assert_eq!(Ok(("", None)), super::line("\t\t\t"));
        assert_eq!(Ok(("", None)), super::line("  \t  "));
    }

    #[test]
    fn ignores_comments() {
        assert_eq!(Ok(("", None)), super::line("#"));
        assert_eq!(Ok(("", None)), super::line("# Comment"));
        assert_eq!(Ok(("", None)), super::line("   # Comment"));
        assert_eq!(Ok(("", None)), super::line("  \t #\t"));
    }

    #[test]
    fn parse_entry() {
        let me = MagicEntry {
            filename: "".to_string(),
            line_num: 0,
            level: 0,
            offset: Offset::direct(DirectOffset::absolute(0)),
            test: Test::new(
                DataType::Long {
                    endian: Endian::Little,
                    signed: true,
                },
                TestType::Number(NumericTest::new(NumOp::Equal, 263i32, None)),
            ),
            message: "a.out little-endian 32-bit executable".to_string(),
            mime_type: None,
        };
        assert_eq!(
            Ok(("", me)),
            super::entry("0	lelong		0407		a.out little-endian 32-bit executable")
        );
    }

    #[test]
    fn parse_name_entry() {
        let me = MagicEntry {
            filename: "".to_string(),
            line_num: 0,
            level: 0,
            offset: Offset::direct(DirectOffset::absolute(0)),
            test: Test::new(
                DataType::Name("riff-walk".to_string()),
                TestType::AlwaysTrue,
            ),
            message: "".to_string(),
            mime_type: None,
        };
        assert_eq!(Ok(("", me)), super::entry("0	name	riff-walk"));
    }
}
