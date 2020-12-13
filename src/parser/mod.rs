use crate::magic::MagicSet;
use anyhow::Result;
use std::io::{BufRead, BufReader, Read};

// mod parsers;

pub fn parse_set<R: Read>(filename: String, input: &mut R) -> Result<MagicSet> {
    let entries = Vec::new();
    let buf_input = BufReader::new(input);

    for (line_num_minus_one, line_rslt) in buf_input.lines().enumerate() {
        let _line_num = line_num_minus_one + 1;
        let _line = line_rslt?;

        /* match parse_line(line.as_str()) {
            Ok((Some(mut entry), rest)) => {
                entry.filename = filename.clone();
                entry.line_num = line_num;
                entries.push(entry);
                if !rest.is_empty() {
                    println!(
                        "Parsed an entry on line {}, but had leftover content: {:?}",
                        line_num, rest
                    );
                }
            }
            Ok((None, rest)) => {
                if !rest.is_empty() {
                    println!("Parsed a comment or blank line  on line {}, but had leftover content: {:?}", line_num, rest);
                }
            }
            Err(err) => {
                let translated = err.translate_position(line.as_str());
                bail!("Parse error on line {}: {}", line_num, translated);
            }
        } */
    }

    let mut set = MagicSet::new(filename);
    set.add_entries(entries)?;
    Ok(set)
}

/*
type CombParseResult<I, O> = Result<(O, I), ParseError<I>>;

fn parse_line<I>(line: I) -> Result<Option<MagicEntry>> {
    let blank = (spaces(), eof()).map(|_| None);
    let comment = (spaces(), token('#'), many::<String, _>(r#try(any()))).map(|_| None);
    let mut ignorer = r#try(blank).or(comment);

    match ignorer.parse(line.clone()) {
        v @ Ok((None, _)) => return v,
        Ok((Some(v), _)) => unreachable!("Parsed something from blanks or comments (!?!?): {:?}", v),
        Err(..) => {}
    }

    entry(line).map(|(ent, rest)| {
        // println!("{:?}", ent);
        (Some(ent), rest)
    })
}

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

fn offset<I>(input: I) -> CombParseResult<I, Offset>
    where I: Stream<Item = char>
{
    parsers::offset().parse(input)
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

#[cfg(test)]
mod tests {
    use crate::data_type::DataType;
    use crate::endian::Endian;
    use crate::magic::*;

    #[test]
    fn ignores_blank_lines() {
        assert_eq!(Ok((None, "")), super::parse_line(""));
        assert_eq!(Ok((None, "")), super::parse_line("    "));
        assert_eq!(Ok((None, "")), super::parse_line("\t\t\t"));
        assert_eq!(Ok((None, "")), super::parse_line("  \t  "));
    }

    #[test]
    fn ignores_comments() {
        assert_eq!(Ok((None, "")), super::parse_line("#"));
        assert_eq!(Ok((None, "")), super::parse_line("# Comment"));
        assert_eq!(Ok((None, "")), super::parse_line("   # Comment"));
        assert_eq!(Ok((None, "")), super::parse_line("  \t #\t"));
    }

    #[test]
    fn direct_offset() {
        assert_eq!(Ok((Offset::Direct(DirectOffset::Absolute(108)), "")), super::offset("108"));
        assert_eq!(Ok((Offset::Direct(DirectOffset::Absolute(108)), "")), super::offset("0x6c"));
        assert_eq!(Ok((Offset::Direct(DirectOffset::Relative(-108)), "")), super::offset("&-108"));
        assert_eq!(Ok((Offset::Direct(DirectOffset::Relative(108)), "")), super::offset("&0x6c"));
    }

    #[test]
    fn data_type_with_mask() {
        assert_eq!(
            Ok(((DataType::Short { signed: true, endian: Endian::Little }, Some(vec![ 0xff, 0x3f ])), "")),
            super::data_type("leshort&0x3fff")
        );
    }

    #[test]
    fn always_true_test_type() {
        let dt = DataType::Byte { signed: false };
        assert_eq!(Ok((TestType::AlwaysTrue, " ")), super::test_type(&dt, None, "x "));
    }

    #[test]
    fn numeric_test_values() {
        let dt = DataType::Byte { signed: false };
        assert_eq!(Ok((TestType::AlwaysTrue, "\t")), super::test_type(&dt, None, "x\t"));

        let dt = DataType::Long { endian: Endian::Native, signed: true };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::Equal, 305i32, None)), "")),
            super::test_type(&dt, None, "305"));

        let dt = DataType::Quad { endian: Endian::Little, signed: true };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::Equal, -305i64, None)), "")),
            super::test_type(&dt, None, "=-305"));

        let dt = DataType::Short { endian: Endian::Big, signed: false };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::GreaterThan, 48_879u16, None)), "")),
            super::test_type(&dt, None, ">0xBeef"));

        let dt = DataType::Long { endian: Endian::Native, signed: false };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::Equal, 263u32, None)), "")),
            super::test_type(&dt, None, "0407"));
    }

    #[test]
    fn numeric_test_value_with_mask() {
        let dt = DataType::Long { endian: Endian::Native, signed: true };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::Equal, 305i32, Some(vec![ 0xff, 0xff, 0xff, 0xff ]))), "")),
            super::test_type(&dt, Some(vec![ 0xff, 0xff, 0xff, 0xff ]), "305"));
    }

    #[test]
    fn string_test_values() {
        let dt = DataType::String;
        assert_eq!(
            Ok((TestType::String(StringTest::new(StringOp::Equal, "fmt ")), "")),
            super::test_type(&dt, None, "fmt\\x20"));
    }

    #[test]
    fn parse_entry() {
        let me = MagicEntry {
            filename: "".to_string(),
            line_num: 0,
            level: 0,
            offset: Offset::direct(DirectOffset::absolute(0)),
            test: Test::new(
                DataType::Long { endian: Endian::Little, signed: true },
                TestType::Number(NumericTest::new(NumOp::Equal, 263i32, None))),
            message: "a.out little-endian 32-bit executable".to_string(),
            mime_type: None,
        };
        assert_eq!(Ok((me, "")), super::entry(
            "0	lelong		0407		a.out little-endian 32-bit executable"
        ));
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
                TestType::AlwaysTrue),
            message: "".to_string(),
            mime_type: None,
        };
        assert_eq!(Ok((me, "")), super::entry(
            "0	name	riff-walk"
        ));
    }
}
*/
