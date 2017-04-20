use combine::*;
use combine::char::*;
use data_type;
use error::{MagicError, MagicResult};
use magic::*;
use std::io::{BufRead, BufReader, Read};

mod parsers;

pub fn parse_set<R: Read>(filename: String, input: &mut R) -> MagicResult<MagicSet> {
    let mut entries = Vec::new();
    let buf_input = BufReader::new(input);

    for (line_num_minus_one, line_rslt) in buf_input.lines().enumerate() {
        let line_num = line_num_minus_one + 1;
        let line = try!(line_rslt);

        match parse_line(line.as_str()) {
            Ok((Some(mut entry), rest)) => {
                entry.filename = filename.clone();
                entry.line_num = line_num;
                entries.push(entry);
                if rest.len() > 0 {
                    println!("Parsed an entry on line {}, but had leftover content: {:?}", line_num, rest);
                }
            },
            Ok((None, rest)) => {
                if rest.len() > 0 {
                    println!("Parsed a comment or blank line  on line {}, but had leftover content: {:?}", line_num, rest);
                }
            },
            Err(err) => {
                let translated = err.translate_position(line.as_str());
                return Err(MagicError::Parse(format!("Parse error on line {}: {}", line_num, translated)));
            }
        }
    }

    let mut set = MagicSet::new(filename);
    try!(set.add_entries(entries));
    Ok(set)
}

type CombParseResult<I, O> = Result<(O, I), ParseError<I>>;

fn parse_line<I>(line: I) -> CombParseResult<I, Option<MagicEntry>>
    where I: Stream<Item = char> + Clone
{
    let blank = (spaces(), eof()).map(|_| None);
    let comment = (spaces(), token('#'), many::<String, _>(try(any()))).map(|_| None);
    let mut ignorer = try(blank).or(comment);

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
    let (level, rest) = try!(many::<String, _>(try(token('>'))).parse(line).map(|(lv_str, rst)| (lv_str.len(), rst)));
    let (offset, rest) = try!(offset(rest));
    let (_, rest) = try!(spaces().parse(rest));
    let ((data_type, _opt_mask), rest) = try!(data_type(rest));
    let (_, rest) = try!(spaces().parse(rest));

    match data_type {
        mut name_dt @ data_type::DataType::Name(..) => {
            let ((name, _), rest) = try!((many::<String, _>(try(any())), eof()).parse(rest));
            name_dt = data_type::DataType::Name(name);

            Ok((
                MagicEntry {
                    filename: String::new(),
                    line_num: 0,
                    level: level as u32,
                    offset: offset,
                    test: Test::new(name_dt, TestType::AlwaysTrue),
                    message: "".to_string(),
                },
                rest
            ))
        },
        mut use_dt @ data_type::DataType::Use(..) => {
            let ((name, _), rest) = try!((many::<String, _>(try(any())), eof()).parse(rest));
            use_dt = data_type::DataType::Use(name.clone());

            Ok((
                MagicEntry {
                    filename: String::new(),
                    line_num: 0,
                    level: level as u32,
                    offset: offset,
                    test: Test::new(use_dt, TestType::UseList(name)),
                    message: "".to_string(),
                },
                rest
            ))
        },
        _ => {
            let (test_type, rest) = try!(test_type(&data_type, rest));
            let (_, rest) = try!(spaces().parse(rest));
            let ((message, _), rest) = try!((many::<String, _>(try(any())), eof()).parse(rest));

            Ok((
                MagicEntry {
                    filename: String::new(),
                    line_num: 0,
                    level: level as u32,
                    offset: offset,
                    test: Test::new(data_type, test_type),
                    message: message,
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

fn test_type<I>(data_type: &data_type::DataType, input: I) -> CombParseResult<I, TestType>
    where I: Stream<Item = char>
{
    if let ok_rslt @ Ok(..) = try(token('x').with(look_ahead(space())).map(|_| TestType::AlwaysTrue)).parse(input.clone()) {
        return ok_rslt;
    }

    if data_type == &data_type::DataType::String {
        (optional(parsers::string_operator()), parsers::escaped_string::<String, _>()).map(|(op, string)| {
            TestType::String(StringTest::new(op.unwrap_or(StringOp::Equal), string))
        }).parse(input)
    } else {
        (optional(parsers::numeric_operator()), parsers::integer_bytes(data_type)).map(|(op, num)| {
            TestType::Number(NumericTest::new_from_bytes(op.unwrap_or(NumOp::Equal), num))
        }).parse(input)
    }
}

#[cfg(test)]
mod tests {
    use data_type::DataType;
    use endian::Endian;
    use magic::*;

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
    fn always_true_test_type() {
        let dt = DataType::Byte { signed: false };
        assert_eq!(Ok((TestType::AlwaysTrue, " ")), super::test_type(&dt, "x "));
    }

    #[test]
    fn numeric_test_values() {
        let dt = DataType::Byte { signed: false };
        assert_eq!(Ok((TestType::AlwaysTrue, "\t")), super::test_type(&dt, "x\t"));

        let dt = DataType::Long { endian: Endian::Native, signed: true };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::Equal, 305i32)), "")),
            super::test_type(&dt, "305"));

        let dt = DataType::Quad { endian: Endian::Little, signed: true };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::Equal, -305i64)), "")),
            super::test_type(&dt, "=-305"));

        let dt = DataType::Short { endian: Endian::Big, signed: false };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::GreaterThan, 48_879u16)), "")),
            super::test_type(&dt, ">0xBeef"));

        let dt = DataType::Long { endian: Endian::Native, signed: false };
        assert_eq!(
            Ok((TestType::Number(NumericTest::new(NumOp::Equal, 263u32)), "")),
            super::test_type(&dt, "0407"));
    }

    #[test]
    fn string_test_values() {
        let dt = DataType::String;
        assert_eq!(
            Ok((TestType::String(StringTest::new(StringOp::Equal, "fmt ")), "")),
            super::test_type(&dt, "fmt\\x20"));
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
                TestType::Number(NumericTest::new(NumOp::Equal, 263i32))),
            message: "a.out little-endian 32-bit executable".to_string(),
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
        };
        assert_eq!(Ok((me, "")), super::entry(
            "0	name	riff-walk"
        ));
    }
}
