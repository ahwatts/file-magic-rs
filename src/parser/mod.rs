use combine::*;
use combine::char::*;
use endian::Endian;
use magic::*;
use error::{MagicError, MagicResult};
use self::parsers::*;
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
                return Err(MagicError::Parse(format!("Parse error on line {}: {}", line_num, err)));
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
    let comment = (spaces(), token('#').or(token('!')), many::<String, _>(try(any()))).map(|_| None);
    let mut ignorer = try(blank).or(comment);

    match ignorer.parse(line.clone()) {
        v @ Ok((None, _)) => return v,
        Ok((Some(..), _)) => unreachable!(),
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
    let (data_type, rest) = try!(data_type(rest));
    let (_, rest) = try!(spaces().parse(rest));
    let (test_val, rest) = try!(test_value(data_type, rest));
    let (_, rest) = try!(spaces().parse(rest));
    let ((message, _), rest) = try!((many::<String, _>(try(any())), eof()).parse(rest));

    Ok((
        MagicEntry {
            filename: String::new(),
            line_num: 0,
            level: level as u32,
            offset: offset,
            // data_type: data_type,
            test: test_val,
            message: message,
        },
        rest
    ))
}

fn offset<I>(input: I) -> CombParseResult<I, Offset>
    where I: Stream<Item = char>
{
    integer(DataType::Quad { endian: Endian::Native, signed: false }).parse(input).map(|(num, rest)| {
        if let NumericValue::UQuad(n) = num {
            (Offset::direct(DirectOffset::absolute(n)), rest)
        } else {
            unreachable!()
        }
    })
}

#[derive(Clone, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum DataType {
    Byte  {                 signed: bool },
    Short { endian: Endian, signed: bool },
    Long  { endian: Endian, signed: bool },
    Quad  { endian: Endian, signed: bool },
    Float(Endian),
    Double(Endian),

    String,

    // Id3(Endian),

    // LongDate(Endian, TimeZone),
    // QuadDate(Endian, TimeZone),
    // WindowsDate(Endian),

    // String, PascalString, BigEndianString16, LittleEndianString16,
    // Indirect, Name, Use,
    // Regex, Search,
    // Default, Clear,
}

impl DataType {
    pub fn endian(&self) -> Endian {
        use self::DataType::*;
        match self {
            &Byte  {..} => Endian::Native,
            &Short { endian: e, signed: _ } => e,
            &Long  { endian: e, signed: _ } => e,
            &Quad  { endian: e, signed: _ } => e,
            &Float(e) => e,
            &Double(e) => e,
            &String => Endian::Native,
        }
    }
}

fn data_type<I>(input: I) -> CombParseResult<I, DataType>
    where I: Stream<Item = char>
{
    use self::DataType::*;
    use endian::Endian::*;

    choice([
        try(string("byte").with(value(Byte { signed: true }))),

        try(string("short")  .with(value(Short { endian: Native, signed: true }))),
        try(string("beshort").with(value(Short { endian: Big,    signed: true }))),
        try(string("leshort").with(value(Short { endian: Little, signed: true }))),

        try(string("long")  .with(value(Long { endian: Native, signed: true }))),
        try(string("belong").with(value(Long { endian: Big,    signed: true }))),
        try(string("lelong").with(value(Long { endian: Little, signed: true }))),
        try(string("melong").with(value(Long { endian: Pdp11,  signed: true }))),

        try(string("quad")  .with(value(Quad { endian: Native, signed: true }))),
        try(string("bequad").with(value(Quad { endian: Big,    signed: true }))),
        try(string("lequad").with(value(Quad { endian: Little, signed: true }))),

        try(string("float")  .with(value(Float(Native)))),
        try(string("befloat").with(value(Float(Big)))),
        try(string("lefloat").with(value(Float(Little)))),

        try(string("double")  .with(value(Double(Native)))),
        try(string("bedouble").with(value(Double(Big)))),
        try(string("ledouble").with(value(Double(Little)))),

        try(string("string").with(value(String))),
    ]).parse(input)
}

fn test_value<I: Stream<Item = char>>(data_type: DataType, input: I) -> CombParseResult<I, Test> {
    token('x').with(look_ahead(space())).map(|_| Test::AlwaysTrue)
        .or(try((optional(numeric_operator()), integer(data_type.clone())).map(|(op, n)| {
            Test::Number(NumericTest::new(&data_type, op.unwrap_or(NumOp::Equal), n))
        })))
        // .or(try((optional(string_operator()), many1::<String, _>(satisfy(|c| c != ' ' && c != '\t')))
        //         .map(|(op, s)| Test::String { op: op.unwrap_or(StrOp::Equal), value: s })))
        .parse(input)
}

#[cfg(test)]
mod tests {
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
        assert_eq!(Ok((Offset::direct(DirectOffset::absolute(108)), "")), super::offset("108"));
        assert_eq!(Ok((Offset::direct(DirectOffset::absolute(108)), "")), super::offset("0x6c"));
    }

    #[test]
    fn data_type() {
        use super::DataType::*;
        use endian::Endian::*;

        assert_eq!(Ok((Byte { signed: true  }, "")), super::data_type("byte"));

        assert_eq!(Ok((Short { endian: Native, signed: true }, "")), super::data_type("short"));
        assert_eq!(Ok((Short { endian: Big,    signed: true }, "")), super::data_type("beshort"));
        assert_eq!(Ok((Short { endian: Little, signed: true }, "")), super::data_type("leshort"));

        assert_eq!(Ok((Long { endian: Native, signed: true }, "")), super::data_type("long"));
        assert_eq!(Ok((Long { endian: Big,    signed: true }, "")), super::data_type("belong"));
        assert_eq!(Ok((Long { endian: Little, signed: true }, "")), super::data_type("lelong"));
        assert_eq!(Ok((Long { endian: Pdp11,  signed: true }, "")), super::data_type("melong"));

        assert_eq!(Ok((Quad { endian: Native, signed: true }, "")), super::data_type("quad"));
        assert_eq!(Ok((Quad { endian: Big,    signed: true }, "")), super::data_type("bequad"));
        assert_eq!(Ok((Quad { endian: Little, signed: true }, "")), super::data_type("lequad"));

        // assert_eq!(Ok((Byte { signed: false }, "")), super::data_type("ubyte"));

        assert_eq!(Ok((Float(Native), "")), super::data_type("float"));
        assert_eq!(Ok((Float(Big),    "")), super::data_type("befloat"));
        assert_eq!(Ok((Float(Little), "")), super::data_type("lefloat"));

        assert_eq!(Ok((Double(Native), "")), super::data_type("double"));
        assert_eq!(Ok((Double(Big),    "")), super::data_type("bedouble"));
        assert_eq!(Ok((Double(Little), "")), super::data_type("ledouble"));
    }

    #[test]
    fn numeric_test_values() {
        use magic::Test::*;
        use super::DataType::*;
        use endian::Endian::*;
        use magic::NumericValue::*;
        use magic::NumOp::*;

        assert_eq!(Ok((AlwaysTrue, " ")), super::test_value(Byte { signed: false }, "x "));

        assert_eq!(
            Ok((Number(NumericTest { endian: Native, logic_op: Equal, test_value: SLong(305) }), "")),
            super::test_value(Long { endian: Native, signed: true }, "305"));

        assert_eq!(
            Ok((Number(NumericTest { endian: Little, logic_op: Equal, test_value: SQuad(305) }), "")),
            super::test_value(Quad { endian: Little, signed: true }, "=305"));

        assert_eq!(
            Ok((Number(NumericTest { endian: Big, logic_op: GreaterThan, test_value: UShort(48_879) }), "")),
            super::test_value(Short { endian: Big, signed: false }, ">0xBeef"));
    }
}
