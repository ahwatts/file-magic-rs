use combine::*;
use combine::char::*;
use entry::*;
use self::parsers::*;
use std::io::{BufRead, BufReader, Read};

mod parsers;

pub fn parse<R: Read>(input: &mut R) -> Result<Vec<MagicEntry>, String> {
    let mut entries = Vec::new();
    let buf_input = BufReader::new(input);

    for (line_num_minus_one, line_rslt) in buf_input.lines().enumerate() {
        let line_num = line_num_minus_one + 1;
        let line = try!(line_rslt.map_err(|e| format!("I/O Error: {}", e)));

        match parse_line(line.as_str()) {
            Ok((Some(entry), rest)) => {
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
                return Err(format!("Parse error on line {}: {}", line_num, err));
            }
        }
    }

    Ok(entries)
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
        Ok((Some(..), _)) => unreachable!(),
        Err(..) => {}
    }

    entry(line).map(|(ent, rest)| {
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
    let (_test_str, rest) = try!(many1::<String, _>(try(satisfy(|c| c != ' ' && c != '\t'))).parse(rest));
    let (_, rest) = try!(spaces().parse(rest));
    let ((message, _), rest) = try!((many1::<String, _>(try(any())), eof()).parse(rest));

    Ok((
        MagicEntry {
            level: level as u32,
            offset: offset,
            data_type: data_type,
            test: Test,
            message: message,
        },
        rest
    ))
}

fn offset<I>(input: I) -> CombParseResult<I, Offset>
    where I: Stream<Item = char>
{
    unsigned_number().parse(input).map(|(n, rest)| {
        (Offset::direct(DirectOffset::absolute(n)), rest)
    })
}

fn data_type<I>(input: I) -> CombParseResult<I, DataType>
    where I: Stream<Item = char>
{
    use entry::DataType::*;
    use entry::Endian::*;

    let (type_str, rest) = try! {
        choice([
            try(string("byte")),

            try(string("short")),
            try(string("beshort")),
            try(string("leshort")),

            try(string("long")),
            try(string("belong")),
            try(string("lelong")),
            try(string("melong")),

            try(string("quad")),
            try(string("bequad")),
            try(string("lequad")),

            try(string("float")),
            try(string("befloat")),
            try(string("lefloat")),

            try(string("double")),
            try(string("bedouble")),
            try(string("ledouble")),
        ]).parse(input)
    };

    match type_str {
        "byte" => Ok((Byte { signed: true }, rest)),

        "short"   => Ok((Short { endian: Native, signed: true }, rest)),
        "beshort" => Ok((Short { endian: Big,    signed: true }, rest)),
        "leshort" => Ok((Short { endian: Little, signed: true }, rest)),

        "long"   => Ok((Long { endian: Native, signed: true }, rest)),
        "belong" => Ok((Long { endian: Big,    signed: true }, rest)),
        "lelong" => Ok((Long { endian: Little, signed: true }, rest)),
        "melong" => Ok((Long { endian: Pdp11,  signed: true }, rest)),

        "quad"   => Ok((Quad { endian: Native, signed: true }, rest)),
        "bequad" => Ok((Quad { endian: Big,    signed: true }, rest)),
        "lequad" => Ok((Quad { endian: Little, signed: true }, rest)),

        "float"   => Ok((Float(Native), rest)),
        "befloat" => Ok((Float(Big),    rest)),
        "lefloat" => Ok((Float(Little), rest)),

        "double"   => Ok((Double(Native), rest)),
        "bedouble" => Ok((Double(Big),    rest)),
        "ledouble" => Ok((Double(Little), rest)),

        _ => {
            unexpected(format!("data type: {}", type_str))
                .parse(rest)
                .map(|_| unreachable!())
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn ignores_blank_lines() {
        assert_eq!(Ok((None, "")), super::parse_line("  \t  "));
    }

    #[test]
    fn ignores_comments() {
        assert_eq!(Ok((None, "")), super::parse_line(" \t# Comment"));
    }
}
