use combine::*;
use combine::char::*;
use entry::*;
use std::io::{BufRead, BufReader, Read};
use std::str::FromStr;

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

fn entry<I>(_line: I) -> CombParseResult<I, MagicEntry>
    where I: Stream<Item = char>
{
    unimplemented!();
}

#[allow(dead_code)]
fn unsigned_number<I>(line: I) -> CombParseResult<I, u64>
    where I: Stream<Item = char>
{
    let hex = (string("0x"), many1::<String, _>(hex_digit()))
        .map(|(_, hex_str)| {
            u64::from_str_radix(&hex_str, 16).unwrap()
        });
    let dec = many1::<String, _>(digit())
        .map(|dec_str| {
            u64::from_str(&dec_str).unwrap()
        });

    try(hex).or(dec).parse(line)
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

    #[test]
    fn numbers() {
        assert_eq!(Ok((3_551_379_183, "")), super::unsigned_number("0xd3adBEEF"));
        assert_eq!(Ok((314, "")), super::unsigned_number("314"));
        assert_eq!(Ok((0, "")), super::unsigned_number("0"));

        // Should this actually be octal?
        assert_eq!(Ok((314, "")), super::unsigned_number("0314"));
    }
}
