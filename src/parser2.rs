use combine::*;
use combine::char::*;
use entry::MagicEntry;
use std::io::{BufRead, BufReader, Read};

pub fn parse<R: Read>(input: &mut R) -> Result<Vec<MagicEntry>, String> {
    let entries = Vec::new();
    let buf_input = BufReader::new(input);

    for (line_num_minus_one, line_rslt) in buf_input.lines().enumerate() {
        let _line_num = line_num_minus_one + 1;
        let _line = try!(line_rslt.map_err(|e| format!("I/O Error: {}", e)));
    }

    Ok(entries)
}

type CombParseResult<I, O> = Result<(O, I), ParseError<I>>;

#[allow(dead_code)]
fn parse_line<I>(line: I) -> CombParseResult<I, Option<MagicEntry>>
    where I: Stream<Item = char> + Clone
{
    let blank = (spaces(), eof()).map(|_| None);
    let comment = (spaces(), token('#'), many::<Vec<char>, _>(try(any()))).map(|_| None);
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

#[allow(dead_code)]
fn entry<I>(_line: I) -> CombParseResult<I, MagicEntry>
    where I: Stream<Item = char>
{
    unimplemented!();
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
