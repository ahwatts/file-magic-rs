#[macro_use] extern crate nom;

use nom::{space, newline, not_line_ending};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MagicEntry {
    level: u32,
}

named!(pub parse(&[u8]) -> Vec<MagicEntry>,
       fold_many0!(
           alt!(blank_line | comment | query),
           Vec::new(),
           |mut acc: Vec<MagicEntry>, item: Option<MagicEntry>| {
               if let Some(entry) = item {
                   acc.push(entry);
               }
               acc
           }
       ));
       

named!(blank_line(&[u8]) -> Option<MagicEntry>, chain!(opt!(space) ~ newline, || None));
named!(comment(&[u8]) -> Option<MagicEntry>, chain!(opt!(space) ~ tag!("#") ~ opt!(not_line_ending) ~ newline, || None));

named!(
    query(&[u8]) -> Option<MagicEntry>,
    chain!(level: fold_many0!(tag!(">"), 0, |acc, _| acc + 1) ~
           line ~
           newline,
           || Some(MagicEntry {
               level: level,
           }))
);

named!(line(&[u8]) -> &[u8], tag!("abcd"));

#[cfg(test)]
mod tests {
    use nom::IResult;
    use super::*;

    #[test]
    fn ignores_blank_lines() {
        let text = r"

     
	
";
        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse(text.as_bytes()));
    }

    #[test]
    fn ignores_comments() {
        let text = r"
# this is a comment
# this is also a comment
	# This is not at the beginning of the line
#
";
        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse(text.as_bytes()));
    }

    #[test]
    fn reads_entries() {
        let text = r"
# comment

abcd
";
        assert_eq!(IResult::Done(&b""[..], vec![
            MagicEntry { level: 0 },
        ]), parse(text.as_bytes()));
    }

    #[test]
    fn reads_levels() {
        let text = r"
>>>abcd
";
        assert_eq!(IResult::Done(&b""[..], vec![
            MagicEntry { level: 3 },
        ]), parse(text.as_bytes()));
    }
}
