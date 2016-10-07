#[macro_use] extern crate nom;

use nom::{space, newline, not_line_ending};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MagicEntry;

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
named!(query(&[u8]) -> Option<MagicEntry>, chain!(tag!("abcd") ~ newline, || Some(MagicEntry)));

#[cfg(test)]
mod tests {
    use nom::IResult;
    use super::*;

    #[test]
    fn ignores_blank_lines() {
        let text = r"

     
	
";
        let result = parse(text.as_bytes());
        assert_eq!(IResult::Done(&b""[..], Vec::new()), result);
    }

    #[test]
    fn ignores_comments() {
        let text = r"
# this is a comment
# this is also a comment
	# This is not at the beginning of the line
#
";
        let result = parse(text.as_bytes());
        assert_eq!(IResult::Done(&b""[..], Vec::new()), result);
    }

    #[test]
    fn reads_entries() {
        let text = r"
# comment

abcd
";
        let result = parse(text.as_bytes());
        assert_eq!(IResult::Done(&b""[..], vec![ MagicEntry ]), result);
    }
}
