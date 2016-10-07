#[macro_use] extern crate nom;

use nom::*;
use std::str::FromStr;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MagicEntry {
    level: u32,
    offset: Offset,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BasicOffset {
    Absolute(u64),
    Relative(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IndirectOffset {
    base: BasicOffset,
    length: u32,
    // op: Operation,
    // disp: Displacement,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Offset {
    Direct(BasicOffset),
    AbsoluteIndirect(IndirectOffset),
    RelativeIndirect(IndirectOffset),
}

impl Offset {
    pub fn absolute(val: u64) -> Offset {
        Offset::Direct(BasicOffset::Absolute(val))
    }

    pub fn relative(val: i64) -> Offset {
        Offset::Direct(BasicOffset::Relative(val))
    }
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
           mut entry: line ~
           newline,
           || {
               entry.level = level;
               Some(entry)
           })
);

named!(line(&[u8]) -> MagicEntry,
       chain!(off: offset,
              || MagicEntry { level: 0, offset: off })
);

named!(offset(&[u8]) -> Offset, alt!(
    unsigned_number => { |n| Offset::absolute(n) } |
    chain!(tag!("&") ~ rel: signed_number, || { Offset::relative(rel) })
));

named!(
    unsigned_number(&[u8]) -> u64,
    map!(digit, |num_bytes| {
        let num_str = std::str::from_utf8(num_bytes).unwrap();
        u64::from_str(num_str).unwrap()
    }));

named!(
    signed_number(&[u8]) -> i64,
    chain!(neg: opt!(tag!("-")) ~
           num: unsigned_number,
           || {
               match neg {
                   Some(..) => -1 * (num as i64),
                   None => num as i64
               }
           })
);

#[cfg(test)]
mod tests {
    use nom::IResult;
    use super::*;

    #[test]
    fn ignores_blank_lines() {
        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("\n".as_bytes()));

        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("    \n".as_bytes()));

        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("\t\n".as_bytes()));

        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("  \t  \n".as_bytes()));
    }

    #[test]
    fn ignores_comments() {
        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("#\n".as_bytes()));
        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("# comment\n".as_bytes()));
        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("   # comment\n".as_bytes()));
        assert_eq!(
            IResult::Done(&b""[..], Vec::new()),
            parse("  \t #\t\n".as_bytes()));
    }

    #[test]
    fn reads_entries() {
        assert_eq!(IResult::Done(&b""[..], vec![
            MagicEntry { level: 0, offset: Offset::absolute(0) },
        ]), parse("# comment\n\n0\n".as_bytes()));
    }

    #[test]
    fn reads_levels() {
        assert_eq!(IResult::Done(&b""[..], vec![
            MagicEntry { level: 3, offset: Offset::absolute(0) },
        ]), parse(">>>0\n".as_bytes()));
    }

    #[test]
    fn direct_offset() {
        assert_eq!(IResult::Done(&b""[..], Offset::absolute(108)),  super::offset("108".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative(108)),  super::offset("&108".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative(-108)), super::offset("&-108".as_bytes()));
    }
}
