#[macro_use] extern crate nom;

pub mod entry;
mod hand;

use entry::*;
use nom::*;
use std::str::FromStr;

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
    direct_offset => { |off| Offset::direct(off) } |
    chain!(tag!("(") ~ off: direct_offset ~ tag!(")"), || { Offset::absolute_indirect(off, None) }) |
    chain!(tag!("&(") ~ off: direct_offset ~ tag!(")"), || { Offset::relative_indirect(off, None) })
));

named!(direct_offset(&[u8]) -> DirectOffset, alt!(
    unsigned_number => { |n| DirectOffset::absolute(n) } |
    chain!(tag!("&") ~ rel: signed_number, || { DirectOffset::relative(rel) })
));

named!(
    unsigned_number(&[u8]) -> u64, alt_complete!(
        chain!(tag!("0x") ~ hex_bytes: hex_digit, || {
            let hex_str = std::str::from_utf8(hex_bytes).unwrap();
            u64::from_str_radix(hex_str, 16).unwrap()
        })
            |
        map!(digit, |num_bytes| {
            let num_str = std::str::from_utf8(num_bytes).unwrap();
            u64::from_str(num_str).unwrap()
        })
    )
);

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
    use entry::*;
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
        assert_eq!(IResult::Done(&b""[..], Offset::absolute(108)),  super::offset("0x6c".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative(108)),  super::offset("&108".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative(108)),  super::offset("&0x6C".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative(-108)), super::offset("&-108".as_bytes()));
    }

    #[test]
    fn indirect_offset() {
        assert_eq!(IResult::Done(&b""[..], Offset::absolute_indirect(DirectOffset::absolute(60), None)), super::offset("(0x3c)".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::absolute_indirect(DirectOffset::relative(124), None)), super::offset("(&0x7c)".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::absolute_indirect(DirectOffset::relative(-124), None)), super::offset("(&-0x7c)".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative_indirect(DirectOffset::absolute(60), None)), super::offset("&(60)".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative_indirect(DirectOffset::relative(124), None)), super::offset("&(&0x7c)".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], Offset::relative_indirect(DirectOffset::relative(-124), None)), super::offset("&(&-124)".as_bytes()));
    }

    #[test]
    fn numbers() {
        assert_eq!(IResult::Done(&b""[..], 3_551_379_183), super::unsigned_number("0xd3adBEEF".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], 314), super::unsigned_number("314".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], 0), super::unsigned_number("0".as_bytes()));

        // Should this actually be octal?
        assert_eq!(IResult::Done(&b""[..], 314), super::unsigned_number("0314".as_bytes()));

        assert_eq!(IResult::Done(&b""[..], 314), super::signed_number("314".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], -314), super::signed_number("-314".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], -124), super::signed_number("-0x7c".as_bytes()));
    }
}
