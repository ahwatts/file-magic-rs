use entry::*;
use nom::*;
use std::str::{self, FromStr};

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
       chain!(off: offset ~
              space ~
              data_type: data_type,
              || {
                  MagicEntry {
                      level: 0,
                      offset: off,
                      data_type: data_type,
                  }
              })
);

named!(offset(&[u8]) -> Offset, alt!(
    direct_offset => { |off| Offset::direct(off) } |
    indirect_offset => { |off| Offset::absolute_indirect(off) } |
    chain!(tag!("&") ~ off: indirect_offset, || { Offset::relative_indirect(off) })
));

named!(direct_offset(&[u8]) -> DirectOffset, alt!(
    unsigned_number => { |n| DirectOffset::absolute(n) } |
    chain!(tag!("&") ~ rel: signed_number, || { DirectOffset::relative(rel) })
));

named!(indirect_offset(&[u8]) -> IndirectOffset,
       chain!(tag!("(") ~
              off: direct_offset ~
              size_and_format: opt!(chain!(tag!(".") ~ size: indirect_offset_size_format, || size)) ~
              tag!(")"),
              || {
                  let (length, format) = size_and_format.unwrap_or((4, IndirectOffsetFormat::LittleEndian));
                  IndirectOffset {
                      base: off,
                      length: length,
                      format: format,
                  }
              }
));

named!(indirect_offset_size_format(&[u8]) -> (usize, IndirectOffsetFormat), alt!(
    tag!("B") => { |_| (1, IndirectOffsetFormat::Byte) }            |
    tag!("C") => { |_| (1, IndirectOffsetFormat::Byte) }            |
    tag!("b") => { |_| (1, IndirectOffsetFormat::Byte) }            |
    tag!("c") => { |_| (1, IndirectOffsetFormat::Byte) }            |
    tag!("S") => { |_| (2, IndirectOffsetFormat::BigEndian) }       |
    tag!("H") => { |_| (2, IndirectOffsetFormat::BigEndian) }       |
    tag!("s") => { |_| (2, IndirectOffsetFormat::LittleEndian) }    |
    tag!("h") => { |_| (2, IndirectOffsetFormat::LittleEndian) }    |
    tag!("L") => { |_| (4, IndirectOffsetFormat::BigEndian) }       |
    tag!("l") => { |_| (4, IndirectOffsetFormat::LittleEndian) }    |
    tag!("I") => { |_| (4, IndirectOffsetFormat::BigEndianId3) }    |
    tag!("i") => { |_| (4, IndirectOffsetFormat::LittleEndianId3) } |
    tag!("m") => { |_| (4, IndirectOffsetFormat::Pdp11Endian) }
));

named!(data_type(&[u8]) -> DataType, alt!(
    tag!("byte")     => { |_| DataType::Byte }                   |
    tag!("short")    => { |_| DataType::Short(Endian::Native) }  |
    tag!("beshort")  => { |_| DataType::Short(Endian::Big) }     |
    tag!("leshort")  => { |_| DataType::Short(Endian::Little) }  |
    tag!("long")     => { |_| DataType::Long(Endian::Native) }   |
    tag!("belong")   => { |_| DataType::Long(Endian::Big) }      |
    tag!("lelong")   => { |_| DataType::Long(Endian::Little) }   |
    tag!("melong")   => { |_| DataType::Long(Endian::Pdp11) }    |
    tag!("quad")     => { |_| DataType::Quad(Endian::Native) }   |
    tag!("bequad")   => { |_| DataType::Quad(Endian::Big) }      |
    tag!("lequad")   => { |_| DataType::Quad(Endian::Little) }   |
    tag!("float")    => { |_| DataType::Float(Endian::Native) }  |
    tag!("befloat")  => { |_| DataType::Float(Endian::Big) }     |
    tag!("lefloat")  => { |_| DataType::Float(Endian::Little) }  |
    tag!("double")   => { |_| DataType::Double(Endian::Native) } |
    tag!("bedouble") => { |_| DataType::Double(Endian::Big) }    |
    tag!("ledouble") => { |_| DataType::Double(Endian::Little) }
));

named!(
    unsigned_number(&[u8]) -> u64, alt_complete!(
        chain!(tag!("0x") ~ hex_bytes: hex_digit, || {
            let hex_str = str::from_utf8(hex_bytes).unwrap();
            u64::from_str_radix(hex_str, 16).unwrap()
        })
            |
        map!(digit, |num_bytes| {
            let num_str = str::from_utf8(num_bytes).unwrap();
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

    // #[test]
    // fn reads_entries() {
    //     assert_eq!(IResult::Done(&b""[..], vec![
    //         MagicEntry {
    //             level: 0,
    //             offset: Offset::absolute(0),
    //         },
    //     ]), parse("# comment\n\n0\n".as_bytes()));
    // }

    // #[test]
    // fn reads_levels() {
    //     assert_eq!(IResult::Done(&b""[..], vec![
    //         MagicEntry { level: 3, offset: Offset::absolute(0) },
    //     ]), parse(">>>0\n".as_bytes()));
    // }

    #[test]
    fn direct_offset() {
        assert_eq!(IResult::Done(&b""[..], DirectOffset::absolute(108)),  super::direct_offset("108".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], DirectOffset::absolute(108)),  super::direct_offset("0x6c".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], DirectOffset::relative(108)),  super::direct_offset("&108".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], DirectOffset::relative(108)),  super::direct_offset("&0x6C".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], DirectOffset::relative(-108)), super::direct_offset("&-108".as_bytes()));
    }

    #[test]
    fn indirect_offset() {
        assert_eq!(
            IResult::Done(
                &b""[..],
                IndirectOffset {
                    base: DirectOffset::absolute(60),
                    length: 4,
                    format: IndirectOffsetFormat::LittleEndian,
                }),
            super::indirect_offset("(0x3c)".as_bytes()));

        assert_eq!(
            IResult::Done(
                &b""[..],
                IndirectOffset {
                    base: DirectOffset::relative(124),
                    length: 4,
                    format: IndirectOffsetFormat::LittleEndian,
                }),
            super::indirect_offset("(&0x7c)".as_bytes()));

        assert_eq!(
            IResult::Done(
                &b""[..],
                IndirectOffset {
                    base: DirectOffset::relative(-124),
                    length: 4,
                    format: IndirectOffsetFormat::LittleEndian,
                }),
            super::indirect_offset("(&-0x7c)".as_bytes()));
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

    #[test]
    fn indirect_offset_size_format() {
        macro_rules! assert_size_format {
            ($test_str:expr => ($base: expr, $len:expr, $format:expr)) => {
                assert_eq!(
                    IResult::Done(&b""[..], IndirectOffset {
                        base: $base,
                        length: $len,
                        format: $format,
                    }),
                    super::indirect_offset($test_str.as_bytes()));
            }
        }

        assert_size_format!("(60.B)" => (DirectOffset::Absolute(60), 1, IndirectOffsetFormat::Byte));
        assert_size_format!("(60.b)" => (DirectOffset::Absolute(60), 1, IndirectOffsetFormat::Byte));
        assert_size_format!("(60.C)" => (DirectOffset::Absolute(60), 1, IndirectOffsetFormat::Byte));
        assert_size_format!("(60.c)" => (DirectOffset::Absolute(60), 1, IndirectOffsetFormat::Byte));

        assert_size_format!("(60.S)" => (DirectOffset::Absolute(60), 2, IndirectOffsetFormat::BigEndian));
        assert_size_format!("(60.H)" => (DirectOffset::Absolute(60), 2, IndirectOffsetFormat::BigEndian));
        assert_size_format!("(60.s)" => (DirectOffset::Absolute(60), 2, IndirectOffsetFormat::LittleEndian));
        assert_size_format!("(60.h)" => (DirectOffset::Absolute(60), 2, IndirectOffsetFormat::LittleEndian));

        assert_size_format!("(60.L)" => (DirectOffset::Absolute(60), 4, IndirectOffsetFormat::BigEndian));
        assert_size_format!("(60.l)" => (DirectOffset::Absolute(60), 4, IndirectOffsetFormat::LittleEndian));

        assert_size_format!("(60.I)" => (DirectOffset::Absolute(60), 4, IndirectOffsetFormat::BigEndianId3));
        assert_size_format!("(60.i)" => (DirectOffset::Absolute(60), 4, IndirectOffsetFormat::LittleEndianId3));

        assert_size_format!("(60.m)" => (DirectOffset::Absolute(60), 4, IndirectOffsetFormat::Pdp11Endian));
    }

    #[test]
    fn data_type() {
        macro_rules! assert_data_type {
            ($test_str:expr => $data_type:ident) => {
                assert_eq!(
                    IResult::Done(&b""[..], DataType::$data_type),
                    super::data_type($test_str.as_bytes()));
            };

            ($test_str:expr => $data_type:ident($($arg:expr),*)) => {
                assert_eq!(
                    IResult::Done(&b""[..], DataType::$data_type($($arg),*)),
                    super::data_type($test_str.as_bytes()));
            };
        }

        assert_data_type!("byte"     => Byte);
        assert_data_type!("short"    => Short(Endian::Native));
        assert_data_type!("beshort"  => Short(Endian::Big));
        assert_data_type!("leshort"  => Short(Endian::Little));
        assert_data_type!("long"     => Long(Endian::Native));
        assert_data_type!("belong"   => Long(Endian::Big));
        assert_data_type!("lelong"   => Long(Endian::Little));
        assert_data_type!("melong"   => Long(Endian::Pdp11));
        assert_data_type!("quad"     => Quad(Endian::Native));
        assert_data_type!("bequad"   => Quad(Endian::Big));
        assert_data_type!("lequad"   => Quad(Endian::Little));
        assert_data_type!("float"    => Float(Endian::Native));
        assert_data_type!("befloat"  => Float(Endian::Big));
        assert_data_type!("lefloat"  => Float(Endian::Little));
        assert_data_type!("double"   => Double(Endian::Native));
        assert_data_type!("bedouble" => Double(Endian::Big));
        assert_data_type!("ledouble" => Double(Endian::Little));
    }
}
