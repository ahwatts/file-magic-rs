#![allow(dead_code)]

use entry::*;
use nom::*;
use std::io::{BufRead, BufReader, Cursor, Read, Write};
use std::str::{self, FromStr};

pub fn parse<R: Read>(input: &mut R) -> Result<Vec<MagicEntry>, String> {
    let mut entries = Vec::new();
    let buf_input = BufReader::new(input);

    for (line_num_minus_one, line_rslt) in buf_input.lines().enumerate() {
        let line_num = line_num_minus_one + 1;
        let line = try!(line_rslt.map_err(|e| format!("I/O Error: {}", e)));

        match parse_line(line.as_bytes()) {
            IResult::Done(rest, Some(entry)) => {
                if rest.len() > 0 {
                    return Err(format!("Unparsed content on line {}: {}", line_num, String::from_utf8_lossy(rest)));
                }
                entries.push(entry);
            },
            IResult::Done(_, None) => {},
            IResult::Incomplete(needed) => {
                return Err(format!("Incomplete input at line {}: {:?}", line_num, needed));
            },
            err @ IResult::Error(..) => {
                let mut buf_message = Cursor::new(Vec::new());
                writeln!(&mut buf_message, "Parse error on line {}: {:?}", line_num, err).unwrap();
                if let Some(v) = prepare_errors(line.as_bytes(), err) {
                    println!("v = {:?}", v);
                    writeln!(&mut buf_message, "{}", print_offsets(line.as_bytes(), 0, &v)).unwrap();
                } else {
                    writeln!(&mut buf_message, "Unknown error").unwrap();
                }
                return Err(String::from_utf8_lossy(&buf_message.into_inner()).to_string());
            }
        }
    }

    Ok(entries)
}

named! {
    parse_line(&[u8]) -> Option<MagicEntry>,
    alt_complete!(
        chain!(opt!(space) ~ eof, || None)              |
        chain!(opt!(space) ~ tag!("#") ~ rest, || None) |
        chain!(level: fold_many0!(tag!(">"), 0, |acc, _| acc + 1) ~
               mut entry: chain!(off: error!(ErrorKind::Custom(1), offset) ~
                                 space ~
                                 data_type: error!(ErrorKind::Custom(2), data_type) ~
                                 space ~
                                 _test: is_not!(" ") ~
                                 space ~
                                 message: not_line_ending,
                                 || {
                                     MagicEntry {
                                         level: 0,
                                         offset: off,
                                         data_type: data_type,
                                         test: Test,
                                         message: String::from_utf8_lossy(message).to_string(),
                                     }
                                 }),
               || {
                   entry.level = level;
                   Some(entry)
               })
    )
}

named!(
    query(&[u8]) -> Option<MagicEntry>,
    chain!(level: fold_many0!(tag!(">"), 0, |acc, _| acc + 1) ~
           mut entry: call!(line),
           || {
               entry.level = level;
               Some(entry)
           })
);

named!(line(&[u8]) -> MagicEntry,
       chain!(off: error!(ErrorKind::Custom(1), call!(offset)) ~
              space ~
              data_type: error!(ErrorKind::Custom(2), call!(data_type)) ~
              space ~
              _test: is_not!(" ") ~
              space ~
              message: not_line_ending,
              || {
                  MagicEntry {
                      level: 0,
                      offset: off,
                      data_type: data_type,
                      test: Test,
                      message: String::from_utf8_lossy(message).to_string(),
                  }
              })
);

named!(offset(&[u8]) -> Offset, alt!(
    direct_offset => { |off| Offset::direct(off) }
    // indirect_offset => { |off| Offset::absolute_indirect(off) } |
    // chain!(tag!("&") ~ off: indirect_offset, || { Offset::relative_indirect(off) })
));

named!(direct_offset(&[u8]) -> DirectOffset, alt!(
    unsigned_number => { |n| DirectOffset::absolute(n) }
    // chain!(tag!("&") ~ rel: signed_number, || { DirectOffset::relative(rel) })
));

// named!(indirect_offset(&[u8]) -> IndirectOffset,
//        chain!(tag!("(") ~
//               off: direct_offset ~
//               size_and_format: opt!(chain!(tag!(".") ~ size: indirect_offset_size_format, || size)) ~
//               tag!(")"),
//               || {
//                   let (length, format) = size_and_format.unwrap_or((4, IndirectOffsetFormat::LittleEndian));
//                   IndirectOffset {
//                       base: off,
//                       length: length,
//                       format: format,
//                   }
//               }
// ));

// named!(indirect_offset_size_format(&[u8]) -> (usize, IndirectOffsetFormat), alt!(
//     tag!("B") => { |_| (1, IndirectOffsetFormat::Byte) }            |
//     tag!("C") => { |_| (1, IndirectOffsetFormat::Byte) }            |
//     tag!("b") => { |_| (1, IndirectOffsetFormat::Byte) }            |
//     tag!("c") => { |_| (1, IndirectOffsetFormat::Byte) }            |
//     tag!("S") => { |_| (2, IndirectOffsetFormat::BigEndian) }       |
//     tag!("H") => { |_| (2, IndirectOffsetFormat::BigEndian) }       |
//     tag!("s") => { |_| (2, IndirectOffsetFormat::LittleEndian) }    |
//     tag!("h") => { |_| (2, IndirectOffsetFormat::LittleEndian) }    |
//     tag!("L") => { |_| (4, IndirectOffsetFormat::BigEndian) }       |
//     tag!("l") => { |_| (4, IndirectOffsetFormat::LittleEndian) }    |
//     tag!("I") => { |_| (4, IndirectOffsetFormat::BigEndianId3) }    |
//     tag!("i") => { |_| (4, IndirectOffsetFormat::LittleEndianId3) } |
//     tag!("m") => { |_| (4, IndirectOffsetFormat::Pdp11Endian) }
// ));

named!(data_type(&[u8]) -> DataType, alt_complete!(unsigned_data_type | signed_data_type));

named!(unsigned_data_type(&[u8]) -> DataType, chain!(tag!("u") ~ dt: signed_data_type, || dt.toggle_signed()));

named!(signed_data_type(&[u8]) -> DataType, alt!(
    tag!("byte")     => { |_| DataType::Byte { signed: true } } |

    tag!("short")    => { |_| DataType::Short { endian: Endian::Native, signed: true } } |
    tag!("beshort")  => { |_| DataType::Short { endian: Endian::Big,    signed: true } } |
    tag!("leshort")  => { |_| DataType::Short { endian: Endian::Little, signed: true } } |

    tag!("long")    => { |_| DataType::Long { endian: Endian::Native, signed: true } } |
    tag!("belong")  => { |_| DataType::Long { endian: Endian::Big,    signed: true } } |
    tag!("lelong")  => { |_| DataType::Long { endian: Endian::Little, signed: true } } |
    tag!("melong")  => { |_| DataType::Long { endian: Endian::Pdp11,  signed: true } } |

    tag!("quad")    => { |_| DataType::Quad { endian: Endian::Native, signed: true } } |
    tag!("bequad")  => { |_| DataType::Quad { endian: Endian::Big,    signed: true } } |
    tag!("lequad")  => { |_| DataType::Quad { endian: Endian::Little, signed: true } } |

    tag!("float")    => { |_| DataType::Float(Endian::Native) }  |
    tag!("befloat")  => { |_| DataType::Float(Endian::Big) }     |
    tag!("lefloat")  => { |_| DataType::Float(Endian::Little) }  |

    tag!("double")   => { |_| DataType::Double(Endian::Native) } |
    tag!("bedouble") => { |_| DataType::Double(Endian::Big) }    |
    tag!("ledouble") => { |_| DataType::Double(Endian::Little) }


    // tag!("leid3") => { |_| DataType::Id3(Endian::Little) } |
    // tag!("beid3") => { |_| DataType::Id3(Endian::Big) }    |


    // tag!("date")     => { |_| DataType::LongDate(Endian::Native, TimeZone::Utc) }   |
    // tag!("bedate")   => { |_| DataType::LongDate(Endian::Big, TimeZone::Utc) }      |
    // tag!("ledate")   => { |_| DataType::LongDate(Endian::Little, TimeZone::Utc) }   |
    // tag!("medate")   => { |_| DataType::LongDate(Endian::Pdp11, TimeZone::Utc) }    |

    // tag!("ldate")    => { |_| DataType::LongDate(Endian::Native, TimeZone::Local) } |
    // tag!("beldate")  => { |_| DataType::LongDate(Endian::Big, TimeZone::Local) }    |
    // tag!("leldate")  => { |_| DataType::LongDate(Endian::Little, TimeZone::Local) } |
    // tag!("meldate")  => { |_| DataType::LongDate(Endian::Pdp11, TimeZone::Local) }  |

    // tag!("qdate")    => { |_| DataType::QuadDate(Endian::Native, TimeZone::Utc) }   |
    // tag!("beqdate")  => { |_| DataType::QuadDate(Endian::Big, TimeZone::Utc) }      |
    // tag!("leqdate")  => { |_| DataType::QuadDate(Endian::Little, TimeZone::Utc) }   |

    // tag!("qldate")   => { |_| DataType::QuadDate(Endian::Native, TimeZone::Local) } |
    // tag!("beqldate") => { |_| DataType::QuadDate(Endian::Big, TimeZone::Local) }    |
    // tag!("leqldate") => { |_| DataType::QuadDate(Endian::Little, TimeZone::Local) } |

    // tag!("qwdate")   => { |_| DataType::WindowsDate(Endian::Native) }               |
    // tag!("beqwdate") => { |_| DataType::WindowsDate(Endian::Big) }                  |
    // tag!("leqwdate") => { |_| DataType::WindowsDate(Endian::Little) }               |

    // tag!("name")     => { |_| DataType::Name }
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

// named!(
//     signed_number(&[u8]) -> i64,
//     chain!(neg: opt!(tag!("-")) ~
//            num: unsigned_number,
//            || {
//                match neg {
//                    Some(..) => -1 * (num as i64),
//                    None => num as i64
//                }
//            })
// );

#[cfg(test)]
mod tests {
    use entry::*;
    use nom::IResult;

    // #[test]
    // fn ignores_blank_lines() {
    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("\n".as_bytes()));

    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("    \n".as_bytes()));

    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("\t\n".as_bytes()));

    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("  \t  \n".as_bytes()));
    // }

    // #[test]
    // fn ignores_comments() {
    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("#\n".as_bytes()));
    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("# comment\n".as_bytes()));
    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("   # comment\n".as_bytes()));
    //     assert_eq!(
    //         IResult::Done(&b""[..], Vec::new()),
    //         parse("  \t #\t\n".as_bytes()));
    // }

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
        // assert_eq!(IResult::Done(&b""[..], DirectOffset::relative(108)),  super::direct_offset("&108".as_bytes()));
        // assert_eq!(IResult::Done(&b""[..], DirectOffset::relative(108)),  super::direct_offset("&0x6C".as_bytes()));
        // assert_eq!(IResult::Done(&b""[..], DirectOffset::relative(-108)), super::direct_offset("&-108".as_bytes()));
    }

    // #[test]
    // fn indirect_offset() {
    //     assert_eq!(
    //         IResult::Done(
    //             &b""[..],
    //             IndirectOffset {
    //                 base: DirectOffset::absolute(60),
    //                 length: 4,
    //                 format: IndirectOffsetFormat::LittleEndian,
    //             }),
    //         super::indirect_offset("(0x3c)".as_bytes()));

    //     assert_eq!(
    //         IResult::Done(
    //             &b""[..],
    //             IndirectOffset {
    //                 base: DirectOffset::relative(124),
    //                 length: 4,
    //                 format: IndirectOffsetFormat::LittleEndian,
    //             }),
    //         super::indirect_offset("(&0x7c)".as_bytes()));

    //     assert_eq!(
    //         IResult::Done(
    //             &b""[..],
    //             IndirectOffset {
    //                 base: DirectOffset::relative(-124),
    //                 length: 4,
    //                 format: IndirectOffsetFormat::LittleEndian,
    //             }),
    //         super::indirect_offset("(&-0x7c)".as_bytes()));
    // }

    #[test]
    fn numbers() {
        assert_eq!(IResult::Done(&b""[..], 3_551_379_183), super::unsigned_number("0xd3adBEEF".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], 314), super::unsigned_number("314".as_bytes()));
        assert_eq!(IResult::Done(&b""[..], 0), super::unsigned_number("0".as_bytes()));

        // Should this actually be octal?
        assert_eq!(IResult::Done(&b""[..], 314), super::unsigned_number("0314".as_bytes()));

        // assert_eq!(IResult::Done(&b""[..], 314), super::signed_number("314".as_bytes()));
        // assert_eq!(IResult::Done(&b""[..], -314), super::signed_number("-314".as_bytes()));
        // assert_eq!(IResult::Done(&b""[..], -124), super::signed_number("-0x7c".as_bytes()));
    }

    // #[test]
    // fn indirect_offset_size_format() {
    //     use entry::DirectOffset::*;
    //     use entry::IndirectOffsetFormat::*;

    //     macro_rules! assert_size_format {
    //         ($test_str:expr => ($base: expr, $len:expr, $format:expr)) => {
    //             assert_eq!(
    //                 IResult::Done(&b""[..], IndirectOffset {
    //                     base: $base,
    //                     length: $len,
    //                     format: $format,
    //                 }),
    //                 super::indirect_offset($test_str.as_bytes()));
    //         }
    //     }

    //     assert_size_format!("(60.B)" => (Absolute(60), 1, Byte));
    //     assert_size_format!("(60.b)" => (Absolute(60), 1, Byte));
    //     assert_size_format!("(60.C)" => (Absolute(60), 1, Byte));
    //     assert_size_format!("(60.c)" => (Absolute(60), 1, Byte));

    //     assert_size_format!("(60.S)" => (Absolute(60), 2, BigEndian));
    //     assert_size_format!("(60.H)" => (Absolute(60), 2, BigEndian));
    //     assert_size_format!("(60.s)" => (Absolute(60), 2, LittleEndian));
    //     assert_size_format!("(60.h)" => (Absolute(60), 2, LittleEndian));

    //     assert_size_format!("(60.L)" => (Absolute(60), 4, BigEndian));
    //     assert_size_format!("(60.l)" => (Absolute(60), 4, LittleEndian));

    //     assert_size_format!("(60.I)" => (Absolute(60), 4, BigEndianId3));
    //     assert_size_format!("(60.i)" => (Absolute(60), 4, LittleEndianId3));

    //     assert_size_format!("(60.m)" => (Absolute(60), 4, Pdp11Endian));
    // }

    #[test]
    fn data_type() {
        use entry::Endian::*;
        // use entry::TimeZone::*;

        macro_rules! assert_data_type {
            ($test_str:expr => $data_type:ident) => {
                assert_eq!(
                    IResult::Done(&b""[..], DataType::$data_type),
                    super::data_type($test_str.as_bytes()));
            };

            ($test_str:expr => $data_type:ident { $($name:ident : $value:expr),+ }) => {
                assert_eq!(
                    IResult::Done(&b""[..], DataType::$data_type { $($name : $value),* }),
                    super::data_type($test_str.as_bytes()));
            };

            ($test_str:expr => $data_type:ident($($arg:expr),*)) => {
                assert_eq!(
                    IResult::Done(&b""[..], DataType::$data_type($($arg),*)),
                    super::data_type($test_str.as_bytes()));
            };
        }

        assert_data_type!("byte"     => Byte  {                 signed: true });
        assert_data_type!("short"    => Short { endian: Native, signed: true });
        assert_data_type!("beshort"  => Short { endian: Big,    signed: true });
        assert_data_type!("leshort"  => Short { endian: Little, signed: true });
        assert_data_type!("long"     => Long  { endian: Native, signed: true });
        assert_data_type!("belong"   => Long  { endian: Big,    signed: true });
        assert_data_type!("lelong"   => Long  { endian: Little, signed: true });
        assert_data_type!("melong"   => Long  { endian: Pdp11,  signed: true });
        assert_data_type!("quad"     => Quad  { endian: Native, signed: true });
        assert_data_type!("bequad"   => Quad  { endian: Big,    signed: true });
        assert_data_type!("lequad"   => Quad  { endian: Little, signed: true });

        assert_data_type!("ubyte"     => Byte  {                 signed: false });
        assert_data_type!("ushort"    => Short { endian: Native, signed: false });
        assert_data_type!("ubeshort"  => Short { endian: Big,    signed: false });
        assert_data_type!("uleshort"  => Short { endian: Little, signed: false });
        assert_data_type!("ulong"     => Long  { endian: Native, signed: false });
        assert_data_type!("ubelong"   => Long  { endian: Big,    signed: false });
        assert_data_type!("ulelong"   => Long  { endian: Little, signed: false });
        assert_data_type!("umelong"   => Long  { endian: Pdp11,  signed: false });
        assert_data_type!("uquad"     => Quad  { endian: Native, signed: false });
        assert_data_type!("ubequad"   => Quad  { endian: Big,    signed: false });
        assert_data_type!("ulequad"   => Quad  { endian: Little, signed: false });

        assert_data_type!("float"    => Float(Native));
        assert_data_type!("befloat"  => Float(Big));
        assert_data_type!("lefloat"  => Float(Little));
        assert_data_type!("double"   => Double(Native));
        assert_data_type!("bedouble" => Double(Big));
        assert_data_type!("ledouble" => Double(Little));

        // assert_data_type!("beid3" => Id3(Big));
        // assert_data_type!("leid3" => Id3(Little));

        // assert_data_type!("date" => LongDate(Native, Utc));
        // assert_data_type!("bedate" => LongDate(Big, Utc));
        // assert_data_type!("ledate" => LongDate(Little, Utc));
        // assert_data_type!("medate" => LongDate(Pdp11, Utc));
        // assert_data_type!("ldate" => LongDate(Native, Local));
        // assert_data_type!("beldate" => LongDate(Big, Local));
        // assert_data_type!("leldate" => LongDate(Little, Local));
        // assert_data_type!("meldate" => LongDate(Pdp11, Local));
        // assert_data_type!("qdate" => QuadDate(Native, Utc));
        // assert_data_type!("beqdate" => QuadDate(Big, Utc));
        // assert_data_type!("leqdate" => QuadDate(Little, Utc));
        // assert_data_type!("qldate" => QuadDate(Native, Local));
        // assert_data_type!("beqldate" => QuadDate(Big, Local));
        // assert_data_type!("leqldate" => QuadDate(Little, Local));
        // assert_data_type!("qwdate" => WindowsDate(Native));
        // assert_data_type!("beqwdate" => WindowsDate(Big));
        // assert_data_type!("leqwdate" => WindowsDate(Little));
    }
}
