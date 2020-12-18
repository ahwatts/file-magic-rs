use crate::{
    data_type::DataType,
    magic::{DirectOffset, IndirectOffset, Offset},
};
use anyhow::anyhow;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::one_of,
    combinator::{map, map_res, opt, value},
    sequence::{pair, tuple},
    IResult,
};

use super::number::unsigned_integer;

pub fn offset(input: &str) -> IResult<&str, Offset> {
    alt((
        map(
            tuple((opt(tag("&")), indirect_offset)),
            |(opt_amp, off_ind)| match opt_amp {
                Some(..) => Offset::RelativeIndirect(off_ind),
                None => Offset::AbsoluteIndirect(off_ind),
            },
        ),
        map(direct_offset, Offset::Direct),
    ))(input)
}

pub fn direct_offset(input: &str) -> IResult<&str, DirectOffset> {
    map(
        pair(opt(tag("&")), unsigned_integer),
        |(opt_amp, num)| match opt_amp {
            Some(..) => DirectOffset::relative(num as i64),
            None => DirectOffset::absolute(num),
        },
    )(input)
}

pub fn indirect_offset(input: &str) -> IResult<&str, IndirectOffset> {
    map(
        tuple((
            value((), tag("(")),
            direct_offset,
            opt(indirect_offset_data_type),
            opt(indirect_offset_bias),
            value((), tag(")")),
        )),
        |(_, doff, opt_dt, opt_bias, _)| {
            use crate::data_type::DataType::*;
            use crate::endian::Endian::*;

            IndirectOffset {
                base: doff,
                data_type: opt_dt.unwrap_or(Long {
                    signed: false,
                    endian: Native,
                }),
                bias: opt_bias.unwrap_or(0),
            }
        },
    )(input)
}

#[rustfmt::skip]
fn indirect_offset_data_type(input: &str) -> IResult<&str, DataType> {
    map_res(
        tuple((one_of(",."), one_of("bBcCeEfFgGhHiIlmsSqQ"))),
        |(signed_char, type_char)| {
            use crate::data_type::DataType::*;
            use crate::endian::Endian::*;

            match (signed_char, type_char) {
                (',', 'b') | (',', 'B') => Ok(Byte { signed: true }),
                ('.', 'b') | ('.', 'B') => Ok(Byte { signed: false }),
                // (',', 'i') | ('.', 'i') => Id3 { endian: Little },
                // (',', 'I') | ('.', 'I') => Id3 { endian: Big },
                (',', 's') => Ok(Short { signed: true, endian: Little }),
                (',', 'S') => Ok(Short { signed: true, endian: Big }),
                ('.', 's') => Ok(Short { signed: false, endian: Little }),
                ('.', 'S') => Ok(Short { signed: false, endian: Big }),
                (',', 'l') => Ok(Long { signed: true, endian: Little }),
                (',', 'L') => Ok(Long { signed: true, endian: Big }),
                ('.', 'l') => Ok(Long { signed: false, endian: Little }),
                ('.', 'L') => Ok(Long { signed: false, endian: Big }),

                _ => Err(anyhow!("Unable to handle offset data type: {:?}", type_char)),
            }
        },
    )(input)
}

fn indirect_offset_bias(input: &str) -> IResult<&str, i64> {
    map_res(
        tuple((one_of("+-"), unsigned_integer::<i64>)),
        |(signed_char, int_val)| match signed_char {
            '+' => Ok(int_val),
            '-' => Ok(-int_val),
            _ => Err(anyhow!("Unknown bias sign: {:?}", signed_char)),
        },
    )(input)
}

#[cfg(test)]
mod tests {
    #[test]
    fn direct_offset() {
        use crate::magic::DirectOffset::*;
        assert_eq!(Ok(("", Absolute(108))), super::direct_offset("0x6c"));
        assert_eq!(Ok(("", Absolute(108))), super::direct_offset("108"));
        assert_eq!(Ok(("", Relative(108))), super::direct_offset("&0x6c"));
        assert_eq!(Ok(("", Relative(108))), super::direct_offset("&108"));
        // assert_eq!(Ok(("", Relative(-108))), super::direct_offset("&-0x6c"));
        // assert_eq!(Ok(("", Relative(-108))), super::direct_offset("&-108"));
    }

    #[test]
    fn indirect_offset() {
        use crate::data_type::DataType::*;
        use crate::endian::Endian::*;
        use crate::magic;
        use crate::magic::DirectOffset::*;

        assert_eq!(
            Ok((
                "",
                magic::IndirectOffset {
                    base: Absolute(108),
                    data_type: Long {
                        signed: false,
                        endian: Native
                    },
                    bias: 0,
                }
            )),
            super::indirect_offset("(108)")
        );

        assert_eq!(
            Ok((
                "",
                magic::IndirectOffset {
                    base: Absolute(4),
                    data_type: Long {
                        signed: false,
                        endian: Little
                    },
                    bias: 4,
                }
            )),
            super::indirect_offset("(4.l+4)")
        );
    }

    #[test]
    fn offset() {
        use crate::data_type::DataType::*;
        use crate::endian::Endian::*;
        use crate::magic;
        use crate::magic::DirectOffset::*;
        use crate::magic::Offset::*;

        assert_eq!(Ok(("", Direct(Absolute(108)))), super::offset("108"));
        assert_eq!(Ok(("", Direct(Relative(108)))), super::offset("&0x6C"));

        assert_eq!(
            Ok((
                "",
                AbsoluteIndirect(magic::IndirectOffset {
                    base: Absolute(108),
                    data_type: Short {
                        signed: false,
                        endian: Big
                    },
                    bias: 0,
                }),
            )),
            super::offset("(108.S)")
        );

        assert_eq!(
            Ok((
                "",
                AbsoluteIndirect(magic::IndirectOffset {
                    base: Relative(108),
                    data_type: Long {
                        signed: true,
                        endian: Little
                    },
                    bias: -5,
                }),
            )),
            super::offset("(&108,l-5)")
        );

        assert_eq!(
            Ok((
                "",
                RelativeIndirect(magic::IndirectOffset {
                    base: Absolute(108),
                    data_type: Byte { signed: false },
                    bias: 8,
                }),
            )),
            super::offset("&(0x6c.B+8)")
        );

        assert_eq!(
            Ok((
                "",
                RelativeIndirect(magic::IndirectOffset {
                    base: Relative(108),
                    data_type: Long {
                        signed: false,
                        endian: Native
                    },
                    bias: 0,
                }),
            )),
            super::offset("&(&108)")
        );
    }
}
