use combine::combinator::*;
use combine::{ConsumedResult, ParseError, Parser, Stream, ParseResult};
use magic;
use std::marker::PhantomData;
use super::*;

pub fn direct_offset<I>() -> DirectOffset<I>
    where I: Stream<Item = char>
{
    DirectOffset(optional(token('&')).and(integer()))
}

pub struct DirectOffset<I>((Optional<Token<I>>, Integer<i64, I>))
    where I: Stream<Item = char>;

impl<I> Parser for DirectOffset<I>
    where I: Stream<Item = char>
{
    type Input = I;
    type Output = magic::DirectOffset;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input).map(|(opt_amp, off)| {
            match opt_amp {
                Some(..) => magic::DirectOffset::Relative(off),
                None => magic::DirectOffset::Absolute(off as u64),
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

pub fn indirect_offset<I>() -> IndirectOffset<I>
    where I: Stream<Item = char>
{
    IndirectOffset(PhantomData)
}

pub struct IndirectOffset<I>(PhantomData<fn(I) -> I>)
    where I: Stream<Item = char>;

impl<I> Parser for IndirectOffset<I>
    where I: Stream<Item = char>
{
    type Input = I;
    type Output = magic::IndirectOffset;

    fn parse_stream(&mut self, input: I) -> ParseResult<Self::Output, Self::Input> {
        use data_type::DataType::*;
        use endian::Endian::*;

        let data_type_parser = (one_of(",.".chars()), one_of("bislBISLm".chars()));
        let bias_parser = (one_of("+-".chars()), integer::<i64, _>());
        let inner_parser = direct_offset().and(optional(data_type_parser).and(optional(bias_parser)));
        between(token('('), token(')'), inner_parser).parse_stream(input)
            .map(|((direct_offset, (opt_data_type, opt_bias)), rest)| {
                let data_type = match opt_data_type {
                    None => Long { signed: false, endian: Native },
                    Some((',', 'b')) | Some((',', 'B')) => Byte { signed: true  },
                    Some(('.', 'b')) | Some(('.', 'B')) => Byte { signed: false },
                    // Some((',', 'i')) | Some(('.', 'i')) => Id3 { endian: Little },
                    // Some((',', 'I')) | Some(('.', 'I')) => Id3 { endian: Big    },
                    Some((',', 's')) => Short { signed: true,  endian: Little },
                    Some((',', 'S')) => Short { signed: true,  endian: Big    },
                    Some(('.', 's')) => Short { signed: false, endian: Little },
                    Some(('.', 'S')) => Short { signed: false, endian: Big    },
                    Some((',', 'l')) => Long { signed: true,  endian: Little },
                    Some((',', 'L')) => Long { signed: true,  endian: Big    },
                    Some(('.', 'l')) => Long { signed: false, endian: Little },
                    Some(('.', 'L')) => Long { signed: false, endian: Big    },
                    Some((',', 'm')) => Long { signed: true,  endian: Pdp11 },
                    Some(('.', 'm')) => Long { signed: false, endian: Pdp11 },

                    // Should probably be a parse error...
                    Some((s, t)) => unreachable!("Invalid data type for indirect offset: x{}{}", s, t),
                };

                let bias = match opt_bias {
                    None => 0,
                    Some(('-', n)) => -1 * n,
                    Some(('+', n)) => n,

                    // Should probably be a parse error...
                    Some((s, n)) => unreachable!("Invalid bias for indirect offset: {}{}", s, n),
                };

                (
                    magic::IndirectOffset {
                        base: direct_offset,
                        data_type: data_type,
                        bias: bias,
                    },
                    rest,
                )
            })
    }
}

pub fn offset<I>() -> Offset<I>
    where I: Stream<Item = char>
{
    Offset(PhantomData)
}

pub struct Offset<I>(PhantomData<fn(I) -> I>)
    where I: Stream<Item = char>;

impl<I> Parser for Offset<I>
    where I: Stream<Item = char>
{
    type Input = I;
    type Output = magic::Offset;

    fn parse_stream(&mut self, input: I) -> ParseResult<Self::Output, Self::Input> {
        if let Ok((direct, rest)) = direct_offset().parse_stream(input.clone()) {
            Ok((magic::Offset::Direct(direct), rest))
        } else {
            optional(token('&')).and(indirect_offset()).parse_stream(input)
                .map(|((opt_amp, indirect), rest)| {
                    match opt_amp {
                        Some(..) => (magic::Offset::RelativeIndirect(indirect), rest),
                        None => (magic::Offset::AbsoluteIndirect(indirect), rest),
                    }
                })
        }
    }
}

#[cfg(test)]
mod tests {
    use combine::Parser;

    #[test]
    fn direct_offset() {
        use magic::DirectOffset::*;
        assert_eq!(Ok((Absolute(108), "")), super::direct_offset().parse("0x6c"));
        assert_eq!(Ok((Absolute(108), "")), super::direct_offset().parse("108"));
        assert_eq!(Ok((Relative(108), "")), super::direct_offset().parse("&0x6c"));
        assert_eq!(Ok((Relative(108), "")), super::direct_offset().parse("&108"));
        assert_eq!(Ok((Relative(-108), "")), super::direct_offset().parse("&-0x6c"));
        assert_eq!(Ok((Relative(-108), "")), super::direct_offset().parse("&-108"));
    }

    #[test]
    fn indirect_offset() {
        use magic;
        use magic::DirectOffset::*;
        use data_type::DataType::*;
        use endian::Endian::*;

        assert_eq!(
            Ok((magic::IndirectOffset {
                base: Absolute(108),
                data_type: Long { signed: false, endian: Native },
                bias: 0,
            }, "")),
            super::indirect_offset().parse("(108)")
        );

        assert_eq!(
            Ok((magic::IndirectOffset {
                base: Absolute(4),
                data_type: Long { signed: false, endian: Little },
                bias: 4,
            }, "")),
            super::indirect_offset().parse("(4.l+4)")
        );
    }

    #[test]
    fn offset() {
        use magic;
        use magic::Offset::*;
        use magic::DirectOffset::*;
        use data_type::DataType::*;
        use endian::Endian::*;

        assert_eq!(Ok((Direct(Absolute(108)), "")), super::offset().parse("108"));
        assert_eq!(Ok((Direct(Relative(108)), "")), super::offset().parse("&0x6C"));

        assert_eq!(
            Ok((AbsoluteIndirect(magic::IndirectOffset {
                base: Absolute(108),
                data_type: Short { signed: false, endian: Big },
                bias: 0,
            }), "")),
            super::offset().parse("(108.S)")
        );

        assert_eq!(
            Ok((AbsoluteIndirect(magic::IndirectOffset {
                base: Relative(108),
                data_type: Long { signed: true, endian: Little },
                bias: -5,
            }), "")),
            super::offset().parse("(&108,l-5)")
        );

        assert_eq!(
            Ok((RelativeIndirect(magic::IndirectOffset {
                base: Absolute(108),
                data_type: Byte { signed: false },
                bias: 8,
            }), "")),
            super::offset().parse("&(0x6c.B+8)")
        );

        assert_eq!(
            Ok((RelativeIndirect(magic::IndirectOffset {
                base: Relative(108),
                data_type: Long { signed: false, endian: Native },
                bias: 0,
            }), "")),
            super::offset().parse("&(&108)")
        );
    }
}
