use combine::char::*;
use combine::combinator::*;
use combine::{ConsumedResult, ParseError, Parser, Stream, ParseResult};
use data_type::{DataDesc};
use magic::*;
use num::{self, Num};
use std::io::{self, ErrorKind};
use std::iter::FromIterator;
use std::marker::PhantomData;
use std::str::Chars;

/// Parses 0..limit tokens with parser.
pub fn at_most<F, P>(limit: u32, parser: P) -> AtMost<F, P>
    where F: FromIterator<P::Output>,
          P: Parser
{
    AtMost { limit: limit, parser: parser, marker: PhantomData, }
}

pub struct AtMost<F, P> where P: Parser {
    limit: u32,
    parser: P,
    marker: PhantomData<F>,
}

impl<F, P> Parser for AtMost<F, P>
    where F: FromIterator<P::Output>,
          P: Parser
{
    type Input = P::Input;
    type Output = F;

    fn parse_stream(&mut self, input: Self::Input) -> ParseResult<Self::Output, Self::Input> {
        let mut iter = self.parser.by_ref().iter(input);
        let value = iter.by_ref().take(self.limit as usize).collect();
        iter.into_result(value)
    }
}

/// Parses a numeric operator.
pub fn numeric_operator<I: Stream<Item = char>>() -> NumericOperator<I> {
    NumericOperator(one_of("=<>!".chars()), PhantomData)
}

pub struct NumericOperator<I>(OneOf<Chars<'static>, I>, PhantomData<fn(I) -> I>)
    where I: Stream<Item = char>;

impl<I: Stream<Item = char>> Parser for NumericOperator<I> {
    type Input = I;
    type Output = NumOp;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input).map(|c| {
            match c {
                '=' => NumOp::Equal,
                '<' => NumOp::LessThan,
                '>' => NumOp::GreaterThan,
                '!' => NumOp::NotEqual,
                _ => unreachable!(),
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

/// Parses a data type descriptor.
pub fn data_type<I: Stream<Item = char>>() -> DataType<I> {
    DataType {
        parser: many1::<String, _>(alpha_num()).and_then(translate_data_type_value),
        marker: PhantomData,
    }
}

pub struct DataType<I: Stream<Item = char>> {
    parser: AndThen<Many1<String, AlphaNum<I>>, fn(String) -> io::Result<DataDesc>>,
    marker: PhantomData<fn(I) -> I>
}

impl<I: Stream<Item = char>> Parser for DataType<I> {
    type Input = I;
    type Output = DataDesc;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input)
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

fn translate_data_type_value(val: String) -> io::Result<DataDesc> {
    use data_type::DataDesc::*;
    use endian::Endian::*;

    match val.as_ref() {
        "byte"  => Ok(Byte  { signed: true }),

        "short"   => Ok(Short { endian: Native, signed: true }),
        "beshort" => Ok(Short { endian: Big,    signed: true }),
        "leshort" => Ok(Short { endian: Little, signed: true }),

        "long"   => Ok(Long { endian: Native, signed: true }),
        "belong" => Ok(Long { endian: Big,    signed: true }),
        "lelong" => Ok(Long { endian: Little, signed: true }),
        "melong" => Ok(Long { endian: Pdp11,  signed: true }),

        "quad"   => Ok(Quad { endian: Native, signed: true }),
        "bequad" => Ok(Quad { endian: Big,    signed: true }),
        "lequad" => Ok(Quad { endian: Little, signed: true }),

        "ubyte"  => Ok(Byte  { signed: false }),

        "ushort"   => Ok(Short { endian: Native, signed: false }),
        "ubeshort" => Ok(Short { endian: Big,    signed: false }),
        "uleshort" => Ok(Short { endian: Little, signed: false }),

        "ulong"   => Ok(Long { endian: Native, signed: false }),
        "ubelong" => Ok(Long { endian: Big,    signed: false }),
        "ulelong" => Ok(Long { endian: Little, signed: false }),
        "umelong" => Ok(Long { endian: Pdp11,  signed: false }),

        "uquad"   => Ok(Quad { endian: Native, signed: false }),
        "ubequad" => Ok(Quad { endian: Big,    signed: false }),
        "ulequad" => Ok(Quad { endian: Little, signed: false }),

        "float"   => Ok(Float(Native)),
        "befloat" => Ok(Float(Big)),
        "lefloat" => Ok(Float(Little)),

        "double"   => Ok(Double(Native)),
        "bedouble" => Ok(Double(Big)),
        "ledouble" => Ok(Double(Little)),

        "string" => Ok(String),

        _ => Err(io::Error::new(ErrorKind::Other, format!("Unknown data type: {:?}", val))),
    }
}

/// Parses a possibly-negative integer in either decimal, octal (with
/// a leading 0), or hexidecimal (with a leading 0x).
pub fn integer<N, I>() -> Integer<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    Integer {
        parser: (
            optional(token('-')),
            token('0')
                .with(token('x').with(hex_integer())
                      .or(oct_integer()))
                .or(dec_integer())
        ),
        marker: PhantomData,
    }
}

pub struct Integer<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    parser: (Optional<Token<I>>, Or<With<Token<I>, Or<With<Token<I>, HexInteger<N, I>>, OctInteger<N, I>>>, DecInteger<N, I>>),
    marker: PhantomData<fn(I) -> N>,
}

impl<N, I> Parser for Integer<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    type Input = I;
    type Output = N;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.by_ref().and_then::<_, _, io::Error>(|(opt_neg, num)| {
            if let Some(..) = opt_neg {
                let minus_one = try! {
                    N::from_str_radix("-1", 10)
                        .map_err(|_| io::Error::new(ErrorKind::Other, "Could not negate number"))
                };
                Ok(num * minus_one)
            } else {
                Ok(num)
            }
        }).parse_lazy(input)
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

/// Parses a non-negative hexidecimal integer.
pub fn hex_integer<N, I>() -> HexInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    HexInteger {
        parser: many1::<String, _>(hex_digit()),
        marker: PhantomData,
    }
}

pub struct HexInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    parser: Many1<String, HexDigit<I>>,
    marker: PhantomData<fn(I) -> N>,
}

impl<N, I> Parser for HexInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    type Input = I;
    type Output = N;

    fn parse_stream(&mut self, input: Self::Input) -> ParseResult<Self::Output, Self::Input> {
        use combine::primitives::{Consumed, Error};

        let position = input.position();
        let (num_str, rest) = self.parser.parse_stream(input)?;

        match parse_str_radix::<N>(&num_str, 16) {
            Ok(n) => Ok((n, rest)),
            Err(s) => {
                let err = ParseError::new(position, Error::Message(From::from(s)));
                Err(Consumed::Consumed(err))
            }
        }
    }
}

/// Parses a non-negative decimal integer.
pub fn dec_integer<N, I>() -> DecInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    DecInteger {
        parser: many1::<String, _>(digit()),
        marker: PhantomData,
    }
}

pub struct DecInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    parser: Many1<String, Digit<I>>,
    marker: PhantomData<fn(I) -> N>,
}

impl<N, I> Parser for DecInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    type Input = I;
    type Output = N;

    fn parse_stream(&mut self, input: Self::Input) -> ParseResult<Self::Output, Self::Input> {
        use combine::primitives::{Consumed, Error};

        let position = input.position();
        let (num_str, rest) = self.parser.parse_stream(input)?;

        match parse_str_radix::<N>(&num_str, 10) {
            Ok(n) => Ok((n, rest)),
            Err(s) => {
                let err = ParseError::new(position, Error::Message(From::from(s)));
                Err(Consumed::Consumed(err))
            }
        }
    }
}

/// Parses a non-negative octal integer.
pub fn oct_integer<N, I>() -> OctInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    OctInteger {
        parser: many1::<String, _>(oct_digit()),
        marker: PhantomData,
    }
}

pub struct OctInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    parser: Many1<String, OctDigit<I>>,
    marker: PhantomData<fn(I) -> N>,
}

impl<N, I> Parser for OctInteger<N, I>
    where N: Num + num::Integer,
          I: Stream<Item = char>
{
    type Input = I;
    type Output = N;

    fn parse_stream(&mut self, input: Self::Input) -> ParseResult<Self::Output, Self::Input> {
        use combine::primitives::{Consumed, Error};

        let position = input.position();
        let (num_str, rest) = self.parser.parse_stream(input)?;

        match parse_str_radix::<N>(&num_str, 8) {
            Ok(n) => Ok((n, rest)),
            Err(s) => {
                let err = ParseError::new(position, Error::Message(From::from(s)));
                Err(Consumed::Consumed(err))
            }
        }
    }
}

fn parse_str_radix<N>(num_str: &str, radix: u32) -> Result<N, String>
    where N: Num + num::Integer
{
    use std::mem;

    match N::from_str_radix(num_str, radix) {
        Ok(n) => Ok(n),
        Err(..) => Err(format!(
            "Unable to parse {:?} as a {}-bit base-{} integer",
            num_str, mem::size_of::<N>() * 8, radix)),
    }
}

pub fn escape_sequence<I>() -> EscapeSequence<I>
    where I: Stream<Item = char>
{

    EscapeSequence {
        named_char: one_of("\\nrt".chars()),
        hex_escape: token('x').with(hex_integer::<u8, _>()),
        oct_escape: oct_integer::<u8, _>(),
    }
}

pub struct EscapeSequence<I>
    where I: Stream<Item = char>,
{
    named_char: OneOf<Chars<'static>, I>,
    hex_escape: With<Token<I>, HexInteger<u8, I>>,
    oct_escape: OctInteger<u8, I>,
}

impl<I> Parser for EscapeSequence<I>
    where I: Stream<Item = char>
{
    type Input = I;
    type Output = char;

    fn parse_stream(&mut self, input: I) -> ParseResult<Self::Output, Self::Input> {
        let named_parser = self.named_char.by_ref().map(|s| {
            match s {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                _ => unreachable!(),
            }
        });
        let hex_parser = self.hex_escape.by_ref().map(|n| From::from(n));
        let oct_parser = self.oct_escape.by_ref().map(|n| From::from(n));
        token('\\').with(named_parser.or(token('x').with(hex_parser)).or(oct_parser)).parse_stream(input)
    }
}

#[cfg(test)]
mod tests {
    use combine::Parser;
    use combine::char::alpha_num;

    #[test]
    fn integers() {
        assert_eq!(Ok((893, "")), super::dec_integer::<u16, _>().parse("893"));
        assert_eq!(Ok((3_551_379_183, "")), super::hex_integer::<u32, _>().parse("d3adBEEF"));
        assert_eq!(Ok((78, "")), super::oct_integer::<i8, _>().parse("116"));

        assert_eq!(Ok((20, "")), super::integer::<i32, _>().parse("20"));
        assert_eq!(Ok((20, "")), super::integer::<i32, _>().parse("0x14"));
        assert_eq!(Ok((20, "")), super::integer::<i32, _>().parse("024"));

        assert_eq!(Ok((-20, "")), super::integer::<i32, _>().parse("-20"));
        assert_eq!(Ok((-20, "")), super::integer::<i32, _>().parse("-0x14"));
        assert_eq!(Ok((-20, "")), super::integer::<i32, _>().parse("-024"));

        assert!(super::hex_integer::<u16, _>().parse("1FFFF").is_err());
        assert!(super::integer::<u32, _>().parse("-1025").is_err());
    }

    #[test]
    fn numerical_operators() {
        use magic::NumOp::*;

        assert_eq!(Ok((Equal, "")),       super::numeric_operator().parse("="));
        assert_eq!(Ok((GreaterThan, "")), super::numeric_operator().parse(">"));
        assert_eq!(Ok((LessThan, "")),    super::numeric_operator().parse("<"));
        assert_eq!(Ok((NotEqual, "")),    super::numeric_operator().parse("!"));
    }

    #[test]
    fn data_type() {
        use data_type::DataDesc::*;
        use endian::Endian::*;

        assert_eq!(Ok((Byte { signed: true  }, "")), super::data_type().parse("byte"));

        assert_eq!(Ok((Short { endian: Native, signed: true }, "")), super::data_type().parse("short"));
        assert_eq!(Ok((Short { endian: Big,    signed: true }, "")), super::data_type().parse("beshort"));
        assert_eq!(Ok((Short { endian: Little, signed: true }, "")), super::data_type().parse("leshort"));

        assert_eq!(Ok((Long { endian: Native, signed: true }, "")), super::data_type().parse("long"));
        assert_eq!(Ok((Long { endian: Big,    signed: true }, "")), super::data_type().parse("belong"));
        assert_eq!(Ok((Long { endian: Little, signed: true }, "")), super::data_type().parse("lelong"));
        assert_eq!(Ok((Long { endian: Pdp11,  signed: true }, "")), super::data_type().parse("melong"));

        assert_eq!(Ok((Quad { endian: Native, signed: true }, "")), super::data_type().parse("quad"));
        assert_eq!(Ok((Quad { endian: Big,    signed: true }, "")), super::data_type().parse("bequad"));
        assert_eq!(Ok((Quad { endian: Little, signed: true }, "")), super::data_type().parse("lequad"));

        assert_eq!(Ok((Byte { signed: false }, "")), super::data_type().parse("ubyte"));

        assert_eq!(Ok((Short { endian: Native, signed: false }, "")), super::data_type().parse("ushort"));
        assert_eq!(Ok((Short { endian: Big,    signed: false }, "")), super::data_type().parse("ubeshort"));
        assert_eq!(Ok((Short { endian: Little, signed: false }, "")), super::data_type().parse("uleshort"));

        assert_eq!(Ok((Long { endian: Native, signed: false }, "")), super::data_type().parse("ulong"));
        assert_eq!(Ok((Long { endian: Big,    signed: false }, "")), super::data_type().parse("ubelong"));
        assert_eq!(Ok((Long { endian: Little, signed: false }, "")), super::data_type().parse("ulelong"));
        assert_eq!(Ok((Long { endian: Pdp11,  signed: false }, "")), super::data_type().parse("umelong"));

        assert_eq!(Ok((Quad { endian: Native, signed: false }, "")), super::data_type().parse("uquad"));
        assert_eq!(Ok((Quad { endian: Big,    signed: false }, "")), super::data_type().parse("ubequad"));
        assert_eq!(Ok((Quad { endian: Little, signed: false }, "")), super::data_type().parse("ulequad"));

        assert_eq!(Ok((Float(Native), "")), super::data_type().parse("float"));
        assert_eq!(Ok((Float(Big),    "")), super::data_type().parse("befloat"));
        assert_eq!(Ok((Float(Little), "")), super::data_type().parse("lefloat"));

        assert_eq!(Ok((Double(Native), "")), super::data_type().parse("double"));
        assert_eq!(Ok((Double(Big),    "")), super::data_type().parse("bedouble"));
        assert_eq!(Ok((Double(Little), "")), super::data_type().parse("ledouble"));
    }

    #[test]
    fn escaped_chars() {
        assert_eq!(Ok(('\n', "")), super::escape_sequence().parse("\\n"));
        assert_eq!(Ok(('\r', "")), super::escape_sequence().parse("\\r"));
        assert_eq!(Ok(('\t', "")), super::escape_sequence().parse("\\t"));
        assert_eq!(Ok(('\\', "")), super::escape_sequence().parse("\\\\"));
        assert_eq!(Ok(('\0', "")), super::escape_sequence().parse("\\0"));
        assert_eq!(Ok(('\x0E', "")), super::escape_sequence().parse("\\016"));
    }

    #[test]
    fn at_most_parser() {
        assert_eq!(Ok(("abc".to_string(), "d")), super::at_most::<String, _>(3, alpha_num()).parse("abcd"));
        assert_eq!(Ok(("ab".to_string(), "&d")), super::at_most::<String, _>(3, alpha_num()).parse("ab&d"));
    }
}
