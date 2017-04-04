use combine::char::*;
use combine::combinator::*;
use combine::{ConsumedResult, ParseError, Parser, Stream, ParseResult};
use data_type;
use magic;
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
    type Output = magic::NumOp;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input).map(|c| {
            match c {
                '=' => magic::NumOp::Equal,
                '<' => magic::NumOp::LessThan,
                '>' => magic::NumOp::GreaterThan,
                '!' => magic::NumOp::NotEqual,
                _ => unreachable!("Invalid numeric operator: {:?}", c),
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

/// Parses a string operator.
pub fn string_operator<I: Stream<Item = char>>() -> StringOperator<I> {
    StringOperator(one_of("=<>".chars()), PhantomData)
}

pub struct StringOperator<I>(OneOf<Chars<'static>, I>, PhantomData<fn(I) -> I>)
    where I: Stream<Item = char>;

impl<I: Stream<Item = char>> Parser for StringOperator<I> {
    type Input = I;
    type Output = magic::StringOp;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input).map(|c| {
            match c {
                '=' => magic::StringOp::Equal,
                '<' => magic::StringOp::LexBefore,
                '>' => magic::StringOp::LexAfter,
                _ => unreachable!("Invalid string operator: {:?}", c),
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
    parser: AndThen<Many1<String, AlphaNum<I>>, fn(String) -> io::Result<data_type::DataType>>,
    marker: PhantomData<fn(I) -> I>
}

impl<I: Stream<Item = char>> Parser for DataType<I> {
    type Input = I;
    type Output = data_type::DataType;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input)
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

fn translate_data_type_value(val: String) -> io::Result<data_type::DataType> {
    use data_type::DataType::*;
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

        "name" => Ok(Name("".to_string())),
        "use"  => Ok(Use("".to_string())),

        _ => Err(io::Error::new(ErrorKind::Other, format!("Unknown data type: {:?}", val))),
    }
}

pub fn integer_bytes<'a, I>(data_type: &'a data_type::DataType) -> IntegerBytes<'a, I>
    where I: Stream<Item = char>
{
    IntegerBytes {
        data_type: data_type,
        marker: PhantomData,
    }
}

pub struct IntegerBytes<'a, I>
    where I: Stream<Item = char>
{
    data_type: &'a data_type::DataType,
    marker: PhantomData<fn(I) -> I>,
}

impl<'a, I> Parser for IntegerBytes<'a, I>
    where I: Stream<Item = char>
{
    type Input = I;
    type Output = Vec<u8>;

    fn parse_stream(&mut self, input: Self::Input) -> ParseResult<Self::Output, Self::Input> {
        use data_type::DataType::*;
        match self.data_type {
            &Byte  { signed: false } => integer::<u8, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            &Byte  { signed: true  } => integer::<i8, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            &Short { endian: _, signed: false } => integer::<u16, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            &Short { endian: _, signed: true  } => integer::<i16, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            &Long  { endian: _, signed: false } => integer::<u32, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            &Long  { endian: _, signed: true  } => integer::<i32, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            &Quad  { endian: _, signed: false } => integer::<u64, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            &Quad  { endian: _, signed: true  } => integer::<i64, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            _ => unreachable!("Cannot parse integer value for data type {:?}", self.data_type),
        }
    }
}


/// Parses a possibly-negative integer in either decimal, octal (with
/// a leading 0), or hexidecimal (with a leading 0x).
pub fn integer<N, I>() -> Integer<N, I>
    where N: Num + num::Integer + Clone,
          I: Stream<Item = char>
{
    Integer {
        parser: (
            optional(token('-')),
            token('0')
                .with(token('x').with(hex_integer())
                      .or(oct_integer())
                      .or(value(N::zero())))
                .or(dec_integer())
        ),
        marker: PhantomData,
    }
}

pub struct Integer<N, I>
    where N: Num + num::Integer + Clone,
          I: Stream<Item = char>
{
    parser: (Optional<Token<I>>, Or<With<Token<I>, Or<Or<With<Token<I>, HexInteger<N, I>>, OctInteger<N, I>>, Value<I, N>>>, DecInteger<N, I>>),
    marker: PhantomData<fn(I) -> N>,
}

impl<N, I> Parser for Integer<N, I>
    where N: Num + num::Integer + Clone,
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
                _ => unreachable!("Invalid escape sequence: {:?}", s),
            }
        });
        let hex_parser = self.hex_escape.by_ref().map(|n| From::from(n));
        let oct_parser = self.oct_escape.by_ref().map(|n| From::from(n));
        token('\\').with(named_parser.or(hex_parser).or(oct_parser)).parse_stream(input)
    }
}

pub fn escaped_string<F, I>() -> EscapedString<F, I>
    where I: Stream<Item = char>,
          F: FromIterator<char>,
{
    EscapedString(many::<F, _>(or(
        escape_sequence(),
        none_of(" ".chars())
    )))
}

pub struct EscapedString<F, I>(Many<F, Or<EscapeSequence<I>, NoneOf<Chars<'static>, I>>>)
    where I: Stream<Item = char>,
          F: FromIterator<char>;

impl<F, I> Parser for EscapedString<F, I>
    where I: Stream<Item = char>,
          F: FromIterator<char>
{
    type Input = I;
    type Output = F;

    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input)
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

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
    use combine::char::alpha_num;
    use data_type;
    use endian::Endian;

    #[test]
    fn escaped_strings() {
        assert_eq!(Ok((String::from("fmt "), "")), super::escaped_string::<String, _>().parse("fmt\\x20"));
    }

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

        assert_eq!(Ok((0, "")), super::integer::<i32, _>().parse("0"));
    }

    #[test]
    fn integer_bytes() {
        let dt = data_type::DataType::Long { endian: Endian::Native, signed: true };
        let bytes = vec![ 20, 0, 0, 0 ];
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("20"));
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0x14"));
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("024"));

        let bytes = vec![ 236, 255, 255, 255 ];
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("-20"));
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("-0x14"));
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("-024"));

        let dt = data_type::DataType::Quad { endian: Endian::Native, signed: false };
        let bytes = vec![ 0x00, 0x40, 0x10, 0x00, 0x00, 0x02, 0x00, 0x10 ];
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0x1000020000104000"));
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0100000040000004040000"));
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("1152923703631167488"));

        let bytes = vec![ 0, 0, 0, 0, 0, 0, 0, 0 ];
        assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0"));
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
    fn string_operators() {
        use magic::StringOp::*;

        assert_eq!(Ok((Equal,     "")), super::string_operator().parse("="));
        assert_eq!(Ok((LexBefore, "")), super::string_operator().parse("<"));
        assert_eq!(Ok((LexAfter,  "")), super::string_operator().parse(">"));
    }

    #[test]
    fn data_type() {
        use data_type::DataType::*;
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
        assert_eq!(Ok((' ', "")), super::escape_sequence().parse("\\x20"));
    }

    #[test]
    fn at_most_parser() {
        assert_eq!(Ok(("abc".to_string(), "d")), super::at_most::<String, _>(3, alpha_num()).parse("abcd"));
        assert_eq!(Ok(("ab".to_string(), "&d")), super::at_most::<String, _>(3, alpha_num()).parse("ab&d"));
    }

    #[test]
    fn direct_offset() {
        use magic::DirectOffset::*;
        assert_eq!(Ok((Absolute(108), "")), super::direct_offset().parse("0x6c"));
        assert_eq!(Ok((Absolute(108), "")), super::direct_offset().parse("108"));
        assert_eq!(Ok((Relative(108), "")), super::direct_offset().parse("&0x6c"));
        assert_eq!(Ok((Relative(108), "")), super::direct_offset().parse("&108"));
        assert_eq!(Ok((Relative(-108), "")), super::direct_offset().parse("&-0x6C"));
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
                base: Relative(-108),
                data_type: Byte { signed: true },
                bias: 0,
            }, "")),
            super::indirect_offset().parse("(&-0x6c,b)")
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
