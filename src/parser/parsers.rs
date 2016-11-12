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

// pub struct Escaped<T, I>
//     where T: Clone + IntoIterator<Item = char>,
//           I: Stream<Item = char>
// {
//     parser: With<Token<I>, Or<Or<With<Token<I>, AtMost<String, HexDigit<I>>>, AtMost<String, OctDigit<I>>>, OneOf<T, I>>>,
//     marker: PhantomData<fn(I) -> I>,
// }

// impl<T, I> Parser for Escaped<T, I>
//     where T: Clone + IntoIterator<Item = char>,
//           I: Stream<Item = char>
// {
//     type Input = I;
//     type Output = char;

//     #[inline]
//     fn parse_lazy(&mut self, input: I) -> ConsumedResult<Self::Output, Self::Input> {
//         self.parser.parse_lazy(input)
//     }

//     fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
//         self.parser.add_error(errors)
//     }
// }

// #[inline(always)]
// pub fn escaped<T, I>(escaper: char, escapees: T) -> Escaped<T, I>
//     where T: Clone + IntoIterator<Item = char>,
//           I: Stream<Item = char>
// {
//     let oct_num = at_most::<String, _>(3, oct_digit());
//     let hex_num = token('x').with(at_most::<String, _>(2, hex_digit()));
//     let esc_char = one_of(escapees);

//     Escaped {
//         parser: token(escaper).with(hex_num.or(oct_num).or(esc_char)),
//         marker: PhantomData,
//     }
// }

// #[derive(Clone)]
// pub struct EscapedNum<I: Stream<Item = char>> {
//     parser: With<Token<I>, Or<(With<Token<I>, AtMost<String, HexDigit<I>>>, Value<I, u32>), (AtMost<String, OctDigit<I>>, Value<I, u32>)>>,
//     marker: PhantomData<fn(I) -> I>
// }

// impl<I: Stream<Item = char>> Parser for EscapedNum<I> {
//     type Input = I;
//     type Output = char;

//     #[inline]
//     fn parse_lazy(&mut self, input: I) -> ConsumedResult<Self::Output, Self::Input> {
//         self.parser.parse_lazy(input).map(|(string, radix)| {
//             println!("string = {:?} radix = {:?}", string, radix);
//             From::from(u8::from_str_radix(&string, radix).unwrap())
//         })
//     }

//     fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
//         self.parser.add_error(errors)
//     }
// }

// #[inline(always)]
// pub fn escaped_num<I: Stream<Item = char>>(escaper: char) -> EscapedNum<I> {
//     let oct_num = (at_most::<String, _>(3, oct_digit()), value(8));
//     let hex_num = (token('x').with(at_most::<String, _>(2, hex_digit())), value(16));

//     EscapedNum {
//         parser: token(escaper).with(hex_num.or(oct_num)),
//         marker: PhantomData,
//     }
// }

// pub struct EscapeSequence<I, P8, P16>
//     where I: Stream<Item = char>
// {
//     parser: (),
//     marker: PhantomData<fn(I) -> I>,
// }

// fn parse_hex_char<I: Stream<Item = char>>(input: I) -> ParseResult<char, I> {
//     use combine::StreamOnce;
//     use combine::primitives::{Error, Consumed};

//     let position = input.position();
//     let (hex_str, input) = try!(at_most::<String, _>(2, hex_digit()).parse_stream(input));
//     if let Ok(hex_char) = u8::from_str_radix(&hex_str, 16) {
//         Ok((From::from(hex_char), input))
//     } else {
//         let errors = ParseError::new(position, Error::Expected(From::from("something")));
//         Err(Consumed::Empty(errors))
//     }
// }

// fn parse_oct_char<I: Stream<Item = char>>(input: I) -> ParseResult<char, I> {
//     use combine::StreamOnce;
//     use combine::primitives::{Error, Consumed};

//     let position = input.position();
//     let (oct_str, input) = try!(at_most::<String, _>(3, oct_digit()).parse_stream(input));
//     if let Ok(oct_char) = u8::from_str_radix(&oct_str, 8) {
//         Ok((From::from(oct_char), input))
//     } else {
//         let errors = ParseError::new(position, Error::Expected(From::from("something2")));
//         Err(Consumed::Empty(errors))
//     }
// }

// pub fn escape_sequence<I, P8, P16>() -> EscapeSequence<I, P8, P16>
//     where I: Stream<Item = char>
// {
//     let named_char = one_of("\\nrt".chars());
//     let hex_escape = token('x').with(parser(parse_hex_char));
//     let oct_escape = parser(parse_oct_char);

//     EscapeSequence {
//         parser: token('\\').with(named_char.or(hex_escape).or(oct_escape)),
//         marker: PhantomData,
//     }
// }

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

    // #[test]
    // fn escaped_chars() {
    //     let mut parser = super::escaped('\\', "nrt\\'\"".chars());
    //     assert_eq!(Ok(('\n', "")), parser.parse("\\n"));
    //     assert_eq!(Ok(('\r', "")), parser.parse("\\r"));
    //     assert_eq!(Ok(('\t', "")), parser.parse("\\t"));
    //     assert_eq!(Ok(('\'', "")), parser.parse("\\'"));
    //     assert_eq!(Ok(('"', "")), parser.parse("\\\""));
    //     assert_eq!(Ok(('\0', "")), parser.parse("\\0"));
    //     assert_eq!(Ok(('\x0E', "")), parser.parse("\\016"));
    // }

    // #[test]
    // fn escaped_numbers() {
    //     assert_eq!(Ok(('\u{D2}', "")), super::escaped_num('\\').parse("\\322"));
    //     assert_eq!(Ok(('\u{D2}', "322")), super::escaped_num('\\').parse("\\322322"));
    //     assert_eq!(Ok(('\n', "")), super::escaped_num('\\').parse("\\x0a"));
    //     assert_eq!(Ok(('\n', "abc")), super::escaped_num('\\').parse("\\x0aabc"));
    // }

    #[test]
    fn at_most_parser() {
        assert_eq!(Ok(("abc".to_string(), "d")), super::at_most::<String, _>(3, alpha_num()).parse("abcd"));
        assert_eq!(Ok(("ab".to_string(), "&d")), super::at_most::<String, _>(3, alpha_num()).parse("ab&d"));
    }
}
