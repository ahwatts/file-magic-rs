use combine::char::*;
use combine::combinator::*;
use combine::{ConsumedResult, ParseError, ParseResult, Parser, Stream};
use data_type;
use num::{self, Num};
use std::io::{self, ErrorKind};
use std::marker::PhantomData;

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

#[cfg(test)]
mod tests {
    use combine::Parser;
    use data_type;
    use endian::Endian;

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
}
