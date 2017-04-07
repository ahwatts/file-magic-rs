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
    // use data_type;
    // use endian::Endian;

    macro_rules! test_uint_boundaries {
        ($bits:expr, $int_type:ident, $format_str:expr) => {
            let bits_m1: $int_type = $bits - 1;

            let zero: $int_type = 0;
            let half_m1: $int_type = (1 << bits_m1) - 1;
            let half: $int_type = half_m1 + 1;
            let max: $int_type = $int_type::max_value();

            let zero_str = format!($format_str, zero);
            let half_m1_str = format!($format_str, half_m1);
            let half_str = format!($format_str, half);
            let max_str = format!($format_str, max);

            // println!("zero     = {:?}", zero_str);
            // println!("half - 1 = {:?}", half_m1_str);
            // println!("half     = {:?}", half_str);
            // println!("max      = {:?}", max_str);

            assert_eq!(Ok((zero, "")), integer::<$int_type, _>().parse(zero_str.as_str()));
            assert_eq!(Ok((half_m1, "")), integer::<$int_type, _>().parse(half_m1_str.as_str()));
            assert_eq!(Ok((half, "")), integer::<$int_type, _>().parse(half_str.as_str()));
            assert_eq!(Ok((max, "")), integer::<$int_type, _>().parse(max_str.as_str()));
        }
    }

    macro_rules! test_int_boundaries {
        ($bits:expr, $int_type:ident, $format_str:expr) => {
            let zero: $int_type = 0;
            let max_pos: $int_type = $int_type::max_value();
            let max_neg: $int_type = $int_type::min_value();
            let minus_1: $int_type = zero - 1;

            let zero_str = format!($format_str, zero);
            let max_pos_str = format!($format_str, max_pos);
            let max_neg_str = format!($format_str, max_neg);
            let minus_1_str = format!($format_str, minus_1);

            // println!("zero = {:?}", zero_str);
            // println!("max+ = {:?}", max_pos_str);
            // println!("max- = {:?}", max_neg_str);
            // println!("-1   = {:?}", minus_1_str);

            assert_eq!(Ok((zero, "")), integer::<$int_type, _>().parse(zero_str.as_str()));
            assert_eq!(Ok((max_pos, "")), integer::<$int_type, _>().parse(max_pos_str.as_str()));
            assert_eq!(Ok((max_neg, "")), integer::<$int_type, _>().parse(max_neg_str.as_str()));
            assert_eq!(Ok((minus_1, "")), integer::<$int_type, _>().parse(minus_1_str.as_str()));
        }
    }

    macro_rules! test_int {
        ($mod_name:ident, $bits:expr, $uint_type:ident, $int_type:ident) => {
            mod $mod_name {
                use super::super::*;

                #[test]
                fn parse_unsigned_from_octal() {
                    test_uint_boundaries!($bits, $uint_type, "0{:o}");
                }

                #[test]
                fn parse_signed_from_octal() {
                    test_int_boundaries!($bits, $int_type, "0{:o}");
                }

                #[test]
                fn parse_unsigned_from_decimal() {
                    test_uint_boundaries!($bits, $uint_type, "{}");
                }

                #[test]
                fn parse_signed_from_decimal() {
                    test_int_boundaries!($bits, $int_type, "{}");
                }

                #[test]
                fn parse_unsigned_from_hex() {
                    test_uint_boundaries!($bits, $uint_type, "0x{:x}");
                }

                #[test]
                fn parse_signed_from_hex() {
                    test_int_boundaries!($bits, $int_type, "0x{:x}");
                }
            }
        }
    }

    test_int!(byte_int, 8, u8, i8);
    test_int!(short_int, 16, u16, i16);
    test_int!(long_int, 32, u32, i32);
    test_int!(long_long_int, 64, u64, i64);

    // #[test]
    // fn integer_bytes() {
    //     let dt = data_type::DataType::Long { endian: Endian::Native, signed: true };
    //     let bytes = vec![ 20, 0, 0, 0 ];
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("20"));
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0x14"));
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("024"));

    //     let bytes = vec![ 236, 255, 255, 255 ];
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("-20"));
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("-0x14"));
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("-024"));

    //     let dt = data_type::DataType::Quad { endian: Endian::Native, signed: false };
    //     let bytes = vec![ 0x00, 0x40, 0x10, 0x00, 0x00, 0x02, 0x00, 0x10 ];
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0x1000020000104000"));
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0100000040000004040000"));
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("1152923703631167488"));

    //     let bytes = vec![ 0, 0, 0, 0, 0, 0, 0, 0 ];
    //     assert_eq!(Ok((bytes.clone(), "")), super::integer_bytes(&dt).parse("0"));
    // }
}
