use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, hex_digit1, oct_digit1},
    combinator::map_res,
    sequence::preceded,
    IResult,
};
use num::{BigUint, Integer, Num};
use std::convert::TryFrom;

pub fn unsigned_integer<N: Integer + TryFrom<BigUint>>(input: &str) -> IResult<&str, N> {
    map_res(big_integer, N::try_from)(input)
}

fn big_integer(input: &str) -> IResult<&str, BigUint> {
    alt((
        map_res(preceded(tag("0x"), hex_digit1), |value| {
            BigUint::from_str_radix(value, 16)
        }),
        map_res(preceded(tag("0"), oct_digit1), |value| {
            BigUint::from_str_radix(value, 8)
        }),
        map_res(digit1, |value| BigUint::from_str_radix(value, 10)),
    ))(input)
}

/*
pub trait ParseableInt: Num + FromBigInt + Clone + Debug {}
impl<N> ParseableInt for N where N: Num + FromBigInt + Clone + Debug {}

pub fn integer_bytes<I>(data_type: &data_type::DataType) -> IntegerBytes<'_, I>
    where I: Stream<Item = char>
{
    IntegerBytes {
        data_type,
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
        use crate::data_type::DataType::*;
        match self.data_type {
            Byte  { signed: false } => integer::<u8, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            Byte  { signed: true  } => integer::<i8, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            Short { endian: _, signed: false } => integer::<u16, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            Short { endian: _, signed: true  } => integer::<i16, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            Long  { endian: _, signed: false } => integer::<u32, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            Long  { endian: _, signed: true  } => integer::<i32, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            Quad  { endian: _, signed: false } => integer::<u64, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            Quad  { endian: _, signed: true  } => integer::<i64, _>().map(|num| { data_type::sized_to_byte_vec(num) }).parse_stream(input),
            _ => unreachable!("Cannot parse integer value for data type {:?}", self.data_type),
        }
    }
}
*/

#[cfg(test)]
mod tests {
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

            assert_eq!(
                Ok(("", zero)),
                unsigned_integer::<$int_type>(zero_str.as_str())
            );
            assert_eq!(
                Ok(("", half_m1)),
                unsigned_integer::<$int_type>(half_m1_str.as_str())
            );
            assert_eq!(
                Ok(("", half)),
                unsigned_integer::<$int_type>(half_str.as_str())
            );
            assert_eq!(
                Ok(("", max)),
                unsigned_integer::<$int_type>(max_str.as_str())
            );
        };
    }

    // macro_rules! test_int_boundaries {
    //     ($bits:expr, $int_type:ident, $pos_format_str:expr, $neg_format_str: expr) => {
    //         let zero: $int_type = 0;
    //         let max_pos: $int_type = $int_type::max_value();
    //         let max_neg: $int_type = $int_type::min_value();
    //         let minus_1: $int_type = zero - 1;

    //         let zero_str = format!($pos_format_str, zero);
    //         let neg_zero_str = format!($neg_format_str, zero);
    //         let max_pos_str = format!($pos_format_str, max_pos);
    //         let max_neg_str = format!($neg_format_str, max_neg);
    //         let minus_1_str = format!($neg_format_str, minus_1);

    //         // println!("zero = {:?} ({})", zero_str, zero);
    //         // println!("max+ = {:?} ({})", max_pos_str, max_pos);
    //         // println!("max- = {:?} ({})", max_neg_str, max_neg);
    //         // println!("-1   = {:?} ({})", minus_1_str, minus_1);

    //         assert_eq!(
    //             Ok(("", zero)),
    //             signed_integer::<$int_type>(zero_str.as_str())
    //         );
    //         assert_eq!(
    //             Ok(("", zero)),
    //             signed_integer::<$int_type>(neg_zero_str.as_str())
    //         );
    //         assert_eq!(
    //             Ok(("", max_pos)),
    //             signed_integer::<$int_type>(max_pos_str.as_str())
    //         );
    //         assert_eq!(
    //             Ok(("", max_neg)),
    //             signed_integer::<$int_type>(max_neg_str.as_str())
    //         );
    //         assert_eq!(
    //             Ok(("", minus_1)),
    //             signed_integer::<$int_type>(minus_1_str.as_str())
    //         );
    //     };
    // }

    macro_rules! test_int {
        ($mod_name:ident, $bits:expr, $uint_type:ident, $int_type:ident) => {
            mod $mod_name {
                use super::super::*;

                #[test]
                fn parse_unsigned_from_octal() {
                    test_uint_boundaries!($bits, $uint_type, "0{:o}");
                }

                // #[test]
                // fn parse_signed_from_octal() {
                //     test_int_boundaries!($bits, $int_type, "0{:o}", "-0{:o}");
                // }

                #[test]
                fn parse_unsigned_from_decimal() {
                    test_uint_boundaries!($bits, $uint_type, "{}");
                }

                // #[test]
                // fn parse_signed_from_decimal() {
                //     test_int_boundaries!($bits, $int_type, "{}", "-{}");
                // }

                #[test]
                fn parse_unsigned_from_hex() {
                    test_uint_boundaries!($bits, $uint_type, "0x{:x}");
                }

                // #[test]
                // fn parse_signed_from_hex() {
                //     test_int_boundaries!($bits, $int_type, "0x{:x}", "-0x{:x}");
                // }
            }
        };
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
