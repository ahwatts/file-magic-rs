use combine::char::*;
use combine::combinator::*;
use combine::{ConsumedResult, ParseError, Parser, Stream};
use magic::*;
use std::marker::PhantomData;
use std::str::Chars;
use super::DataType;

macro_rules! impl_parser {
    (
        $name:ident($($ty_var:ident),*), $base_ty:ty, $inner_type:ty, $out_type:ty,
        |$self_var:ident, $input_var:ident| $parse_lazy_body:block
    ) => {
        #[derive(Clone)]
        pub struct $name<I $(,$ty_var)*>($inner_type, PhantomData<fn(I) -> I>)
            where I: Stream<Item = $base_ty> $(, $ty_var: Parser<Input = I>)*;

        impl<I $(,$ty_var)*> Parser for $name<I $(,$ty_var)*>
            where I: Stream<Item = $base_ty> $(, $ty_var: Parser<Input = I>)*
        {
            type Input = I;
            type Output = $out_type;

            #[inline]
            fn parse_lazy(&mut self, eennput: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
                let $self_var = self;
                let $input_var = eennput;
                $parse_lazy_body
            }

            fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
                self.0.add_error(errors)
            }
        }
    }
}

impl_parser! {
    NumericOperator(), char, OneOf<Chars<'static>, I>, NumOp,
    |celf, input| {
        celf.0.parse_lazy(input).map(|c| {
            match c {
                '=' => NumOp::Equal,
                '<' => NumOp::LessThan,
                '>' => NumOp::GreaterThan,
                '!' => NumOp::NotEqual,
                _ => unreachable!(),
            }
        })
    }
}

#[inline(always)]
pub fn numeric_operator<I: Stream<Item = char>>() -> NumericOperator<I> {
    NumericOperator(one_of("=<>!".chars()), PhantomData)
}

#[derive(Clone)]
pub struct Integer<I: Stream<Item = char>> {
    parser: (Optional<Token<I>>, Or<Try<HexInteger<I>>, Try<DecInteger<I>>>),
    data_type: DataType,
    marker: PhantomData<fn(I) -> I>,
}

impl<I: Stream<Item = char>> Parser for Integer<I> {
    type Input = I;
    type Output = NumericValue;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input).map(|(neg, num)| {
            if let Some(..) = neg {
                match num {
                    NumericValue::UByte(b)  => NumericValue::SByte (-1 * b as  i8),
                    NumericValue::UShort(s) => NumericValue::SShort(-1 * s as i16),
                    NumericValue::ULong(l)  => NumericValue::SLong (-1 * l as i32),
                    NumericValue::UQuad(q)  => NumericValue::SQuad (-1 * q as i64),

                    NumericValue::SByte(b)  => NumericValue::SByte (-1 * b),
                    NumericValue::SShort(s) => NumericValue::SShort(-1 * s),
                    NumericValue::SLong(l)  => NumericValue::SLong (-1 * l),
                    NumericValue::SQuad(q)  => NumericValue::SQuad (-1 * q),
                }
            } else {
                num
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

pub fn integer<I: Stream<Item = char>>(data_type: DataType) -> Integer<I> {
    Integer {
        parser: (optional(token('-')), try(hex_integer(data_type.clone())).or(try(dec_integer(data_type.clone())))),
        data_type: data_type,
        marker: PhantomData,
    }
}

#[derive(Clone)]
pub struct HexInteger<I: Stream<Item = char>> {
    parser: With<Str<I>, Many1<String, HexDigit<I>>>,
    data_type: DataType,
    marker: PhantomData<fn(I) -> I>,
}

impl<I: Stream<Item = char>> Parser for HexInteger<I> {
    type Input = I;
    type Output = NumericValue;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input).map(|num| {
            match self.data_type {
                // Unsigned values.
                DataType::Byte { signed: false } => {
                    NumericValue::UByte(
                        u8::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 8-bit unsigned int", num)))
                },
                DataType::Short { endian: _, signed: false } => {
                    NumericValue::UShort(
                        u16::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 16-bit unsigned int", num)))
                },
                DataType::Long { endian: _, signed: false } => {
                    NumericValue::ULong(
                        u32::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 32-bit unsigned int", num)))
                },
                DataType::Quad { endian: _, signed: false } => {
                    NumericValue::UQuad(
                        u64::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 64-bit unsigned int", num)))
                },

                // Signed values.
                DataType::Byte { signed: true } => {
                    NumericValue::SByte(
                        i8::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 8-bit signed int", num)))
                },
                DataType::Short { endian: _, signed: true } => {
                    NumericValue::SShort(
                        i16::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 16-bit signed int", num)))
                },
                DataType::Long { endian: _, signed: true } => {
                    NumericValue::SLong(
                        i32::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 32-bit signed int", num)))
                },
                DataType::Quad { endian: _, signed: true } => {
                    NumericValue::SQuad(
                        i64::from_str_radix(&num, 16)
                            .expect(&format!("Could not parse {} as an 64-bit signed int", num)))
                },

                _ => unreachable!(),
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

#[inline(always)]
pub fn hex_integer<I>(data_type: DataType) -> HexInteger<I> where I: Stream<Item = char> {
    HexInteger {
        parser: string("0x").with(many1::<String, _>(hex_digit())),
        data_type: data_type,
        marker: PhantomData,
    }
}

#[derive(Clone)]
pub struct DecInteger<I: Stream<Item = char>> {
    parser: Many1<String, Digit<I>>,
    data_type: DataType,
    marker: PhantomData<fn(I) -> I>,
}

impl<I: Stream<Item = char>> Parser for DecInteger<I> {
    type Input = I;
    type Output = NumericValue;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input).map(|num| {
            match self.data_type {
                // Unsigned values.
                DataType::Byte { signed: false } => {
                    NumericValue::UByte(
                        u8::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 8-bit unsigned int", num)))
                },
                DataType::Short { endian: _, signed: false } => {
                    NumericValue::UShort(
                        u16::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 16-bit unsigned int", num)))
                },
                DataType::Long { endian: _, signed: false } => {
                    NumericValue::ULong(
                        u32::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 32-bit unsigned int", num)))
                },
                DataType::Quad { endian: _, signed: false } => {
                    NumericValue::UQuad(
                        u64::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 64-bit unsigned int", num)))
                },

                // Signed values.
                DataType::Byte { signed: true } => {
                    NumericValue::SByte(
                        i8::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 8-bit signed int", num)))
                },
                DataType::Short { endian: _, signed: true } => {
                    NumericValue::SShort(
                        i16::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 16-bit signed int", num)))
                },
                DataType::Long { endian: _, signed: true } => {
                    NumericValue::SLong(
                        i32::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 32-bit signed int", num)))
                },
                DataType::Quad { endian: _, signed: true } => {
                    NumericValue::SQuad(
                        i64::from_str_radix(&num, 10)
                            .expect(&format!("Could not parse {} as an 64-bit signed int", num)))
                },

                _ => unreachable!(),
            }
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

#[inline(always)]
pub fn dec_integer<I>(data_type: DataType) -> DecInteger<I> where I: Stream<Item = char> {
    DecInteger {
        parser: many1::<String, _>(digit()),
        data_type: data_type,
        marker: PhantomData,
    }
}

#[cfg(test)]
mod tests {
    use combine::Parser;
    use endian::Endian;
    use magic::*;
    use super::super::DataType;

    #[test]
    fn integers() {
        assert_eq!(
            Ok((NumericValue::ULong(3_551_379_183), "")),
            super::integer(DataType::Long { endian: Endian::Native, signed: false }).parse("0xd3adBEEF"));

        assert_eq!(
            Ok((NumericValue::SShort(314), "")),
            super::integer(DataType::Short { endian: Endian::Native, signed: true }).parse("314"));

        assert_eq!(
            Ok((NumericValue::SShort(-314), "")),
            super::integer(DataType::Short { endian: Endian::Native, signed: true }).parse("-314"));

        assert_eq!(
            Ok((NumericValue::UByte(0), "")),
            super::integer(DataType::Byte { signed: false }).parse("0"));

        // Should this actually be octal?
        assert_eq!(
            Ok((NumericValue::UShort(314), "")),
            super::integer(DataType::Short { endian: Endian::Native, signed: false }).parse("0314"));
    }

    #[test]
    fn numerical_operators() {
        assert_eq!(Ok((NumOp::Equal, "")),       super::numeric_operator().parse("="));
        assert_eq!(Ok((NumOp::GreaterThan, "")), super::numeric_operator().parse(">"));
        assert_eq!(Ok((NumOp::LessThan, "")),    super::numeric_operator().parse("<"));
        assert_eq!(Ok((NumOp::NotEqual, "")),    super::numeric_operator().parse("!"));
        // assert_eq!(Ok((NumOp::BitAnd, "")),      super::numeric_operator().parse("&"));
        // assert_eq!(Ok((NumOp::BitXor, "")),      super::numeric_operator().parse("^"));
        // assert_eq!(Ok((NumOp::BitNeg, "")),      super::numeric_operator().parse("~"));
    }

    // #[test]
    // fn string_operators() {
    //     assert_eq!(Ok((StrOp::Equal, "")), super::string_operator().parse("="));
    //     assert_eq!(Ok((StrOp::LexBefore, "")), super::string_operator().parse("<"));
    //     assert_eq!(Ok((StrOp::LexAfter, "")), super::string_operator().parse(">"));
    // }
}
