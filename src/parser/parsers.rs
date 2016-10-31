use combine::{ConsumedResult, ParseError, Parser, Stream};
use combine::char::*;
use combine::combinator::*;
use entry::*;
use std::marker::PhantomData;
use std::str::Chars;

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
                '!' => NumOp::Not,
                '&' => NumOp::BitAnd,
                '^' => NumOp::BitXor,
                '~' => NumOp::BitNeg,
                _ => unreachable!(),
            }
        })
    }
}

#[inline(always)]
pub fn numeric_operator<I: Stream<Item = char>>() -> NumericOperator<I> {
    NumericOperator(one_of("=<>!&^~".chars()), PhantomData)
}

impl_parser! {
    StringOperator(), char, OneOf<Chars<'static>, I>, StrOp,
    |celf, input| {
        celf.0.parse_lazy(input).map(|c| {
            match c {
                '=' => StrOp::Equal,
                '>' => StrOp::LexAfter,
                '<' => StrOp::LexBefore,
                _ => unreachable!(),
            }
        })
    }
}

#[inline(always)]
pub fn string_operator<I: Stream<Item = char>>() -> StringOperator<I> {
    StringOperator(one_of("=<>".chars()), PhantomData)
}

// impl_parser! {
//     SignedNumber(), char, With<Token<I>, UnsignedNumber<I>>, i64,
//     |celf, input| {
//         celf.0.parse_lazy(input).map(|n| -1 * (n as i64))
//     }
// }

// #[inline(always)]
// pub fn signed_number<I: Stream<Item = char>>() -> SignedNumber<I> {
//     SignedNumber(token('-').with(unsigned_number()), PhantomData)
// }

impl_parser! {
    UnsignedNumber(), char, Or<Try<HexNumber<I>>, Try<DecNumber<I>>>, u64,
    |celf, input| { celf.0.parse_lazy(input) }
}

#[inline(always)]
pub fn unsigned_number<I: Stream<Item = char>>() -> UnsignedNumber<I> {
    UnsignedNumber(try(hex_number()).or(try(dec_number())), PhantomData)
}

impl_parser! {
    HexNumber(), char, With<Str<I>, Many1<String, HexDigit<I>>>, u64,
    |celf, input| {
        celf.0.parse_lazy(input).map(|hex_str| {
            u64::from_str_radix(&hex_str, 16).unwrap()
        })
    }
}

#[inline(always)]
pub fn hex_number<I>() -> HexNumber<I> where I: Stream<Item = char> {
    HexNumber(string("0x").with(many1::<String, _>(hex_digit())), PhantomData)
}

impl_parser! {
    DecNumber(), char, Many1<String, Digit<I>>, u64,
    |celf, input| {
        use std::str::FromStr;
        celf.0.parse_lazy(input).map(|dec_str| {
            u64::from_str(&dec_str).unwrap()
        })
    }
}

#[inline(always)]
pub fn dec_number<I>() -> DecNumber<I> where I: Stream<Item = char> {
    DecNumber(many1::<String, _>(digit()), PhantomData)
}

#[cfg(test)]
mod tests {
    use entry::*;
    use combine::Parser;

    #[test]
    fn numbers() {
        assert_eq!(Ok((3_551_379_183, "")), super::unsigned_number().parse("0xd3adBEEF"));
        assert_eq!(Ok((314, "")), super::unsigned_number().parse("314"));
        assert_eq!(Ok((0, "")), super::unsigned_number().parse("0"));

        // Should this actually be octal?
        assert_eq!(Ok((314, "")), super::unsigned_number().parse("0314"));
    }

    #[test]
    fn numerical_operators() {
        assert_eq!(Ok((NumOp::Equal, "")),       super::numeric_operator().parse("="));
        assert_eq!(Ok((NumOp::GreaterThan, "")), super::numeric_operator().parse(">"));
        assert_eq!(Ok((NumOp::LessThan, "")),    super::numeric_operator().parse("<"));
        assert_eq!(Ok((NumOp::Not, "")),         super::numeric_operator().parse("!"));
        assert_eq!(Ok((NumOp::BitAnd, "")),      super::numeric_operator().parse("&"));
        assert_eq!(Ok((NumOp::BitXor, "")),      super::numeric_operator().parse("^"));
        assert_eq!(Ok((NumOp::BitNeg, "")),      super::numeric_operator().parse("~"));
    }

    #[test]
    fn string_operators() {
        assert_eq!(Ok((StrOp::Equal, "")), super::string_operator().parse("="));
        assert_eq!(Ok((StrOp::LexBefore, "")), super::string_operator().parse("<"));
        assert_eq!(Ok((StrOp::LexAfter, "")), super::string_operator().parse(">"));
    }
}
