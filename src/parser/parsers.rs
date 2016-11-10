use combine::char::*;
use combine::combinator::*;
use combine::{ConsumedResult, ParseError, Parser, Stream};
use data_type::{DataDesc, NumericValue};
use magic::*;
use std::io::{self, ErrorKind};
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
pub struct DataType<I: Stream<Item = char>> {
    parser: AndThen<Many1<String, AlphaNum<I>>, fn(String) -> io::Result<DataDesc>>,
    _marker: PhantomData<fn(I) -> I>
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

pub fn data_type<I: Stream<Item = char>>() -> DataType<I> {
    DataType {
        parser: many1::<String, _>(alpha_num()).and_then(translate_data_type_value),
        _marker: PhantomData,
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

#[derive(Clone)]
pub struct Integer<I: Stream<Item = char>> {
    parser: (Optional<Token<I>>, Or<Try<HexInteger<I>>, Try<DecInteger<I>>>),
    data_type: DataDesc,
    marker: PhantomData<fn(I) -> I>,
}

impl<I: Stream<Item = char>> Parser for Integer<I> {
    type Input = I;
    type Output = NumericValue;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input).map(|(neg, num)| {
            use data_type::NumericValue::*;

            if let Some(..) = neg {
                match num {
                    UByte(b)  => SByte (-1 * b as  i8),
                    UShort(s) => SShort(-1 * s as i16),
                    ULong(l)  => SLong (-1 * l as i32),
                    UQuad(q)  => SQuad (-1 * q as i64),

                    SByte(b)  => SByte (-1 * b),
                    SShort(s) => SShort(-1 * s),
                    SLong(l)  => SLong (-1 * l),
                    SQuad(q)  => SQuad (-1 * q),
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

pub fn integer<I: Stream<Item = char>>(data_type: DataDesc) -> Integer<I> {
    Integer {
        parser: (optional(token('-')), try(hex_integer(data_type.clone())).or(try(dec_integer(data_type.clone())))),
        data_type: data_type,
        marker: PhantomData,
    }
}

#[derive(Clone)]
pub struct HexInteger<I: Stream<Item = char>> {
    parser: With<Str<I>, Many1<String, HexDigit<I>>>,
    data_type: DataDesc,
    marker: PhantomData<fn(I) -> I>,
}

impl<I: Stream<Item = char>> Parser for HexInteger<I> {
    type Input = I;
    type Output = NumericValue;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input).map(|num| {
            NumericValue::from_described_str_radix(&self.data_type, &num, 16)
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

#[inline(always)]
pub fn hex_integer<I>(data_type: DataDesc) -> HexInteger<I> where I: Stream<Item = char> {
    HexInteger {
        parser: string("0x").with(many1::<String, _>(hex_digit())),
        data_type: data_type,
        marker: PhantomData,
    }
}

#[derive(Clone)]
pub struct DecInteger<I: Stream<Item = char>> {
    parser: Many1<String, Digit<I>>,
    data_type: DataDesc,
    marker: PhantomData<fn(I) -> I>,
}

impl<I: Stream<Item = char>> Parser for DecInteger<I> {
    type Input = I;
    type Output = NumericValue;

    #[inline]
    fn parse_lazy(&mut self, input: Self::Input) -> ConsumedResult<Self::Output, Self::Input> {
        self.parser.parse_lazy(input).map(|num| {
            NumericValue::from_described_str_radix(&self.data_type, num, 10)
        })
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.parser.add_error(errors)
    }
}

#[inline(always)]
pub fn dec_integer<I>(data_type: DataDesc) -> DecInteger<I> where I: Stream<Item = char> {
    DecInteger {
        parser: many1::<String, _>(digit()),
        data_type: data_type,
        marker: PhantomData,
    }
}

pub struct Escaped<T, I>(With<Token<I>, OneOf<T, I>>, PhantomData<fn(I) -> I>)
    where T: Clone + IntoIterator<Item = char>,
          I: Stream<Item = char>;

impl<T, I> Parser for Escaped<T, I>
    where T: Clone + IntoIterator<Item = char>,
          I: Stream<Item = char>
{
    type Input = I;
    type Output = char;

    #[inline]
    fn parse_lazy(&mut self, input: I) -> ConsumedResult<Self::Output, Self::Input> {
        self.0.parse_lazy(input)
    }

    fn add_error(&mut self, errors: &mut ParseError<Self::Input>) {
        self.0.add_error(errors)
    }
}

#[inline(always)]
pub fn escaped<T, I>(escaper: char, escapees: T) -> Escaped<T, I>
    where I: Stream<Item = char>,
          T: Clone + IntoIterator<Item = char>
{
    Escaped(token(escaper).with(one_of(escapees)), PhantomData)
}

#[cfg(test)]
mod tests {
    use combine::Parser;

    #[test]
    fn integers() {
        use data_type::DataDesc::*;
        use data_type::NumericValue::*;
        use endian::Endian::*;

        assert_eq!(
            Ok((ULong(3_551_379_183), "")),
            super::integer(Long { endian: Native, signed: false }).parse("0xd3adBEEF"));

        assert_eq!(
            Ok((SShort(314), "")),
            super::integer(Short { endian: Native, signed: true }).parse("314"));

        assert_eq!(
            Ok((SShort(-314), "")),
            super::integer(Short { endian: Native, signed: true }).parse("-314"));

        assert_eq!(
            Ok((UByte(0), "")),
            super::integer(Byte { signed: false }).parse("0"));

        // Should this actually be octal?
        assert_eq!(
            Ok((UShort(314), "")),
            super::integer(Short { endian: Native, signed: false }).parse("0314"));
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

        assert_eq!(Ok((Float(Native), "")), super::data_type().parse("float"));
        assert_eq!(Ok((Float(Big),    "")), super::data_type().parse("befloat"));
        assert_eq!(Ok((Float(Little), "")), super::data_type().parse("lefloat"));

        assert_eq!(Ok((Double(Native), "")), super::data_type().parse("double"));
        assert_eq!(Ok((Double(Big),    "")), super::data_type().parse("bedouble"));
        assert_eq!(Ok((Double(Little), "")), super::data_type().parse("ledouble"));
    }

    #[test]
    fn escaped_chars() {
        let mut parser = super::escaped('\\', "nrt\\'\"".chars());
        assert_eq!(Ok(('\n', "")), parser.parse("\\n"));
        assert_eq!(Ok(('\r', "")), parser.parse("\\r"));
        assert_eq!(Ok(('\t', "")), parser.parse("\\t"));
        assert_eq!(Ok(('\'', "")), parser.parse("\\'"));
        assert_eq!(Ok(('"', "")), parser.parse("\\\""));
        assert_eq!(Ok(('\0', "")), parser.parse("\\0"));
        assert_eq!(Ok(('\x0E', "")), parser.parse("\\016"));
    }
}
