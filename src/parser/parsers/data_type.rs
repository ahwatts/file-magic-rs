use combine::char::*;
use combine::combinator::*;
use combine::{ConsumedResult, ParseError, Parser, Stream};
use data_type;
use std::io::{self, ErrorKind};
use std::marker::PhantomData;

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

#[cfg(test)]
mod tests {
    use combine::Parser;

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
}
