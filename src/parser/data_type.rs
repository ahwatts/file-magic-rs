// -*- rustic-indent-offset: 4; -*-

use anyhow::anyhow;
use nom::{character::complete::alphanumeric1, combinator::map_res, IResult};

use crate::data_type::DataType;

#[rustfmt::skip]
pub fn data_type(input: &str) -> IResult<&str, DataType> {
    map_res(alphanumeric1, |type_name| {
        use crate::data_type::DataType::*;
        use crate::endian::Endian::*;

        match type_name {
            "byte"  => Ok(Byte  { signed: true }),

            "short"   => Ok(Short { endian: Native, signed: true }),
            "beshort" => Ok(Short { endian: Big,    signed: true }),
            "leshort" => Ok(Short { endian: Little, signed: true }),

            "long"   => Ok(Long { endian: Native, signed: true }),
            "belong" => Ok(Long { endian: Big,    signed: true }),
            "lelong" => Ok(Long { endian: Little, signed: true }),

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

            _ => Err(anyhow!("Unknown data type: {:?}", type_name)),
        }
    })(input)
}

#[cfg(test)]
mod tests {
    #[test]
    #[rustfmt::skip]
    fn data_type() {
        use crate::data_type::DataType::*;
        use crate::endian::Endian::*;

        assert_eq!(Ok(("", Byte { signed: true  })), super::data_type("byte"));

        assert_eq!(Ok(("", Short { endian: Native, signed: true })), super::data_type("short"));
        assert_eq!(Ok(("", Short { endian: Big,    signed: true })), super::data_type("beshort"));
        assert_eq!(Ok(("", Short { endian: Little, signed: true })), super::data_type("leshort"));

        assert_eq!(Ok(("", Long { endian: Native, signed: true })), super::data_type("long"));
        assert_eq!(Ok(("", Long { endian: Big,    signed: true })), super::data_type("belong"));
        assert_eq!(Ok(("", Long { endian: Little, signed: true })), super::data_type("lelong"));

        assert_eq!(Ok(("", Quad { endian: Native, signed: true })), super::data_type("quad"));
        assert_eq!(Ok(("", Quad { endian: Big,    signed: true })), super::data_type("bequad"));
        assert_eq!(Ok(("", Quad { endian: Little, signed: true })), super::data_type("lequad"));

        assert_eq!(Ok(("", Byte { signed: false })), super::data_type("ubyte"));

        assert_eq!(Ok(("", Short { endian: Native, signed: false })), super::data_type("ushort"));
        assert_eq!(Ok(("", Short { endian: Big,    signed: false })), super::data_type("ubeshort"));
        assert_eq!(Ok(("", Short { endian: Little, signed: false })), super::data_type("uleshort"));

        assert_eq!(Ok(("", Long { endian: Native, signed: false })), super::data_type("ulong"));
        assert_eq!(Ok(("", Long { endian: Big,    signed: false })), super::data_type("ubelong"));
        assert_eq!(Ok(("", Long { endian: Little, signed: false })), super::data_type("ulelong"));

        assert_eq!(Ok(("", Quad { endian: Native, signed: false })), super::data_type("uquad"));
        assert_eq!(Ok(("", Quad { endian: Big,    signed: false })), super::data_type("ubequad"));
        assert_eq!(Ok(("", Quad { endian: Little, signed: false })), super::data_type("ulequad"));

        assert_eq!(Ok(("", Float(Native))), super::data_type("float"));
        assert_eq!(Ok(("", Float(Big))), super::data_type("befloat"));
        assert_eq!(Ok(("", Float(Little))), super::data_type("lefloat"));

        assert_eq!(Ok(("", Double(Native))), super::data_type("double"));
        assert_eq!(Ok(("", Double(Big))), super::data_type("bedouble"));
        assert_eq!(Ok(("", Double(Little))), super::data_type("ledouble"));
    }

    #[test]
    fn data_type_with_mask() {
        use crate::data_type::DataType::*;
        use crate::endian::Endian::*;
        assert_eq!(
            Ok((
                "",
                Short {
                    signed: true,
                    endian: Little
                }
            )),
            super::data_type("leshort&0x3fff"),
        );
    }
}
