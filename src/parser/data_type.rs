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
            "byte"   | "dC" | "d1" => Ok(Byte  { signed: true }),
            "ubyte"  | "uC" | "u1" => Ok(Byte  { signed: false }),
            "short"  | "dS" | "d2" => Ok(Short { endian: Native, signed: true }),
            "ushort" | "uS" | "u2" => Ok(Short { endian: Native, signed: false }),
            "long"   | "dI" | "dL" | "d4" => Ok(Long { endian: Native, signed: true }),
            "ulong"  | "uI" | "uL" | "u4" => Ok(Long { endian: Native, signed: false }),
            "quad"   | "dQ" => Ok(Quad { endian: Native, signed: true }),
            "uquad"  | "uQ" => Ok(Quad { endian: Native, signed: false }),
            "float"  => Ok(Float(Native)),
            "double" => Ok(Double(Native)),

            "beshort"  => Ok(Short { endian: Big, signed: true }),
            "ubeshort" => Ok(Short { endian: Big, signed: false }),
            "belong"   => Ok(Long  { endian: Big, signed: true }),
            "ubelong"  => Ok(Long  { endian: Big, signed: false }),
            "bequad"   => Ok(Quad  { endian: Big, signed: true }),
            "ubequad"  => Ok(Quad  { endian: Big, signed: false }),
            "befloat"  => Ok(Float(Big)),
            "bedouble" => Ok(Double(Big)),

            "leshort"  => Ok(Short { endian: Little, signed: true }),
            "uleshort" => Ok(Short { endian: Little, signed: false }),
            "lelong"   => Ok(Long  { endian: Little, signed: true }),
            "ulelong"  => Ok(Long  { endian: Little, signed: false }),
            "lequad"   => Ok(Quad  { endian: Little, signed: true }),
            "ulequad"  => Ok(Quad  { endian: Little, signed: false }),
            "lefloat"  => Ok(Float(Little)),
            "ledouble" => Ok(Double(Little)),

            "melong" | "medate" | "meldate" => Err(anyhow!("PDP-11 middle-endian values not supported")),

            "string" | "s" => Ok(String),
            "pstring" => Err(anyhow!("Pascal strings not supported (yet)")),
            "bestring16" | "lestring16" => Err(anyhow!("UCS16 strings not supported (yet)")),

            "date" | "qdate" | "ldate" | "qldate" | "qwdate" |
            "bedate" | "beqdate" | "beldate" | "beqldate" | "beqwdate" |
            "ledate" | "leqdate" | "leldate" | "leqldate" | "leqwdate"
                => Err(anyhow!("Date values not supported (yet)")),

            "beid3" | "leid3" => Err(anyhow!("ID3 values not supported (yet)")),
            "indirect" | "indirect/r" => Err(anyhow!("Indirect magic not supported (yet)")),

            "name" => Ok(Name("".to_string())),
            "use"  => Ok(Use("".to_string())),

            "regex" => Err(anyhow!("Regex tests not supported (yet)")),
            "search" => Err(anyhow!("Literal search strings not supported (yet)")),
            "default" | "clear" => Err(anyhow!("Default no-type tests not supported (yet)")),
            "der" => Err(anyhow!("DER certificate parsing not supported (yet)")),
            "guid" => Err(anyhow!("GUID tests not supported (yet)")),
            "offset" => Err(anyhow!("Offset tests are not supported (yet)")),

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
