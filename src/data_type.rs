use endian::Endian;

#[derive(Clone, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum DataDesc {
    Byte  {                 signed: bool },
    Short { endian: Endian, signed: bool },
    Long  { endian: Endian, signed: bool },
    Quad  { endian: Endian, signed: bool },
    Float(Endian),
    Double(Endian),

    String,

    // Id3(Endian),

    // LongDate(Endian, TimeZone),
    // QuadDate(Endian, TimeZone),
    // WindowsDate(Endian),

    // String, PascalString, BigEndianString16, LittleEndianString16,
    // Indirect, Name, Use,
    // Regex, Search,
    // Default, Clear,
}

impl DataDesc {
    pub fn endian(&self) -> Endian {
        use self::DataDesc::*;
        match self {
            &Byte  {..} => Endian::Native,
            &Short { endian: e, signed: _ } => e,
            &Long  { endian: e, signed: _ } => e,
            &Quad  { endian: e, signed: _ } => e,
            &Float(e) => e,
            &Double(e) => e,
            &String => Endian::Native,
        }
    }
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum NumericValue {
    UByte(u8), UShort(u16), ULong(u32), UQuad(u64),
    SByte(i8), SShort(i16), SLong(i32), SQuad(i64),
    // Float32(f32), Float64(f64),
}

impl NumericValue {
    pub fn from_described_str_radix<S: AsRef<str>>(desc: &DataDesc, val_str: S, radix: u32) -> NumericValue {
        use self::DataDesc::*;
        use self::NumericValue::*;

        match desc {
            // Unsigned values.
            &Byte { signed: false } => {
                UByte(
                    u8::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 8-bit unsigned int", val_str.as_ref())))
            },
            &Short { endian: _, signed: false } => {
                UShort(
                    u16::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 16-bit unsigned int", val_str.as_ref())))
            },
            &Long { endian: _, signed: false } => {
                ULong(
                    u32::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 32-bit unsigned int", val_str.as_ref())))
            },
            &Quad { endian: _, signed: false } => {
                UQuad(
                    u64::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 64-bit unsigned int", val_str.as_ref())))
            },

            // Signed values.
            &Byte { signed: true } => {
                SByte(
                    i8::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 8-bit signed int", val_str.as_ref())))
            },
            &Short { endian: _, signed: true } => {
                SShort(
                    i16::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 16-bit signed int", val_str.as_ref())))
            },
            &Long { endian: _, signed: true } => {
                SLong(
                    i32::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 32-bit signed int", val_str.as_ref())))
            },
            &Quad { endian: _, signed: true } => {
                SQuad(
                    i64::from_str_radix(&val_str.as_ref(), radix)
                        .expect(&format!("Could not parse {} as an 64-bit signed int", val_str.as_ref())))
            },

            _ => unreachable!()
        }
    }
}
