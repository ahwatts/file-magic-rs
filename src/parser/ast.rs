use endian::Endian;

#[derive(Clone, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum DataType {
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

impl DataType {
    pub fn endian(&self) -> Endian {
        use self::DataType::*;
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
