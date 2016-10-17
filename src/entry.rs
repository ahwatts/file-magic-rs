#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MagicEntry {
    pub level: u32,
    pub offset: Offset,
}

// 123     123 bytes from the start
// &123    123 bytes from here
// (123)   (the value at 123 bytes from the start) bytes from the start
// (&123)  (the value at 123 bytes from here) bytes from the start
// &(123)  (the value at 123 bytes from the start) bytes from here
// &(&123) (the value at 123 bytes from here) bytes from here
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Offset {
    Direct(DirectOffset),
    AbsoluteIndirect(IndirectOffset),
    RelativeIndirect(IndirectOffset),
}

impl Offset {
    pub fn absolute(val: u64) -> Offset {
        Offset::Direct(DirectOffset::Absolute(val))
    }

    pub fn relative(val: i64) -> Offset {
        Offset::Direct(DirectOffset::Relative(val))
    }

    pub fn direct(base: DirectOffset) -> Offset {
        Offset::Direct(base)
    }

    pub fn absolute_indirect(base: IndirectOffset) -> Offset {
        Offset::AbsoluteIndirect(base)
    }

    pub fn relative_indirect(base: IndirectOffset) -> Offset {
        Offset::RelativeIndirect(base)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DirectOffset {
    Absolute(u64),
    Relative(i64),
}

impl DirectOffset {
    pub fn absolute(val: u64) -> DirectOffset {
        DirectOffset::Absolute(val)
    }

    pub fn relative(val: i64) -> DirectOffset {
        DirectOffset::Relative(val)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IndirectOffset {
    pub base: DirectOffset,
    pub length: usize,
    pub format: IndirectOffsetFormat,
    // pub op,
    // pub arg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum IndirectOffsetFormat {
    Byte,
    BigEndian,
    LittleEndian,
    BigEndianId3,
    LittleEndianId3,
    Pdp11Endian,
}

pub enum DataType {
    Byte,
    Short(Endian),
    Long(Endian),
    Quad(Endian),
    Float(Endian),
    Double(Endian),

    Id3(Endian),

    LongDate(Endian, TimeZone),
    QuadDate(Endian, TimeZone),
    WindowsDate(Endian),

    String, PascalString, BigEndianString16, LittleEndianString16,
    Indirect, Name, Use,
    Regex, Search,
    Default, Clear,
}

pub enum Endian {
    Little,
    Big,
    Native,
    Pdp11,
}

pub enum TimeZone {
    Local,
    Utc,
}
