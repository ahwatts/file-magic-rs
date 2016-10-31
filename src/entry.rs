use std::io::{self, Read, Seek, SeekFrom};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MagicEntry {
    pub level: u32,
    pub offset: Offset,
    pub data_type: DataType,
    pub test: Test,
    pub message: String,
}

impl MagicEntry {
    pub fn matches<F: Read + Seek>(&self, file: &mut F) -> io::Result<bool> {
        try!(self.offset.seek_to(file));
        unimplemented!();
    }
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
    // AbsoluteIndirect(IndirectOffset),
    // RelativeIndirect(IndirectOffset),
}

impl Offset {
    // pub fn absolute(val: u64) -> Offset {
    //     Offset::Direct(DirectOffset::Absolute(val))
    // }

    // pub fn relative(val: i64) -> Offset {
    //     Offset::Direct(DirectOffset::Relative(val))
    // }

    pub fn direct(base: DirectOffset) -> Offset {
        Offset::Direct(base)
    }

    // pub fn absolute_indirect(base: IndirectOffset) -> Offset {
    //     Offset::AbsoluteIndirect(base)
    // }

    // pub fn relative_indirect(base: IndirectOffset) -> Offset {
    //     Offset::RelativeIndirect(base)
    // }

    pub fn seek_to<F: Seek>(&self, file: &mut F) -> io::Result<()> {
        match self {
            &Offset::Direct(DirectOffset::Absolute(off)) => {
                try!(file.seek(SeekFrom::Start(off)));
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DirectOffset {
    Absolute(u64),
    // Relative(i64),
}

impl DirectOffset {
    pub fn absolute(val: u64) -> DirectOffset {
        DirectOffset::Absolute(val)
    }

    // pub fn relative(val: i64) -> DirectOffset {
    //     DirectOffset::Relative(val)
    // }
}

// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub struct IndirectOffset {
//     pub base: DirectOffset,
//     pub length: usize,
//     pub format: IndirectOffsetFormat,
//     // pub op,
//     // pub arg,
// }

// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub enum IndirectOffsetFormat {
//     Byte,
//     BigEndian,
//     LittleEndian,
//     BigEndianId3,
//     LittleEndianId3,
//     Pdp11Endian,
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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
    pub fn toggle_signed(self) -> DataType {
        use self::DataType::*;

        match self {
            Byte  {            signed: s } => Byte  {            signed: !s },
            Short { endian: e, signed: s } => Short { endian: e, signed: !s },
            Long  { endian: e, signed: s } => Long  { endian: e, signed: !s },
            Quad  { endian: e, signed: s } => Quad  { endian: e, signed: !s },
            v @ _ => v,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Endian {
    Little,
    Big,
    Native,
    Pdp11,
}

// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub enum TimeZone {
//     Local,
//     Utc,
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumOp {
    Equal,
    LessThan,
    GreaterThan,
    Not,
    BitAnd,
    BitXor,
    BitNeg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StrOp {
    Equal,
    LexBefore,
    LexAfter,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Test {
    AlwaysTrue,
    Number { op: NumOp, value: u64 },
    String { op: StrOp, value: String },
}

impl Test {
    pub fn matches<F: Read + Seek>(&self, _file: &mut F) -> bool {
        unimplemented!()
    }
}
