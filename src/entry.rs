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
    pub format: Format,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Format {
    Byte,
    BigEndian,
    LittleEndian,
    BigEndianId3,
    LittleEndianId3,
    Pdp11Endian,
}

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
