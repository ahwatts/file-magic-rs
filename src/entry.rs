#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MagicEntry {
    pub level: u32,
    pub offset: Offset,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BasicOffset {
    Absolute(u64),
    Relative(i64),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IndirectOffset {
    pub base: BasicOffset,
    pub length: u32,
    // op: Operation,
    // disp: Displacement,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Offset {
    Direct(BasicOffset),
    AbsoluteIndirect(IndirectOffset),
    RelativeIndirect(IndirectOffset),
}

impl Offset {
    pub fn absolute(val: u64) -> Offset {
        Offset::Direct(BasicOffset::Absolute(val))
    }

    pub fn relative(val: i64) -> Offset {
        Offset::Direct(BasicOffset::Relative(val))
    }
}
