#![allow(unused_imports)]

use endian::Endian;
use error::MagicResult;
use num::{Num, PrimInt, Signed, Unsigned, FromPrimitive};
use std::any::Any;
use std::io::{self, Read};
use std::mem;

pub trait MagicTest {
    fn perform<F: Read>(&self, file: &mut F) -> MagicResult<bool>;
}

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

// #[derive(Clone, Copy, Debug, PartialEq, Eq)]
// pub enum TimeZone {
//     Local,
//     Utc,
// }

#[derive(Clone, Copy, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum NumOp {
    Equal,
    LessThan,
    GreaterThan,
    Not,
    BitAnd,
    BitXor,
    BitNeg,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum StrOp {
    Equal,
    LexBefore,
    LexAfter,
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum Test {
    AlwaysTrue,
    Number { op: NumOp, value: Numeric },
    String { op: StrOp, value: String },
}

#[derive(Clone, Copy, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum Numeric {
    SignedInt(i64),
    UnsignedInt(u64),
    FloatingPoint(f64),
}

// impl Numeric {
//     pub fn matches(&self, op: NumOp, other: &Numeric) -> bool {
//         use self::Numeric::*;

//         match (self, other) {
//             (&SignedInt(lhs), &SignedInt(rhs)) => lhs == rhs,
//         }
//     }
// }
