use byteorder::*;
use endian::*;
use std::io::{self, Cursor, Read, Seek, SeekFrom};
use std::iter;
use super::MatchResult;
use error::MagicResult;

#[derive(Clone, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub struct MagicEntry {
    pub filename: String,
    pub line_num: usize,
    pub level: u32,
    pub offset: Offset,
    pub data_type: DataType,
    pub test: Test,
    pub message: String,
}

impl MagicEntry {
    pub fn matches<F: Read + Seek>(&self, file: &mut F) -> MagicResult<MatchResult> {
        try!(self.offset.seek_to(file));

        let mut file_value: Vec<u8> = iter::repeat(0u8).take(self.magic_len()).collect();
        try!(file.read_exact(&mut file_value));

        println!("file value = {:?}", file_value);
        println!("test value = {:?}", self.test);

        match self.test {
            Test::AlwaysTrue => Ok(MatchResult::Matches(self.message.clone())),
            Test::Number { op, value } => {
                if self.number_match(op, value, &file_value) {
                    Ok(MatchResult::Matches(self.message.clone()))
                } else {
                    Ok(MatchResult::NoMatch)
                }
            },
            Test::String {..} => Ok(MatchResult::NoMatch),
        }
    }

    fn number_match(&self, op: NumOp, test_value: Numeric, file_bytes: &[u8]) -> bool {
        let file_value = self.extract_numeric_value(file_bytes);
        println!("file value (extracted) = {:?}", file_value);

        match op {
            NumOp::Equal       => file_value == test_value as i64,
            NumOp::GreaterThan => file_value >  test_value as i64,
            NumOp::LessThan    => file_value <  test_value as i64,
            NumOp::Not         => file_value != test_value as i64,
            NumOp::BitAnd      => (file_value & test_value as i64) > 0,
            NumOp::BitXor      => (file_value ^ test_value as i64) > 0,
            NumOp::BitNeg      => unimplemented!(),
        }
    }

    fn extract_numeric_value(&self, file_value: &[u8]) -> Numeric {
        use self::DataType::*;
        use self::Numeric::*;
        use endian::Endian::*;

        let mut reader = Cursor::new(file_value);

        match self.data_type {
            Short { endian: Pdp11, signed: _ } => panic!("Middle (PDP-11) endian with short data type"),
            Quad  { endian: Pdp11, signed: _ } => panic!("Middle (PDP-11) endian with quad data type"),
            Double(Pdp11)  => panic!("Middle (PDP-11) endian with double data type"),

            Byte { signed: true  } =>   SignedInt(reader.read_i8().unwrap() as i64),
            Byte { signed: false } => UnsignedInt(reader.read_u8().unwrap() as u64),

            Short { endian: e, signed: true  } =>   SignedInt(e.read_i16(&mut reader).unwrap() as i64),
            Short { endian: e, signed: false } => UnsignedInt(e.read_u16(&mut reader).unwrap() as u64),

            Long { endian: e, signed: true  } =>   SignedInt(e.read_i32(&mut reader).unwrap() as i64),
            Long { endian: e, signed: false } => UnsignedInt(e.read_u32(&mut reader).unwrap() as u64),

            Quad { endian: e, signed: true  } =>   SignedInt(e.read_i64(&mut reader).unwrap() as i64),
            Quad { endian: e, signed: false } => UnsignedInt(e.read_u64(&mut reader).unwrap() as u64),

            Float(e)  => FloatingPoint(e.read_f32(&mut reader).unwrap() as f64),
            Double(e) => FloatingPoint(e.read_f64(&mut reader).unwrap() as f64),

            String => panic!("String data type with a numeric test value!"),
        }
    }

    fn magic_len(&self) -> usize {
        use self::DataType::*;

        match self.data_type {
            Byte { .. }  => 1,
            Short { .. } => 2,
            Long { .. } | Float(..)  => 4,
            Quad { .. } | Double(..) => 8,
            String => {
                if let Test::String { op: _, value: ref s } = self.test {
                    s.len()
                } else {
                    panic!("String data type with non-string test value!")
                }
            },
        }
    }
}

// 123     123 bytes from the start
// &123    123 bytes from here
// (123)   (the value at 123 bytes from the start) bytes from the start
// (&123)  (the value at 123 bytes from here) bytes from the start
// &(123)  (the value at 123 bytes from the start) bytes from here
// &(&123) (the value at 123 bytes from here) bytes from here
#[derive(Clone, Copy, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
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

#[derive(Clone, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum DataType {
    Byte  {                         signed: bool },
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

impl Numeric {
    pub fn matches(&self, op: NumOp, other: &Numeric) -> bool {
        use self::Numeric::*;

        match (self, other) {
            (&SignedInt(lhs), &SignedInt(rhs)) => lhs == rhs,
        }
    }
}
