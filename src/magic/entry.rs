use std::io::{Read, Seek};
use super::MatchResult;
use super::offset::Offset;
use super::test::Test;
use error::MagicResult;

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub struct MagicEntry {
    pub filename: String,
    pub line_num: usize,
    pub level: u32,
    pub offset: Offset,
    pub test: Test,
    pub message: String,
}

impl MagicEntry {
    pub fn matches<F: Read + Seek>(&self, file: &mut F) -> MagicResult<MatchResult> {
        try!(self.offset.seek_to(file));

        match self.test {
            Test::AlwaysTrue => Ok(MatchResult::Matches(self.message.clone())),
            Test::Number(ref num_test) => {
                match num_test.matches(file) {
                    Ok(true) => Ok(MatchResult::Matches(self.message.clone())),
                    Ok(false) => Ok(MatchResult::NoMatch),
                    Err(e) => Err(e),
                }
            },
        }
    }

    // fn number_match(&self, op: NumOp, test_value: Numeric, file_bytes: &[u8]) -> bool {
    //     let file_value = self.extract_numeric_value(file_bytes);
    //     println!("file value (extracted) = {:?}", file_value);

    //     // match op {
    //     //     NumOp::Equal       => file_value == test_value as i64,
    //     //     NumOp::GreaterThan => file_value >  test_value as i64,
    //     //     NumOp::LessThan    => file_value <  test_value as i64,
    //     //     NumOp::Not         => file_value != test_value as i64,
    //     //     NumOp::BitAnd      => (file_value & test_value as i64) > 0,
    //     //     NumOp::BitXor      => (file_value ^ test_value as i64) > 0,
    //     //     NumOp::BitNeg      => unimplemented!(),
    //     // }
    //     unimplemented!()
    // }

    // fn extract_numeric_value(&self, file_value: &[u8]) -> Numeric {
    //     use super::test::DataType::*;
    //     use super::test::Numeric::*;
    //     use endian::Endian::*;

    //     let mut reader = Cursor::new(file_value);

    //     // match self.data_type {
    //     //     Short { endian: Pdp11, signed: _ } => panic!("Middle (PDP-11) endian with short data type"),
    //     //     Quad  { endian: Pdp11, signed: _ } => panic!("Middle (PDP-11) endian with quad data type"),
    //     //     Double(Pdp11)  => panic!("Middle (PDP-11) endian with double data type"),

    //     //     Byte { signed: true  } =>   SignedInt(reader.read_i8().unwrap() as i64),
    //     //     Byte { signed: false } => UnsignedInt(reader.read_u8().unwrap() as u64),

    //     //     Short { endian: e, signed: true  } =>   SignedInt(e.read_i16(&mut reader).unwrap() as i64),
    //     //     Short { endian: e, signed: false } => UnsignedInt(e.read_u16(&mut reader).unwrap() as u64),

    //     //     Long { endian: e, signed: true  } =>   SignedInt(e.read_i32(&mut reader).unwrap() as i64),
    //     //     Long { endian: e, signed: false } => UnsignedInt(e.read_u32(&mut reader).unwrap() as u64),

    //     //     Quad { endian: e, signed: true  } =>   SignedInt(e.read_i64(&mut reader).unwrap() as i64),
    //     //     Quad { endian: e, signed: false } => UnsignedInt(e.read_u64(&mut reader).unwrap() as u64),

    //     //     Float(e)  => FloatingPoint(e.read_f32(&mut reader).unwrap() as f64),
    //     //     Double(e) => FloatingPoint(e.read_f64(&mut reader).unwrap() as f64),

    //     //     String => panic!("String data type with a numeric test value!"),
    //     // }
    //     unimplemented!()
    // }

    // fn magic_len(&self) -> usize {
    //     use super::test::DataType::*;

    //     match self.data_type {
    //         Byte { .. }  => 1,
    //         Short { .. } => 2,
    //         Long { .. } | Float(..)  => 4,
    //         Quad { .. } | Double(..) => 8,
    //         String => {
    //             if let Test::String { op: _, value: ref s } = self.test {
    //                 s.len()
    //             } else {
    //                 panic!("String data type with non-string test value!")
    //             }
    //         },
    //     }
    // }
}
