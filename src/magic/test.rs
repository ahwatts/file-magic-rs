use endian::Endian;
use error::MagicResult;
use std::io::Read;
use parser::DataType;

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum Test {
    AlwaysTrue,
    Number(NumericTest),
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub struct NumericTest {
    endian: Endian,
    logic_op: NumOp,
    test_value: NumericValue,
}

impl NumericTest {
    pub fn new(data_type: DataType, logic_op: NumOp, test_value: NumericValue) -> NumericTest {
        NumericTest {
            endian: data_type.endian(),
            logic_op: logic_op,
            test_value: test_value,
        }
    }

    pub fn matches<F: Read>(&self, _file: &mut F) -> MagicResult<bool> {
        unimplemented!()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum NumOp {
    Equal,
    LessThan,
    GreaterThan,
    NotEqual,
    // BitAnd,
    // BitXor,
    // BitNeg,
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum NumericValue {
    UByte(u8), UShort(u16), ULong(u32), UQuad(u64),
    SByte(i8), SShort(i16), SLong(i32), SQuad(i64),
    Float(f32), Double(f64),
}
