use endian::Endian;
use error::MagicResult;
use parser::DataType;
use std::fmt::Debug;
use std::io::Read;

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum Test {
    AlwaysTrue,
    Number(NumericTest),
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub struct NumericTest {
    pub endian: Endian,
    pub logic_op: NumOp,
    pub test_value: NumericValue,
}

impl NumericTest {
    pub fn new(data_type: &DataType, logic_op: NumOp, test_value: NumericValue) -> NumericTest {
        NumericTest {
            endian: data_type.endian(),
            logic_op: logic_op,
            test_value: test_value,
        }
    }

    pub fn matches<F: Read>(&self, file: &mut F) -> MagicResult<bool> {
        match self.test_value {
            NumericValue::UByte(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_u8(file)),  test_value)),
            NumericValue::UShort(test_value) => Ok(self.logic_op.matches(try!(self.endian.read_u16(file)), test_value)),
            NumericValue::ULong(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_u32(file)), test_value)),
            NumericValue::UQuad(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_u64(file)), test_value)),

            NumericValue::SByte(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_i8(file)),  test_value)),
            NumericValue::SShort(test_value) => Ok(self.logic_op.matches(try!(self.endian.read_i16(file)), test_value)),
            NumericValue::SLong(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_i32(file)), test_value)),
            NumericValue::SQuad(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_i64(file)), test_value)),
        }
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

impl NumOp {
    fn matches<T: PartialEq + PartialOrd + Sized + Debug>(&self, lhs: T, rhs: T) -> bool {
        println!("NumOp matches: {:?} {:?} {:?}", lhs, self, rhs);
        match self {
            &NumOp::Equal       => lhs == rhs,
            &NumOp::LessThan    => lhs  < rhs,
            &NumOp::GreaterThan => lhs  > rhs,
            &NumOp::NotEqual    => lhs != rhs,
        }
    }
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum NumericValue {
    UByte(u8), UShort(u16), ULong(u32), UQuad(u64),
    SByte(i8), SShort(i16), SLong(i32), SQuad(i64),
    // Float32(f32), Float64(f64),
}
