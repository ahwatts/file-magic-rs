use endian::Endian;
use error::MagicResult;
use data_type::{DataDesc, NumericValue};
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
    pub fn new(data_desc: &DataDesc, logic_op: NumOp, test_value: NumericValue) -> NumericTest {
        NumericTest {
            endian: data_desc.endian(),
            logic_op: logic_op,
            test_value: test_value,
        }
    }

    pub fn matches<F: Read>(&self, file: &mut F) -> MagicResult<bool> {
        use data_type::NumericValue::*;

        match self.test_value {
            UByte(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_u8(file)),  test_value)),
            UShort(test_value) => Ok(self.logic_op.matches(try!(self.endian.read_u16(file)), test_value)),
            ULong(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_u32(file)), test_value)),
            UQuad(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_u64(file)), test_value)),

            SByte(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_i8(file)),  test_value)),
            SShort(test_value) => Ok(self.logic_op.matches(try!(self.endian.read_i16(file)), test_value)),
            SLong(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_i32(file)), test_value)),
            SQuad(test_value)  => Ok(self.logic_op.matches(try!(self.endian.read_i64(file)), test_value)),
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
        use self::NumOp::*;

        println!("NumOp matches: {:?} {:?} {:?}", lhs, self, rhs);

        match self {
            &Equal       => lhs == rhs,
            &LessThan    => lhs  < rhs,
            &GreaterThan => lhs  > rhs,
            &NotEqual    => lhs != rhs,
        }
    }
}
