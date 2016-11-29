use error::{MagicError, MagicResult};
use data_type;
use num::{Num, Integer};
use std::fmt::Debug;
use std::mem;
use std::io::Read;

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub struct Test {
    data_type: data_type::DataType,
    test_type: TestType,
}

impl Test {
    pub fn new(desc: data_type::DataType, test_type: TestType) -> Test {
        Test {
            data_type: desc,
            test_type: test_type,
        }
    }

    pub fn data_type(&self) -> &data_type::DataType {
        &self.data_type
    }

    pub fn test_type(&self) -> &TestType {
        &self.test_type
    }

    pub fn matches<R: Read>(&self, file: &mut R) -> MagicResult<bool> {
        match self.test_type {
            TestType::AlwaysTrue => Ok(true),
            TestType::Number(ref num_test) => num_test.matches_file(&self.data_type, file),
        }
    }
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub enum TestType {
    AlwaysTrue,
    Number(NumericTest),
}

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub struct NumericTest {
    logic_op: NumOp,
    test_value: Vec<u8>,
}

impl NumericTest {
    pub fn new<N: Num + Integer>(logic_op: NumOp, test_value: N) -> NumericTest {
        NumericTest {
            logic_op: logic_op,
            test_value: data_type::sized_to_byte_vec(test_value),
        }
    }

    pub fn new_from_bytes<I: Into<Vec<u8>> + Debug>(logic_op: NumOp, test_value_bytes: I) -> NumericTest {
        let rv = NumericTest {
            logic_op: logic_op,
            test_value: test_value_bytes.into(),
        };
        rv
    }

    pub fn matches_file<R: Read>(&self, data_type: &data_type::DataType, file: &mut R) -> MagicResult<bool> {
        use data_type::DataType::*;

        let actual_data = data_type.read(file)?;

        match data_type {
            &Byte { signed: true  } => self.matches_type::<i8>(&actual_data),
            &Byte { signed: false } => self.matches_type::<u8>(&actual_data),
            &Short { endian: _, signed: true  } => self.matches_type::<i16>(&actual_data),
            &Short { endian: _, signed: false } => self.matches_type::<u16>(&actual_data),
            &Long  { endian: _, signed: true  } => self.matches_type::<i32>(&actual_data),
            &Long  { endian: _, signed: false } => self.matches_type::<u32>(&actual_data),
            &Quad  { endian: _, signed: true  } => self.matches_type::<i64>(&actual_data),
            &Quad  { endian: _, signed: false } => self.matches_type::<u64>(&actual_data),
            _ => unreachable!(),
        }
    }

    fn matches_type<N>(&self, actual_value: &[u8]) -> MagicResult<bool>
        where N: Num + Integer + Debug + Sized + Copy
    {
        if actual_value.len() != self.test_value.len() {
            return Err(MagicError::LengthMismatch(self.test_value.len(), actual_value.len()));
        } else if mem::size_of::<N>() != self.test_value.len() {
            return Err(MagicError::LengthMismatch(self.test_value.len(), mem::size_of::<N>()));
        }

        let casted_test: N = unsafe { *(self.test_value.as_ptr() as *const N) };
        let casted_actual: N = unsafe { *(actual_value.as_ptr() as *const N) };
        Ok(self.logic_op.matches(casted_test, casted_actual))
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

#[cfg(test)]
mod tests {
    use super::{NumOp, NumericTest};

    #[test]
    fn numeric_test_matches_type() {
        let test = NumericTest {
            logic_op: NumOp::Equal,
            test_value: vec![ 3u8, 0, 0, 0 ],
        };

        assert!(test.matches_type::<u32>(&vec![ 3u8, 0, 0, 0 ]).unwrap());
        assert!(test.matches_type::<u32>(&[ 3u8, 0, 0, 0 ]).unwrap());
        assert!(!test.matches_type::<u32>(&vec![ 4u8, 0, 0, 0 ]).unwrap());
        assert!(test.matches_type::<u16>(&vec![ 3u8, 0, ]).is_err());
        assert!(test.matches_type::<u16>(&vec![ 3u8, 0, 0, 0 ]).is_err());
    }
}
