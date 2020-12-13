use byteorder::{BigEndian, LittleEndian, NativeEndian, ReadBytesExt};
use serde::{Deserialize, Serialize};
use std::io;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum Endian {
    Little,
    Big,
    Native,
}

macro_rules! read_bytes_fn {
    ($int_ty:ty, $read_fn:ident) => {
        pub fn $read_fn<R: ReadBytesExt>(&self, input: &mut R) -> io::Result<$int_ty> {
            use self::Endian::*;

            match self {
                &Little => input.$read_fn::<LittleEndian>(),
                &Big => input.$read_fn::<BigEndian>(),
                &Native => input.$read_fn::<NativeEndian>(),
            }
        }
    };
}

impl Endian {
    pub fn read_u8<R: ReadBytesExt>(&self, input: &mut R) -> io::Result<u8> {
        input.read_u8()
    }
    pub fn read_i8<R: ReadBytesExt>(&self, input: &mut R) -> io::Result<i8> {
        input.read_i8()
    }

    read_bytes_fn!(u16, read_u16);
    read_bytes_fn!(u32, read_u32);
    read_bytes_fn!(u64, read_u64);

    read_bytes_fn!(i16, read_i16);
    read_bytes_fn!(i32, read_i32);
    read_bytes_fn!(i64, read_i64);

    read_bytes_fn!(f32, read_f32);
    read_bytes_fn!(f64, read_f64);
}
