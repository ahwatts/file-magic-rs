use byteorder::{NativeEndian, WriteBytesExt};
use endian::Endian;
use error::{MagicError, MagicResult};
use num::{Num, Integer, ToPrimitive};
use std::io::{self, Read, Write};
use std::mem;

#[derive(Clone, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum DataType {
    Byte  {                 signed: bool },
    Short { endian: Endian, signed: bool },
    Long  { endian: Endian, signed: bool },
    Quad  { endian: Endian, signed: bool },
    Float(Endian),
    Double(Endian),

    String,

    Name(String),
    Use(String),

    // Id3(Endian),

    // LongDate(Endian, TimeZone),
    // QuadDate(Endian, TimeZone),
    // WindowsDate(Endian),

    // String, PascalString, BigEndianString16, LittleEndianString16,
    // Indirect, Name, Use,
    // Regex, Search,
    // Default, Clear,
}

macro_rules! read_type_to_vec {
    ($celf:ident, $file:ident, $read_mtd:ident, $write_mtd:ident) => {{
        let value = $celf.endian().$read_mtd($file)?;
        let mut vec = Vec::new();
        vec.$write_mtd::<NativeEndian>(value)?;
        Ok(vec)
    }}
}

impl DataType {
    pub fn read<R: Read>(&self, file: &mut R) -> io::Result<Vec<u8>> {
        use self::DataType::*;

        match self {
            &Byte { signed: true } => {
                let value = self.endian().read_i8(file)?;
                let mut vec = Vec::new();
                vec.write_i8(value)?;
                Ok(vec)
            },
            &Byte { signed: false } => {
                let value = self.endian().read_u8(file)?;
                let mut vec = Vec::new();
                vec.write_u8(value)?;
                Ok(vec)
            }

            &Short { signed: true,  .. } => read_type_to_vec!(self, file, read_i16, write_i16),
            &Short { signed: false, .. } => read_type_to_vec!(self, file, read_u16, write_u16),
            &Long  { signed: true,  .. } => read_type_to_vec!(self, file, read_i32, write_i32),
            &Long  { signed: false, .. } => read_type_to_vec!(self, file, read_u32, write_u32),
            &Quad  { signed: true,  .. } => read_type_to_vec!(self, file, read_i64, write_i64),
            &Quad  { signed: false, .. } => read_type_to_vec!(self, file, read_u64, write_u64),

            _ => unimplemented!(),
        }
    }

    pub fn write<N: Num + Integer + ToPrimitive, W: Write>(&self, number: N, file: &mut W) -> io::Result<()> {
        use self::DataType::*;

        match self {
            &Byte { signed: true  } => file.write_i8(number.to_i8().unwrap()),
            &Byte { signed: false } => file.write_u8(number.to_u8().unwrap()),
            &Short { signed: true,  .. } => file.write_i16::<NativeEndian>(number.to_i16().unwrap()),
            &Short { signed: false, .. } => file.write_u16::<NativeEndian>(number.to_u16().unwrap()),
            &Long  { signed: true,  .. } => file.write_i32::<NativeEndian>(number.to_i32().unwrap()),
            &Long  { signed: false, .. } => file.write_u32::<NativeEndian>(number.to_u32().unwrap()),
            &Quad  { signed: true,  .. } => file.write_i64::<NativeEndian>(number.to_i64().unwrap()),
            &Quad  { signed: false, .. } => file.write_u64::<NativeEndian>(number.to_u64().unwrap()),
            _ => unimplemented!(),
        }
    }

    pub fn endian(&self) -> Endian {
        use self::DataType::*;
        use endian::Endian::*;

        match self {
            &Byte  { .. } => Native,
            &Short { endian: e, signed: _ } => e,
            &Long  { endian: e, signed: _ } => e,
            &Quad  { endian: e, signed: _ } => e,
            &Float(e)  => e,
            &Double(e) => e,
            _ => unimplemented!(),
        }
    }
}

pub fn sized_to_byte_vec<T: Sized>(val: T) -> Vec<u8> {
    let length = mem::size_of_val(&val);
    let boxed = Box::new(val);
    let ptr: *mut u8 = Box::into_raw(boxed) as *mut u8;
    unsafe { Vec::from_raw_parts(ptr, length, length) }
}

pub fn byte_vec_to_sized<T: Sized>(val: Vec<u8>) -> MagicResult<T> {
    let length = mem::size_of::<T>();
    if length == val.len() {
        Ok(*unsafe { Box::from_raw(val.as_ptr() as *mut T) })
    } else {
        Err(MagicError::LengthMismatch(val.len(), length))
    }
}
