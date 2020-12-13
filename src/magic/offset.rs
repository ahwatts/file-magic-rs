use crate::data_type::{self, DataType};
use std::io::{self, Read, Seek, SeekFrom};
use serde::{Deserialize, Serialize};

// 123     123 bytes from the start
// &123    123 bytes from here
// (123)   (the value at 123 bytes from the start) bytes from the start
// (&123)  (the value at 123 bytes from here) bytes from the start
// &(123)  (the value at 123 bytes from the start) bytes from here
// &(&123) (the value at 123 bytes from here) bytes from here
#[derive(Clone, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum Offset {
    Direct(DirectOffset),
    AbsoluteIndirect(IndirectOffset),
    RelativeIndirect(IndirectOffset),
}

impl Offset {
    pub fn absolute(val: u64) -> Offset {
        Offset::Direct(DirectOffset::Absolute(val))
    }

    pub fn relative(val: i64) -> Offset {
        Offset::Direct(DirectOffset::Relative(val))
    }

    pub fn direct(base: DirectOffset) -> Offset {
        Offset::Direct(base)
    }

    pub fn absolute_indirect(base: IndirectOffset) -> Offset {
        Offset::AbsoluteIndirect(base)
    }

    pub fn relative_indirect(base: IndirectOffset) -> Offset {
        Offset::RelativeIndirect(base)
    }

    pub fn seek_to<F: Read + Seek>(&self, file: &mut F) -> io::Result<u64> {
        let direct = match self {
            Offset::Direct(off) => *off,
            Offset::AbsoluteIndirect(indirect) => {
                DirectOffset::Absolute(indirect.read_absolute_offset(file)?)
            },
            Offset::RelativeIndirect(indirect) => {
                DirectOffset::Relative(indirect.read_relative_offset(file)?)
            },
        };
        direct.seek_to(file)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum DirectOffset {
    Absolute(u64),
    Relative(i64),
}

impl DirectOffset {
    pub fn absolute(val: u64) -> DirectOffset {
        DirectOffset::Absolute(val)
    }

    pub fn relative(val: i64) -> DirectOffset {
        DirectOffset::Relative(val)
    }

    pub fn seek_to<F: Seek>(&self, file: &mut F) -> io::Result<u64> {
        match self {
            DirectOffset::Absolute(off) => file.seek(SeekFrom::Start(*off)),
            DirectOffset::Relative(off) => file.seek(SeekFrom::Current(*off)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct IndirectOffset {
    pub base: DirectOffset,
    pub data_type: DataType,
    pub bias: i64,
}

impl IndirectOffset {
    pub fn read_absolute_offset<F: Read + Seek>(&self, file: &mut F) -> io::Result<u64> {
        let bytes = self.read_offset_as_vec(file)?;
        data_type::byte_vec_to_sized::<u64>(bytes)
            .map(|o| (o as i64 + self.bias) as u64)
            .map_err(|e| {
                io::Error::new(io::ErrorKind::Other, e)
            })
    }

    pub fn read_relative_offset<F: Read + Seek>(&self, file: &mut F) -> io::Result<i64> {
        let bytes = self.read_offset_as_vec(file)?;
        data_type::byte_vec_to_sized::<i64>(bytes)
            .map(|o| o + self.bias)
            .map_err(|e| {
                io::Error::new(io::ErrorKind::Other, e)
            })
    }

    fn read_offset_as_vec<F: Read + Seek>(&self, file: &mut F) -> io::Result<Vec<u8>> {
        self.base.seek_to(file)?;
        self.data_type.read(file)
    }
}

#[cfg(test)]
mod tests {
    use std::iter;
    use std::io::Cursor;
    use super::*;

    #[test]
    fn absolute_direct_offset_seek_to() {
        let file: Vec<u8> = iter::repeat(0u8).take(1024).collect();
        let mut opened = Cursor::new(file);
        DirectOffset::Absolute(40).seek_to(&mut opened).unwrap();
        assert_eq!(40, opened.position());

        Offset::Direct(DirectOffset::Absolute(1000)).seek_to(&mut opened).unwrap();
        assert_eq!(1000, opened.position());
    }

    #[test]
    fn relative_direct_offset_seek_to() {
        let file: Vec<u8> = iter::repeat(0u8).take(1024).collect();
        let mut opened = Cursor::new(file);
        opened.seek(SeekFrom::Start(40)).unwrap();
        DirectOffset::Relative(-10).seek_to(&mut opened).unwrap();
        assert_eq!(30, opened.position());

        Offset::Direct(DirectOffset::Relative(500)).seek_to(&mut opened).unwrap();
        assert_eq!(530, opened.position());
    }
}
