use byteorder::{ByteOrder, BigEndian, LittleEndian, NativeEndian, ReadBytesExt};
use std::io;

#[derive(Clone, Copy, Debug, PartialEq, Eq, RustcEncodable, RustcDecodable)]
pub enum Endian {
    Little,
    Big,
    Native,
    Pdp11,
}

macro_rules! read_bytes_fn {
    ($int_ty:ty, $read_fn:ident) => {
        pub fn $read_fn<R: ReadBytesExt>(&self, input: &mut R) -> io::Result<$int_ty> {
            use self::Endian::*;

            match self {
                &Little => input.$read_fn::<LittleEndian>(),
                &Big    => input.$read_fn::<BigEndian>(),
                &Native => input.$read_fn::<NativeEndian>(),
                &Pdp11  => input.$read_fn::<Pdp11Endian>(),
            }
        }
    }
}

impl Endian {
    pub fn read_u8<R: ReadBytesExt>(&self, input: &mut R) -> io::Result<u8> { input.read_u8() }
    pub fn read_i8<R: ReadBytesExt>(&self, input: &mut R) -> io::Result<i8> { input.read_i8() }

    read_bytes_fn!(u16, read_u16);
    read_bytes_fn!(u32, read_u32);
    read_bytes_fn!(u64, read_u64);

    read_bytes_fn!(i16, read_i16);
    read_bytes_fn!(i32, read_i32);
    read_bytes_fn!(i64, read_i64);

    read_bytes_fn!(f32, read_f32);
    read_bytes_fn!(f64, read_f64);
}

pub enum Pdp11Endian {}

impl ByteOrder for Pdp11Endian {
    fn read_u16(buf: &[u8]) -> u16 {
        LittleEndian::read_u16(buf)
    }

    fn read_u32(buf: &[u8]) -> u32 {
        let low_word = BigEndian::read_u16(buf) as u32;
        let high_word = BigEndian::read_u16(&buf[2..]) as u32;
        (high_word << 16) + low_word
    }

    fn read_u64(buf: &[u8]) -> u64 {
        LittleEndian::read_u64(buf)
    }

    fn read_uint(buf: &[u8], nbytes: usize) -> u64 {
        LittleEndian::read_uint(buf, nbytes)
    }

    fn write_u16(buf: &mut [u8], n: u16) {
        LittleEndian::write_u16(buf, n)
    }

    fn write_u32(buf: &mut [u8], n: u32) {
        let high_word = (n >> 16) as u16;
        let low_word = (n % 65536) as u16;
        BigEndian::write_u16(buf, low_word);
        BigEndian::write_u16(&mut buf[2..], high_word);
    }

    fn write_u64(buf: &mut [u8], n: u64) {
        LittleEndian::write_u64(buf, n)
    }

    fn write_uint(buf: &mut [u8], n: u64, nbytes: usize) {
        LittleEndian::write_uint(buf, n, nbytes)
    }
}

#[cfg(test)]
mod tests {
    use byteorder::ByteOrder;
    use super::Pdp11Endian;

    #[test]
    fn pdp11_u32() {
        let buf: [u8; 4] = [ 2, 1, 4, 3 ];
        assert_eq!(0x04030201, Pdp11Endian::read_u32(&buf));

        let mut buf: [u8; 4] = [ 0, 0, 0, 0 ];
        Pdp11Endian::write_u32(&mut buf, 0x04030201);
        assert_eq!([ 2, 1, 4, 3 ], buf);
    }

    #[test]
    fn pdp11_i32() {
        let buf: [u8; 4] = [ 0xFD, 0xFF, 0xFB, 0xFC ];
        assert_eq!(-0x04030201, Pdp11Endian::read_i32(&buf));

        let mut buf: [u8; 4] = [ 0, 0, 0, 0 ];
        Pdp11Endian::write_i32(&mut buf, -0x04030201);
        assert_eq!([ 0xFD, 0xFF, 0xFB, 0xFC ], buf);
    }
}
