extern crate byteorder;
extern crate combine;
extern crate num;
extern crate rmp_serialize;
extern crate rustc_serialize;

#[macro_use] extern crate quick_error;

pub mod data_type;
pub mod endian;
pub mod from_bytes;
pub mod error;
pub mod magic;
pub mod parser;
