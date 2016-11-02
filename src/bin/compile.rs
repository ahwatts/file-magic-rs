extern crate file_magic;
extern crate rmp_serialize;
extern crate rustc_serialize;

use file_magic::parser;
use std::env;
use std::fs::File;
use rmp_serialize as rmp;
use rustc_serialize::Encodable;

fn main() {
    let filename = env::args().skip(1).next().expect("No filename argument.");
    let mut file = File::open(filename).unwrap();

    match parser::parse(&mut file) {
        Ok(entries) => {
            for entry in entries.iter() {
                println!("{:?}", entry);
            }

            let mut output_file = File::create("magic.mgc.mpk").unwrap();
            let mut encoder = rmp::Encoder::new(&mut output_file);
            entries.encode(&mut encoder).unwrap();
        },
        Err(err) => {
            println!("{}", err);
        }
    }
}
