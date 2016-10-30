extern crate file_magic;

use file_magic::parser;
use std::env;
use std::fs::File;

fn main() {
    let filename = env::args().skip(1).next().expect("No filename argument.");
    let mut file = File::open(filename).unwrap();

    // let result = parser::parse(&mut file);
    match parser::parse(&mut file) {
        Ok(entries) => {
            for entry in entries.iter() {
                println!("{:?}", entry);
            }
        },
        Err(err) => {
            println!("{}", err);
        }
    }
}
