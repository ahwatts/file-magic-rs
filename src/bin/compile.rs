extern crate file_magic;
extern crate nom;

use file_magic::parser;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

fn main() {
    let filename = env::args().skip(1).next().expect("No filename argument.");
    let mut file = File::open(filename).unwrap();
    let mut content = String::new();
    file.read_to_string(&mut content).unwrap();

    println!("parsing {} bytes...", content.as_bytes().len());
    let result = parser::parse(content.as_bytes());
    match result {
        IResult::Done(rest, output) =>  println!("Done: output = {:#?} left over = {}", output, rest.len()),
        IResult::Error(error) => println!("Error: error = {:?}", error),
        IResult::Incomplete(needed) => println!("Incomplete: needed = {:?}", needed),
    }
}
