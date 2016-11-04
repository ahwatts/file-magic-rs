extern crate file_magic;
extern crate rmp_serialize;
extern crate rustc_serialize;

use file_magic::parser;
use std::env;
use std::fs::File;
use rmp_serialize as rmp;
use rustc_serialize::Encodable;
use std::path::PathBuf;

fn main() {
    let file_path = PathBuf::from(env::args().skip(1).next().expect("No filename argument."));
    let mut file = File::open(&file_path).unwrap();
    let file_name = file_path.file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string();

    match parser::parse_set(file_name, &mut file) {
        Ok(set) => {
            println!("{:#?}", set);
            let mut output_file = File::create("magic.mgc.mpk").unwrap();
            let mut encoder = rmp::Encoder::new(&mut output_file);
            set.encode(&mut encoder).unwrap();
        },
        Err(err) => {
            println!("{}", err);
        }
    }
}
