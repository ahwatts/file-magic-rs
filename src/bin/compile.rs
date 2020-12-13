use file_magic::magic::SerializableMagicSet;
use file_magic::parser;
use std::env;
use std::fs::{write, File};
use std::path::PathBuf;

fn main() {
    let file_path = PathBuf::from(env::args().nth(1).expect("No filename argument."));
    let mut file = File::open(&file_path).unwrap();
    let file_name = file_path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string();

    match parser::parse_set(file_name, &mut file) {
        Ok(set) => {
            println!("{:#?}", set);
            let serializable_set = SerializableMagicSet::from(set);
            let serialized = rmp_serde::to_vec(&serializable_set).unwrap();
            write("magic.mgc.mpk", serialized).unwrap();
        }
        Err(err) => {
            println!("{}", err);
        }
    }
}
