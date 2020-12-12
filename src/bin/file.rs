use rmp_serialize;
use file_magic::magic::MagicSet;
use rmp_serialize as rmp;
use rustc_serialize::Decodable;
use std::env;
use std::fs::File;

fn main() {
    let filename = env::args().nth(1).expect("No filename argument.");
    let mut file = File::open(filename).unwrap();

    let mut rules_file = File::open("magic.mgc.mpk").unwrap();
    let mut decoder = rmp::Decoder::new(&mut rules_file);
    let magic = MagicSet::decode(&mut decoder).unwrap();

    println!("{:?}", magic.matches(&mut file));
}
