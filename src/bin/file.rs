extern crate file_magic;
extern crate rmp_serialize;
extern crate rustc_serialize;

use file_magic::magic::MagicSet;
use rmp_serialize as rmp;
use rustc_serialize::Decodable;
use std::env;
use std::fs::File;

fn main() {
    let filename = env::args().skip(1).next().expect("No filename argument.");
    let mut file = File::open(filename).unwrap();

    let mut rules_file = File::open("magic.mgc.mpk").unwrap();
    let mut decoder = rmp::Decoder::new(&mut rules_file);
    let magic = MagicSet::decode(&mut decoder).unwrap();

    println!("magic = {:?}", magic);

    // for rule in rules.iter() {
    //     println!("rule {:?}", rule);
    //     println!("match? {:?}", rule.matches(&mut file));
    //     println!("");
    // }
}
