use file_magic::magic::{MagicSet, SerializableMagicSet};
use std::env;
use std::fs::File;

fn main() {
    let filename = env::args().nth(1).expect("No filename argument.");
    let mut file = File::open(filename).unwrap();

    let rules_file = File::open("magic.mgc.mpk").unwrap();
    let magic =
        MagicSet::from(rmp_serde::from_read::<_, SerializableMagicSet>(rules_file).unwrap());

    println!("{:?}", magic.matches(&mut file));
}
