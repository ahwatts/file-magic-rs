// use file_magic::parser;
use std::env;
use std::fs::File;
use std::path::PathBuf;

fn main() {
    let file_path = PathBuf::from(env::args().nth(1).expect("No filename argument."));
    let mut _file = File::open(&file_path).unwrap();
    let _file_name = file_path.file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("")
        .to_string();

    /* match parser::parse_set(file_name, &mut file) {
        Ok(set) => {
            println!("{:#?}", set);
            let mut output_file = File::create("magic.mgc.mpk").unwrap();
            let mut encoder = rmp::Encoder::new(&mut output_file);
            set.encode(&mut encoder).unwrap();
        },
        Err(err) => {
            println!("{}", err);
        }
    } */
}
