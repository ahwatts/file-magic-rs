#![allow(dead_code)]

use error::{MagicError, MagicResult};
use std::iter::Peekable;

pub use self::entry::*;

mod entry;

#[derive(Clone, Debug, RustcEncodable, RustcDecodable)]
pub struct MagicSet {
    filename: String,
    lists: Vec<MagicList>,
}

impl MagicSet {
    pub fn new(filename: String) -> MagicSet {
        MagicSet {
            filename: filename,
            lists: Vec::new(),
        }
    }

    pub fn add_entries(&mut self, mut entries: Vec<MagicEntry>) -> MagicResult<()> {
        let mut entry_iter = entries.drain(..).peekable();

        while let Some(entry) = entry_iter.next() {
            if entry.level > 0 {
                return Err(MagicError::Parse(format!("Root magic entry does not have a level of 0: {:?}", entry)));
            } else {
                let mut list = MagicList {
                    filename: self.filename.clone(),
                    root: entry,
                    children: Vec::new(),
                };

                try!(list.add_entries(&mut entry_iter));

                self.lists.push(list);
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, RustcEncodable, RustcDecodable)]
struct MagicList {
    filename: String,
    root: MagicEntry,
    children: Vec<MagicList>,
}

impl MagicList {
    fn add_entries<I: Iterator<Item = MagicEntry>>(&mut self, mut entries: &mut Peekable<I>) -> MagicResult<()> {
        loop {
            let opt_level = entries.peek().map(|e| e.level);

            match opt_level {
                Some(level) => {
                    if level <= self.root.level {
                        return Ok(());
                    } else if level == self.root.level + 1 {
                        let mut list = MagicList {
                            filename: self.filename.clone(),
                            root: entries.next().unwrap(),
                            children: Vec::new(),
                        };
                        try!(list.add_entries(&mut entries));
                        self.children.push(list);
                    } else {
                        return Err(MagicError::Parse(format!(
                            "Level too deep for magic entry! {} > {} + 1",
                            level, self.root.level)));
                    }
                },
                None => break,
            }

        }

        Ok(())
    }
}
