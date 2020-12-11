use data_type::DataType;
use error::{MagicError, MagicResult};
use std::collections::HashMap;
use std::iter::Peekable;
use std::io::{Read, Seek};
use std::rc::Rc;

pub use self::entry::*;
pub use self::offset::*;
pub use self::test::*;

mod entry;
mod offset;
mod test;

#[derive(Clone, Debug, RustcEncodable, RustcDecodable)]
pub struct MagicSet {
    filename: String,
    lists: Vec<Rc<MagicList>>,
    named: HashMap<String, Rc<MagicList>>,
}

impl MagicSet {
    pub fn new(filename: String) -> MagicSet {
        MagicSet {
            filename,
            lists: Vec::new(),
            named: HashMap::new(),
        }
    }

    pub fn add_entries(&mut self, mut entries: Vec<MagicEntry>) -> MagicResult<()> {
        let mut entry_iter = entries.drain(..).peekable();

        while let Some(entry) = entry_iter.next() {
            if entry.level > 0 {
                return Err(MagicError::Parse(format!("Root magic entry does not have a level of 0: {:?}", entry)));
            } else {
                let opt_name = if let DataType::Name(name) = entry.test.data_type() {
                    Some(name.clone())
                } else {
                    None
                };

                let mut list = MagicList {
                    filename: self.filename.clone(),
                    root: entry,
                    children: Vec::new(),
                };

                list.add_entries(&mut entry_iter)?;

                let list_owner = Rc::new(list);
                self.lists.push(list_owner.clone());

                if let Some(name) = opt_name {
                    self.named.insert(name, list_owner.clone());
                }
            }
        }

        Ok(())
    }

    pub fn matches<F: Read + Seek>(&self, file: &mut F) -> MagicResult<MatchResult> {
        use self::MatchResult::*;

        for list in self.lists.iter() {
            match list.matches(file) {
                v @ Ok(Matches(..)) => return v,
                Ok(NoMatch) => {},
                v @ Err(..) => return v,
            }
        }

        Ok(NoMatch)
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
                        list.add_entries(&mut entries)?;
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

    pub fn matches<F: Read + Seek>(&self, file: &mut F) -> MagicResult<MatchResult> {
        use self::MatchResult::*;

        let root_match = self.root.matches(file)?;
        match root_match {
            Matches(root_message) => {
                let mut message = Vec::new();
                message.push(root_message);
                for entry in self.children.iter() {
                    if let Matches(child_message) = entry.matches(file)? {
                        message.push(child_message);
                    }
                }
                Ok(Matches(message.join(", ")))
            },
            NoMatch => Ok(NoMatch),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MatchResult {
    Matches(String),
    NoMatch,
}
