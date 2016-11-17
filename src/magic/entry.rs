use std::io::{Read, Seek};
use super::MatchResult;
use super::offset::Offset;
use super::test::Test;
use error::MagicResult;

#[derive(Clone, Debug, PartialEq, RustcEncodable, RustcDecodable)]
pub struct MagicEntry {
    pub filename: String,
    pub line_num: usize,
    pub level: u32,
    pub offset: Offset,
    pub test: Test,
    pub message: String,
}

impl MagicEntry {
    pub fn matches<F: Read + Seek>(&self, file: &mut F) -> MagicResult<MatchResult> {
        try!(self.offset.seek_to(file));
        match self.test.matches(file) {
            Ok(true) => Ok(MatchResult::Matches(self.message.clone())),
            Ok(false) => Ok(MatchResult::NoMatch),
            Err(e) => Err(e),
        }
    }
}
