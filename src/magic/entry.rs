use super::offset::Offset;
use super::test::Test;
use super::MatchResult;
use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::io::{Read, Seek};

#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct MagicEntry {
    pub filename: String,
    pub line_num: usize,
    pub level: u32,
    pub offset: Offset,
    pub test: Test,
    pub message: String,
    pub mime_type: Option<String>,
}

impl MagicEntry {
    pub fn matches<F: Read + Seek>(&self, file: &mut F) -> Result<MatchResult> {
        self.offset.seek_to(file)?;
        match self.test.matches(file) {
            Ok(true) => Ok(MatchResult::Matches(self.message.clone())),
            Ok(false) => Ok(MatchResult::NoMatch),
            Err(e) => Err(e),
        }
    }
}
