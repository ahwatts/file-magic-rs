/* use std::io;

pub type MagicResult<T> = Result<T, MagicError>;

quick_error! {
    #[derive(Debug)]
    pub enum MagicError {
        Io(err: io::Error) {
            cause(err)
            description(err.description())
            from()
        }

        Parse(desc: String) {
            description(desc)
        }

        LengthMismatch(expected: usize, actual: usize) {
            description("Type length mismatch")
            display("Type length mismatch: expected {} bytes, got {} bytes", expected, actual)
        }
    }
}
*/
