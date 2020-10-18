use std::{fmt, result, str, error as std_error};
use std::io::{Result, Read, Error, ErrorKind};

pub trait CharsExt {
    fn chars_iterator(self) -> Chars<Self> where Self: Sized {
        Chars { inner: self }
    }
}

impl<T: Read> CharsExt for T {

}

#[derive(Debug, Clone)]
#[must_use = "iterators are lazy and do nothing unless consumed"]
pub struct Chars<R> {
    inner: R,
}

/// An enumeration of possible errors that can be generated from the `Chars`
/// adapter.
#[derive(Debug)]
pub enum CharsError {
    /// Variant representing that the underlying stream was read successfully
    /// but it did not contain valid utf8 data.
    NotUtf8,

    /// Variant representing that an I/O error occurred.
    Other(Error),
}

impl<R: Read> Iterator for Chars<R> {
    type Item = result::Result<char, CharsError>;

    fn next(&mut self) -> Option<result::Result<char, CharsError>> {
        let first_byte = match read_one_byte(&mut self.inner)? {
            Ok(b) => b,
            Err(e) => return Some(Err(CharsError::Other(e))),
        };
        let width = utf8_char_width(first_byte);
        if width == 1 { return Some(Ok(first_byte as char)) }
        if width == 0 { return Some(Err(CharsError::NotUtf8)) }
        let mut buf = [first_byte, 0, 0, 0];
        {
            let mut start = 1;
            while start < width {
                match self.inner.read(&mut buf[start..width]) {
                    Ok(0) => return Some(Err(CharsError::NotUtf8)),
                    Ok(n) => start += n,
                    Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                    Err(e) => return Some(Err(CharsError::Other(e))),
                }
            }
        }
        Some(match str::from_utf8(&buf[..width]).ok() {
            Some(s) => Ok(s.chars().next().unwrap()),
            None => Err(CharsError::NotUtf8),
        })
    }
}

impl std_error::Error for CharsError {
    fn description(&self) -> &str {
        match *self {
            CharsError::NotUtf8 => "invalid utf8 encoding",
            #[allow(deprecated)]
            CharsError::Other(ref e) => std_error::Error::description(e),
        }
    }
    fn cause(&self) -> Option<&dyn std_error::Error> {
        match *self {
            CharsError::NotUtf8 => None,
            CharsError::Other(ref e) => e.source(),
        }
    }
}

impl fmt::Display for CharsError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            CharsError::NotUtf8 => {
                "byte stream did not contain valid utf8".fmt(f)
            }
            CharsError::Other(ref e) => e.fmt(f),
        }
    }
}

fn read_one_byte(reader: &mut dyn Read) -> Option<Result<u8>> {
    let mut buf = [0];
    loop {
        match reader.read(&mut buf) {
            Ok(0) => break None,
            Ok(..) => break Some(Ok(buf[0])),
            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => break Some(Err(e)),
        };
    }
}

/// Given a first byte, determines how many bytes are in this UTF-8 character.
#[inline]
pub fn utf8_char_width(b: u8) -> usize {
    match b {
        0..=0x7f => 1,
        0xc2..=0xdf => 2,
        0xe0..=0xef => 3,
        0xf0..=0xf4 => 4,
        0x80..=0xc1 | 0xf5..=0xff => 0,
    }
}

#[cfg(test)]
mod tests {
    use super::CharsExt;

    use std::io::Cursor;

    // https://tools.ietf.org/html/rfc3629
    static UTF8_CHAR_WIDTH: [u8; 256] = [
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x1F
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x3F
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x5F
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, // 0x7F
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0x9F
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, // 0xBF
        0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
        2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2, // 0xDF
        3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3, // 0xEF
        4,4,4,4,4,0,0,0,0,0,0,0,0,0,0,0, // 0xFF
    ];

    #[test]
    pub fn char_width() {
        for i in 0..=255 {
            assert_eq!(super::utf8_char_width(i) as u8, UTF8_CHAR_WIDTH[i as usize]);
        }
    }

    #[test]
    fn chars_iterator_works() {
        let s = "ħɛlɭoʊ, Ï'ˀm̃ c̈₀⁰ɫ".to_owned();

        let c = Cursor::new(s.clone().into_bytes());

        let v: Vec<char> = c.chars_iterator().flat_map(Result::ok).collect();
        let v2: Vec<char> = s.chars().collect();
        assert_eq!(v, v2);
    }
}