use std::fmt;

pub fn write_delim<A, B>(f: &mut fmt::Formatter, items: &[A], delim: B) -> fmt::Result
where
    A: fmt::Display,
    B: fmt::Display,
{
    let mut items = items.iter();
    if let Some(item) = items.next() {
        write!(f, "{item}")?;
        for item in items {
            write!(f, "{delim}{item}")?;
        }
    }
    fmt::Result::Ok(())
}
