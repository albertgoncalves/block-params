use std::fmt;

#[derive(Clone)]
pub enum Op {
    Add,
    LessEqual,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Add => write!(f, "+"),
            Self::LessEqual => write!(f, "<="),
        }
    }
}
