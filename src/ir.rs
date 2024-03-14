use crate::op::Op;
use crate::prelude::write_delim;
use std::fmt;

#[derive(Clone)]
pub struct Ident<'a>(pub &'a str, pub Option<usize>);

#[derive(Clone)]
pub enum Immediate<'a> {
    Int(i64),
    Ident(Ident<'a>),
}

#[derive(Clone)]
pub enum Value<'a> {
    Immediate(Immediate<'a>),
    BinOp(Op, Immediate<'a>, Immediate<'a>),
}

#[derive(Clone)]
pub struct Label<'a>(pub Ident<'a>, pub Vec<Ident<'a>>);

#[derive(Clone)]
pub enum Inst<'a> {
    Label(Label<'a>),
    Let(Ident<'a>, Value<'a>),
    Set(Ident<'a>, Value<'a>),
    Jump(Label<'a>),
    Branch(Value<'a>, Label<'a>, Label<'a>),
    Call(Ident<'a>, Vec<Immediate<'a>>),
    Return(Option<Immediate<'a>>),
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.1 {
            None => write!(f, "{}", self.0),
            Some(k) => write!(f, "{}{}", self.0, k),
        }
    }
}

impl fmt::Display for Immediate<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Ident(ident) => write!(f, "{ident}"),
        }
    }
}

impl fmt::Display for Value<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Immediate(immediate) => write!(f, "{immediate}"),
            Self::BinOp(op, left, right) => write!(f, "{left} {op} {right}"),
        }
    }
}

impl fmt::Display for Label<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}(", self.0)?;
        write_delim(f, &self.1, ", ")?;
        write!(f, ")")
    }
}

impl fmt::Display for Inst<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Label(label) => write!(f, "{label}:"),
            Self::Let(ident, value) => write!(f, "    {ident} := {value}"),
            Self::Set(ident, value) => write!(f, "    {ident} = {value}"),
            Self::Jump(label) => write!(f, "    jump {label}"),
            Self::Branch(value, r#true, r#false) => {
                write!(f, "    branch {value} ? {true} : {false}")
            }
            Self::Call(ident, args) => {
                write!(f, "    call {ident}(")?;
                write_delim(f, args, ", ")?;
                write!(f, ")")
            }
            Self::Return(Some(immediate)) => write!(f, "    return({immediate})"),
            Self::Return(None) => write!(f, "    return()"),
        }
    }
}

impl Inst<'_> {
    pub const fn is_label(&self) -> bool {
        matches!(self, Inst::Label(..))
    }
}