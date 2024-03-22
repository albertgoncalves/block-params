use crate::op::Op;
use crate::prelude::write_delim;
use std::fmt;

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Named<'a>(pub &'a str, pub Option<usize>);

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Anonymous {
    Value(usize),
    Label(usize),
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Ident<'a> {
    Named(Named<'a>),
    Anonymous(Anonymous),
}

#[derive(Clone)]
pub enum Immediate<'a> {
    Int(i64),
    Ident(Ident<'a>),
}

#[derive(Clone)]
pub enum Value<'a> {
    Immediate(Immediate<'a>),
    BinOp(Op, Immediate<'a>, Immediate<'a>),
    Call(Ident<'a>, Vec<Immediate<'a>>),
}

#[derive(Clone)]
pub struct Label<'a>(pub Ident<'a>, pub Vec<Named<'a>>);

#[derive(Clone)]
pub enum Inst<'a> {
    Label(Label<'a>),
    Let(Ident<'a>, Value<'a>),
    Set(Ident<'a>, Value<'a>),
    Void(Value<'a>),
    Jump(Label<'a>),
    Branch(Value<'a>, Label<'a>, Label<'a>),
    Return(Option<Immediate<'a>>),
}

pub struct MutReads<'a, 'b> {
    pub named: Vec<&'b mut Named<'a>>,
    pub anonymous: Vec<&'b mut Anonymous>,
}

pub struct Idents<'a>(pub Vec<Ident<'a>>);

impl fmt::Display for Named<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self(named, Some(k)) => write!(f, "{named}.{k}"),
            Self(named, None) => write!(f, "{named}"),
        }
    }
}

impl fmt::Display for Anonymous {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Value(k) => write!(f, "__value__{k}"),
            Self::Label(k) => write!(f, "__label__{k}"),
        }
    }
}

impl fmt::Display for Ident<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Named(named) => write!(f, "{named}"),
            Self::Anonymous(anonymous) => write!(f, "{anonymous}"),
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
            Self::Call(ident, args) => {
                write!(f, "call {ident}(")?;
                write_delim(f, args, ", ")?;
                write!(f, ")")
            }
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
            Self::Void(value) => write!(f, "    {value}"),
            Self::Jump(label) => write!(f, "    jump {label}"),
            Self::Branch(value, r#true, r#false) => {
                write!(f, "    branch {value} ? {true} : {false}")
            }
            Self::Return(Some(immediate)) => write!(f, "    return({immediate})"),
            Self::Return(None) => write!(f, "    return()"),
        }
    }
}

impl<'a, 'b> MutReads<'a, 'b> {
    pub fn new() -> Self {
        Self {
            named: vec![],
            anonymous: vec![],
        }
    }

    fn push_mut_reads_ident(&mut self, ident: &'b mut Ident<'a>) {
        match ident {
            Ident::Named(named) => self.named.push(named),
            Ident::Anonymous(anonymous) => self.anonymous.push(anonymous),
        };
    }

    fn push_mut_reads_immediate(&mut self, immediate: &'b mut Immediate<'a>) {
        match immediate {
            Immediate::Int(..) => (),
            Immediate::Ident(ident) => self.push_mut_reads_ident(ident),
        }
    }

    fn push_mut_reads_value(&mut self, value: &'b mut Value<'a>) {
        match value {
            Value::Immediate(immediate) => self.push_mut_reads_immediate(immediate),
            Value::BinOp(_, left, right) => {
                self.push_mut_reads_immediate(left);
                self.push_mut_reads_immediate(right);
            }
            Value::Call(func, args) => {
                self.push_mut_reads_ident(func);
                for arg in args {
                    self.push_mut_reads_immediate(arg);
                }
            }
        }
    }

    fn push_mut_reads_label(&mut self, label: &'b mut Label<'a>) {
        self.push_mut_reads_ident(&mut label.0);
        for named in &mut label.1 {
            self.named.push(named);
        }
    }

    pub fn push_mut_reads_inst(&mut self, inst: &'b mut Inst<'a>) {
        match inst {
            Inst::Label(..) => unreachable!(),
            Inst::Let(_, value) => {
                self.push_mut_reads_value(value);
            }
            Inst::Set(ident, value) => {
                self.push_mut_reads_value(value);
                self.push_mut_reads_ident(ident);
            }
            Inst::Void(value) => self.push_mut_reads_value(value),
            Inst::Jump(label) => self.push_mut_reads_label(label),
            Inst::Branch(value, r#true, r#false) => {
                self.push_mut_reads_value(value);
                self.push_mut_reads_label(r#true);
                self.push_mut_reads_label(r#false);
            }
            Inst::Return(Some(immediate)) => self.push_mut_reads_immediate(immediate),
            Inst::Return(None) => (),
        }
    }
}

impl<'a> Idents<'a> {
    pub const fn new() -> Self {
        Self(vec![])
    }

    fn push_idents_ident(&mut self, ident: &Ident<'a>) {
        self.0.push(ident.clone());
    }

    fn push_idents_immediate(&mut self, immediate: &Immediate<'a>) {
        match immediate {
            Immediate::Int(..) => (),
            Immediate::Ident(ident) => self.push_idents_ident(ident),
        }
    }

    fn push_idents_value(&mut self, value: &Value<'a>) {
        match value {
            Value::Immediate(immediate) => self.push_idents_immediate(immediate),
            Value::BinOp(_, left, right) => {
                self.push_idents_immediate(left);
                self.push_idents_immediate(right);
            }
            Value::Call(func, args) => {
                self.push_idents_ident(func);
                for arg in args {
                    self.push_idents_immediate(arg);
                }
            }
        }
    }

    fn push_idents_label(&mut self, label: &Label<'a>) {
        self.push_idents_ident(&label.0);
        for named in &label.1 {
            self.0.push(Ident::Named(named.clone()));
        }
    }

    pub fn push_idents_inst(&mut self, inst: &Inst<'a>) {
        match inst {
            Inst::Label(..) | Inst::Set(..) => unreachable!(),
            Inst::Let(_, value) => {
                self.push_idents_value(value);
            }
            Inst::Void(value) => self.push_idents_value(value),
            Inst::Jump(label) => self.push_idents_label(label),
            Inst::Branch(value, r#true, r#false) => {
                self.push_idents_value(value);
                self.push_idents_label(r#true);
                self.push_idents_label(r#false);
            }
            Inst::Return(Some(immediate)) => self.push_idents_immediate(immediate),
            Inst::Return(None) => (),
        }
    }
}
