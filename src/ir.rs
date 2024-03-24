use crate::op::Op;
use crate::prelude::write_delim;
use std::fmt;

#[derive(Clone, Eq, Hash, PartialEq)]
pub struct Name<'a>(pub &'a str, pub Option<usize>);

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Anonymous {
    Value(usize),
    Label(usize),
}

#[derive(Clone, Eq, Hash, PartialEq)]
pub enum Ident<'a> {
    Name(Name<'a>),
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
pub struct Label<'a>(pub Ident<'a>, pub Vec<Name<'a>>);

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

impl fmt::Display for Name<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self(name, Some(k)) => write!(f, "{name}.{k}"),
            Self(name, None) => write!(f, "{name}"),
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
            Self::Name(name) => write!(f, "{name}"),
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

fn push_mut_names_ident<'a, 'b>(vec: &mut Vec<&'b mut Name<'a>>, ident: &'b mut Ident<'a>) {
    if let Ident::Name(name) = ident {
        vec.push(name);
    }
}

fn push_mut_names_immediate<'a, 'b>(
    vec: &mut Vec<&'b mut Name<'a>>,
    immediate: &'b mut Immediate<'a>,
) {
    match immediate {
        Immediate::Int(..) => (),
        Immediate::Ident(ident) => push_mut_names_ident(vec, ident),
    }
}

fn push_mut_names_value<'a, 'b>(vec: &mut Vec<&'b mut Name<'a>>, value: &'b mut Value<'a>) {
    match value {
        Value::Immediate(immediate) => push_mut_names_immediate(vec, immediate),
        Value::BinOp(_, left, right) => {
            push_mut_names_immediate(vec, left);
            push_mut_names_immediate(vec, right);
        }
        Value::Call(func, args) => {
            push_mut_names_ident(vec, func);
            for arg in args {
                push_mut_names_immediate(vec, arg);
            }
        }
    }
}

fn push_mut_names_label<'a, 'b>(vec: &mut Vec<&'b mut Name<'a>>, label: &'b mut Label<'a>) {
    push_mut_names_ident(vec, &mut label.0);
    for name in &mut label.1 {
        vec.push(name);
    }
}

pub fn push_mut_names_inst<'a, 'b>(vec: &mut Vec<&'b mut Name<'a>>, inst: &'b mut Inst<'a>) {
    match inst {
        Inst::Label(..) => unreachable!(),
        Inst::Let(_, value) => {
            push_mut_names_value(vec, value);
        }
        Inst::Set(ident, value) => {
            push_mut_names_value(vec, value);
            push_mut_names_ident(vec, ident);
        }
        Inst::Void(value) => push_mut_names_value(vec, value),
        Inst::Jump(label) => push_mut_names_label(vec, label),
        Inst::Branch(value, r#true, r#false) => {
            push_mut_names_value(vec, value);
            push_mut_names_label(vec, r#true);
            push_mut_names_label(vec, r#false);
        }
        Inst::Return(Some(immediate)) => push_mut_names_immediate(vec, immediate),
        Inst::Return(None) => (),
    }
}

fn push_names_ident<'a>(vec: &mut Vec<Name<'a>>, ident: &Ident<'a>) {
    if let Ident::Name(name) = ident {
        vec.push(name.clone());
    }
}

fn push_names_immediate<'a>(vec: &mut Vec<Name<'a>>, immediate: &Immediate<'a>) {
    match immediate {
        Immediate::Int(..) => (),
        Immediate::Ident(ident) => push_names_ident(vec, ident),
    }
}

fn push_names_value<'a>(vec: &mut Vec<Name<'a>>, value: &Value<'a>) {
    match value {
        Value::Immediate(immediate) => push_names_immediate(vec, immediate),
        Value::BinOp(_, left, right) => {
            push_names_immediate(vec, left);
            push_names_immediate(vec, right);
        }
        Value::Call(func, args) => {
            push_names_ident(vec, func);
            for arg in args {
                push_names_immediate(vec, arg);
            }
        }
    }
}

fn push_names_label<'a>(vec: &mut Vec<Name<'a>>, label: &Label<'a>) {
    push_names_ident(vec, &label.0);
    for name in &label.1 {
        vec.push(name.clone());
    }
}

pub fn push_names_inst<'a>(vec: &mut Vec<Name<'a>>, inst: &Inst<'a>) {
    match inst {
        Inst::Label(..) => unreachable!(),
        Inst::Let(_, value) => {
            push_names_value(vec, value);
        }
        Inst::Set(ident, value) => {
            push_names_value(vec, value);
            push_names_ident(vec, ident);
        }
        Inst::Void(value) => push_names_value(vec, value),
        Inst::Jump(label) => push_names_label(vec, label),
        Inst::Branch(value, r#true, r#false) => {
            push_names_value(vec, value);
            push_names_label(vec, r#true);
            push_names_label(vec, r#false);
        }
        Inst::Return(Some(immediate)) => push_names_immediate(vec, immediate),
        Inst::Return(None) => (),
    }
}

fn push_idents_ident<'a>(vec: &mut Vec<Ident<'a>>, ident: &Ident<'a>) {
    vec.push(ident.clone());
}

fn push_idents_immediate<'a>(vec: &mut Vec<Ident<'a>>, immediate: &Immediate<'a>) {
    match immediate {
        Immediate::Int(..) => (),
        Immediate::Ident(ident) => push_idents_ident(vec, ident),
    }
}

fn push_idents_value<'a>(vec: &mut Vec<Ident<'a>>, value: &Value<'a>) {
    match value {
        Value::Immediate(immediate) => push_idents_immediate(vec, immediate),
        Value::BinOp(_, left, right) => {
            push_idents_immediate(vec, left);
            push_idents_immediate(vec, right);
        }
        Value::Call(func, args) => {
            push_idents_ident(vec, func);
            for arg in args {
                push_idents_immediate(vec, arg);
            }
        }
    }
}

fn push_idents_label<'a>(vec: &mut Vec<Ident<'a>>, label: &Label<'a>) {
    push_idents_ident(vec, &label.0);
    for name in &label.1 {
        vec.push(Ident::Name(name.clone()));
    }
}

pub fn push_idents_inst<'a>(vec: &mut Vec<Ident<'a>>, inst: &Inst<'a>) {
    match inst {
        Inst::Label(..) | Inst::Set(..) => unreachable!(),
        Inst::Let(_, value) => {
            push_idents_value(vec, value);
        }
        Inst::Void(value) => push_idents_value(vec, value),
        Inst::Jump(label) => push_idents_label(vec, label),
        Inst::Branch(value, r#true, r#false) => {
            push_idents_value(vec, value);
            push_idents_label(vec, r#true);
            push_idents_label(vec, r#false);
        }
        Inst::Return(Some(immediate)) => push_idents_immediate(vec, immediate),
        Inst::Return(None) => (),
    }
}

fn push_mut_immediates_immediate<'a, 'b>(
    vec: &mut Vec<&'b mut Immediate<'a>>,
    immediate: &'b mut Immediate<'a>,
) {
    vec.push(immediate);
}

fn push_mut_immediates_value<'a, 'b>(
    vec: &mut Vec<&'b mut Immediate<'a>>,
    value: &'b mut Value<'a>,
) {
    match value {
        Value::Immediate(immediate) => push_mut_immediates_immediate(vec, immediate),
        Value::BinOp(_, left, right) => {
            push_mut_immediates_immediate(vec, left);
            push_mut_immediates_immediate(vec, right);
        }
        Value::Call(_, args) => {
            for arg in args {
                push_mut_immediates_immediate(vec, arg);
            }
        }
    }
}

pub fn push_mut_immediates_inst<'a, 'b>(
    vec: &mut Vec<&'b mut Immediate<'a>>,
    inst: &'b mut Inst<'a>,
) {
    match inst {
        Inst::Label(..) | Inst::Set(..) => unreachable!(),
        Inst::Let(_, value) | Inst::Void(value) => push_mut_immediates_value(vec, value),
        Inst::Return(Some(immediate)) => push_mut_immediates_immediate(vec, immediate),
        Inst::Jump(..) | Inst::Branch(..) | Inst::Return(None) => (),
    }
}
