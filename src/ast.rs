use crate::op::Op;
use crate::prelude::write_delim;
use std::fmt;

pub struct Call<'a>(pub Expr<'a>, pub Vec<Expr<'a>>);

pub struct Func<'a>(pub Vec<&'a str>, pub Scope<'a>);

pub struct NamedFunc<'a>(pub &'a str, pub Func<'a>);

pub enum Expr<'a> {
    Int(i64),
    Ident(&'a str),
    BinOp(Op, Box<(Expr<'a>, Expr<'a>)>),
    Call(Box<Call<'a>>),
}

pub enum Stmt<'a> {
    Void(Call<'a>),
    Let(&'a str, Expr<'a>),
    Set(Expr<'a>, Expr<'a>),
    If(Expr<'a>, Scope<'a>),
    Loop(Scope<'a>),
    Break,
    Return(Option<Expr<'a>>),
}

pub struct Scope<'a>(pub Vec<Stmt<'a>>);

impl Call<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        let func = &self.0;
        let args = &self.1;
        write!(f, "(")?;
        func.display(f, pad)?;
        write!(f, ")(")?;
        let mut args = args.iter();
        if let Some(arg) = args.next() {
            arg.display(f, pad)?;
            for arg in args {
                write!(f, ", ")?;
                arg.display(f, pad)?;
            }
        }
        write!(f, ")")
    }
}

impl Func<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        write!(f, "function(")?;
        write_delim(f, &self.0, ", ")?;
        write!(f, ") ")?;
        self.1.display(f, pad)
    }
}

impl NamedFunc<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        write!(f, "function {}(", self.0)?;
        write_delim(f, &self.1 .0, ", ")?;
        write!(f, ") ")?;
        self.1 .1.display(f, pad)
    }
}

impl Expr<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        match self {
            Expr::Int(int) => write!(f, "{int}"),
            Expr::Ident(ident) => write!(f, "{ident}"),
            Expr::BinOp(op, exprs) => {
                let left = &exprs.0;
                let right = &exprs.1;
                write!(f, "(")?;
                left.display(f, pad)?;
                write!(f, " {op} ")?;
                right.display(f, pad)?;
                write!(f, ")")
            }
            Expr::Call(call) => call.display(f, pad),
        }
    }
}

impl Stmt<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        match self {
            Stmt::Void(call) => {
                call.display(f, pad)?;
                write!(f, ";")
            }
            Stmt::Let(ident, expr) => {
                write!(f, "var {ident} = ")?;
                expr.display(f, pad)?;
                write!(f, ";")
            }
            Stmt::Set(target, value) => {
                target.display(f, pad)?;
                write!(f, " = ")?;
                value.display(f, pad)?;
                write!(f, ";")
            }
            Stmt::If(expr, scope) => {
                write!(f, "if (")?;
                expr.display(f, pad)?;
                write!(f, ") ")?;
                scope.display(f, pad)
            }
            Stmt::Loop(scope) => {
                write!(f, "for (;;) ")?;
                scope.display(f, pad)
            }
            Stmt::Break => write!(f, "break;"),
            Stmt::Return(Some(expr)) => {
                write!(f, "return ")?;
                expr.display(f, pad)?;
                write!(f, ";")
            }
            Stmt::Return(None) => write!(f, "return;"),
        }
    }
}

impl Scope<'_> {
    fn display(&self, f: &mut fmt::Formatter, pad: usize) -> fmt::Result {
        writeln!(f, "{{")?;
        {
            let pad = pad + 4;
            for stmt in &self.0 {
                write!(f, "{:pad$}", "")?;
                stmt.display(f, pad)?;
                writeln!(f)?;
            }
        }
        write!(f, "{:pad$}}}", "")
    }
}

macro_rules! impl_displays {
    ($ident:ident) => {
        impl fmt::Display for $ident<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                self.display(f, 0)
            }
        }
    };
    ($ident:ident, $($rest:tt)*) => {
        impl_displays!($ident);
        impl_displays!($($rest)*);
    };
    () => {}
}

impl_displays!(Call, Func, NamedFunc, Expr, Stmt, Scope);
