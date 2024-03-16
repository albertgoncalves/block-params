use crate::ast;
use crate::ir;
use std::collections::{HashMap, HashSet};
use std::fmt;

struct Scope<'a> {
    begin: ir::Label<'a>,
    end: ir::Label<'a>,
}

pub struct Block<'a> {
    pub label: ir::Label<'a>,
    pub insts: Vec<ir::Inst<'a>>,
}

pub struct Blocks<'a>(Vec<Block<'a>>);

pub struct State<'a> {
    pub insts: Vec<ir::Inst<'a>>,
    k: usize,
    stack: Vec<Scope<'a>>,
}

impl fmt::Display for Blocks<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for block in &self.0 {
            writeln!(f, "    {}:", block.label)?;
            for inst in &block.insts {
                writeln!(f, "    {inst}")?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

type Globals<'a> = HashSet<&'a str>;
type Strings<'a> = Vec<&'a str>;
type Locals<'a> = HashMap<&'a str, usize>;

impl<'a> ir::User<'a> {
    fn visited(&self, globals: &Globals<'a>, strings: &mut Strings<'a>) {
        assert!(self.1.is_none());
        if globals.contains(self.0) {
            return;
        }
        strings.push(self.0);
    }

    fn promote(&mut self, globals: &Globals<'a>, locals: &mut Locals<'a>) {
        assert!(self.1.is_none());
        if globals.contains(self.0) {
            return;
        }
        let k = locals.get(self.0).map_or(0, |k| k + 1);
        self.1 = Some(k);
        locals.insert(self.0, k);
    }

    fn catch_up(&mut self, globals: &Globals<'a>, locals: &Locals<'a>) {
        assert!(self.1.is_none());
        if globals.contains(self.0) {
            return;
        }
        let k = locals[self.0];
        self.1 = Some(k);
    }
}

impl<'a> ir::Ident<'a> {
    fn visited(&self, globals: &Globals<'a>, strings: &mut Strings<'a>) {
        match self {
            Self::User(user @ ir::User(_, None)) => user.visited(globals, strings),
            Self::User(..) | Self::Anonymous(..) => unreachable!(),
        }
    }

    fn promote(&mut self, globals: &Globals<'a>, locals: &mut Locals<'a>) {
        match self {
            Self::User(user @ ir::User(_, None)) => user.promote(globals, locals),
            Self::User(..) | Self::Anonymous(..) => unreachable!(),
        }
    }

    fn catch_up(&mut self, globals: &Globals<'a>, locals: &Locals<'a>) {
        match self {
            Self::User(user @ ir::User(_, None)) => user.catch_up(globals, locals),
            Self::User(..) | Self::Anonymous(..) => unreachable!(),
        }
    }
}

impl<'a> ir::Immediate<'a> {
    fn visited(&self, globals: &Globals<'a>, strings: &mut Strings<'a>) {
        if let Self::Ident(ident) = self {
            ident.visited(globals, strings);
        }
    }

    fn catch_up(&mut self, globals: &Globals<'a>, locals: &Locals<'a>) {
        if let Self::Ident(ident) = self {
            ident.catch_up(globals, locals);
        }
    }
}

impl<'a> ir::Value<'a> {
    fn visited(&self, globals: &Globals<'a>, strings: &mut Strings<'a>) {
        match self {
            Self::Immediate(immediate) => immediate.visited(globals, strings),
            Self::BinOp(_, left, right) => {
                left.visited(globals, strings);
                right.visited(globals, strings);
            }
        }
    }

    fn catch_up(&mut self, globals: &Globals<'a>, locals: &Locals<'a>) {
        match self {
            Self::Immediate(immediate) => immediate.catch_up(globals, locals),
            Self::BinOp(_, left, right) => {
                left.catch_up(globals, locals);
                right.catch_up(globals, locals);
            }
        }
    }
}

type Labels<'a> = HashMap<ir::Ident<'a>, Vec<ir::User<'a>>>;

impl<'a> ir::Label<'a> {
    fn visited(&self, globals: &Globals<'a>, strings: &mut Strings<'a>) {
        for user in &self.1 {
            user.visited(globals, strings);
        }
    }

    fn inject(&mut self, labels: &Labels<'a>) -> bool {
        match labels.get(&self.0) {
            Some(idents) => {
                let mut result = false;
                for ident in idents {
                    if !self.1.contains(ident) {
                        self.1.push(ident.clone());
                        result |= true;
                    }
                }
                result
            }
            None => false,
        }
    }

    fn catch_up(&mut self, globals: &Globals<'a>, locals: &Locals<'a>) {
        for user in &mut self.1 {
            user.catch_up(globals, locals);
        }
    }
}

impl<'a> ir::Inst<'a> {
    const fn is_label(&self) -> bool {
        matches!(self, Self::Label(..))
    }

    const fn is_jump(&self) -> bool {
        matches!(self, Self::Jump(..) | Self::Branch(..) | Self::Return(..))
    }

    fn visited(&self, globals: &Globals<'a>, strings: &mut Strings<'a>) {
        match self {
            Self::Label(..) => unreachable!(),
            Self::Let(_, value) | Self::Set(_, value) => value.visited(globals, strings),
            Self::Jump(label) => label.visited(globals, strings),
            Self::Branch(value, r#true, r#false) => {
                value.visited(globals, strings);
                r#true.visited(globals, strings);
                r#false.visited(globals, strings);
            }
            Self::Call(func, args) => {
                func.visited(globals, strings);
                for arg in args {
                    arg.visited(globals, strings);
                }
            }
            Self::Return(Some(immediate)) => immediate.visited(globals, strings),
            Self::Return(None) => (),
        }
    }

    const fn declared(&self) -> Option<&'a str> {
        match self {
            Self::Let(ir::Ident::User(ir::User(string, _)), _) => Some(*string),
            _ => None,
        }
    }

    fn inject(&mut self, labels: &Labels<'a>) -> bool {
        match self {
            Self::Label(..) => unreachable!(),
            Self::Jump(label) => label.inject(labels),
            Self::Branch(_, r#true, r#false) => r#true.inject(labels) | r#false.inject(labels),
            Self::Let(..) | Self::Set(..) | Self::Call(..) | Self::Return(..) => false,
        }
    }

    fn promote_and_catch_up(&mut self, globals: &Globals<'a>, locals: &mut Locals<'a>) {
        match self {
            Self::Label(..) => unreachable!(),
            Self::Let(ident, value) | Self::Set(ident, value) => {
                value.catch_up(globals, locals);
                ident.promote(globals, locals);
                *self = Self::Let(ident.clone(), value.clone());
            }
            Self::Jump(label) => label.catch_up(globals, locals),
            Self::Branch(value, r#true, r#false) => {
                value.catch_up(globals, locals);
                r#true.catch_up(globals, locals);
                r#false.catch_up(globals, locals);
            }
            Self::Call(func, args) => {
                func.catch_up(globals, locals);
                for arg in args {
                    arg.catch_up(globals, locals);
                }
            }
            Self::Return(Some(immediate)) => immediate.catch_up(globals, locals),
            Self::Return(None) => (),
        }
    }
}

impl<'a> From<&[ir::Inst<'a>]> for Block<'a> {
    fn from(insts: &[ir::Inst<'a>]) -> Self {
        let ir::Inst::Label(ref label) = insts[0] else {
            unreachable!();
        };

        let n = insts.len() - 1;
        assert!(insts[n].is_jump());
        for inst in &insts[1..n] {
            assert!(!inst.is_jump());
        }

        Self {
            label: label.clone(),
            insts: insts[1..].to_vec(),
        }
    }
}

impl<'a> Block<'a> {
    fn walk(&mut self, globals: &Globals<'a>) {
        let mut locals = HashSet::new();
        let mut strings: Strings<'a> = vec![];

        for inst in &self.insts {
            inst.visited(globals, &mut strings);
            for string in &strings {
                if locals.contains(string) || self.label.1.contains(&ir::User(string, None)) {
                    continue;
                }
                self.label.1.push(ir::User(string, None));
            }
            if let Some(string) = inst.declared() {
                locals.insert(string);
            }
        }
    }
}

impl<'a> From<&[ir::Inst<'a>]> for Blocks<'a> {
    fn from(insts: &[ir::Inst<'a>]) -> Self {
        let mut blocks = vec![];
        let mut i = 0;

        for (j, inst) in insts.iter().enumerate() {
            if inst.is_label() {
                if i != j {
                    blocks.push(&insts[i..j]);
                }
                i = j;
            }
        }
        assert!(i != insts.len());
        blocks.push(&insts[i..insts.len()]);

        Self(blocks.into_iter().map(Block::from).collect())
    }
}

impl<'a> Blocks<'a> {
    pub fn walk(&mut self, globals: &Globals<'a>) {
        let mut labels: Labels<'a> = HashMap::new();
        let mut repeat = true;
        while repeat {
            for block in &mut self.0 {
                block.walk(globals);
                labels.insert(block.label.0.clone(), block.label.1.clone());
            }
            repeat = false;
            for block in &mut self.0 {
                for inst in &mut block.insts {
                    repeat |= inst.inject(&labels);
                }
            }
        }

        let mut locals: Locals<'a> = HashMap::new();
        for block in &mut self.0 {
            for user in &mut block.label.1 {
                user.promote(globals, &mut locals);
            }
            for inst in &mut block.insts {
                inst.promote_and_catch_up(globals, &mut locals);
            }
        }
    }
}

impl<'a> State<'a> {
    pub const fn new() -> Self {
        Self {
            k: 0,
            insts: vec![],
            stack: vec![],
        }
    }

    fn step_k(&mut self) -> usize {
        let k = self.k;
        self.k += 1;
        k
    }

    fn step_ident(&mut self, anonymous: ir::Anonymous) -> ir::Ident<'a> {
        ir::Ident::Anonymous(anonymous, self.step_k())
    }

    fn step_label(&mut self) -> ir::Label<'a> {
        ir::Label(self.step_ident(ir::Anonymous::Label), vec![])
    }

    fn push_stack(&mut self) -> Scope<'a> {
        let begin = self.step_label();
        let end = self.step_label();
        self.stack.push(Scope {
            begin: begin.clone(),
            end: end.clone(),
        });
        Scope { begin, end }
    }

    fn pop_stack(&mut self) -> Scope<'a> {
        self.stack.pop().unwrap()
    }
}

pub trait IntoInsts<'a> {
    fn into_insts(self, state: &mut State<'a>);
}

pub trait IntoImmediate<'a> {
    fn into_immediate(self, state: &mut State<'a>) -> ir::Immediate<'a>;
}

pub trait IntoValue<'a> {
    fn into_value(self, state: &mut State<'a>) -> ir::Value<'a>;
}

impl<'a> IntoInsts<'a> for &ast::NamedFunc<'a> {
    fn into_insts(self, state: &mut State<'a>) {
        let args = (self.1 .0)
            .iter()
            .map(|string| ir::User(string, None))
            .collect();
        state.insts.push(ir::Inst::Label(ir::Label(
            ir::Ident::User(ir::User(self.0, None)),
            args,
        )));
        self.1 .1.into_insts(state);
    }
}

impl<'a> IntoImmediate<'a> for &ast::Expr<'a> {
    fn into_immediate(self, state: &mut State<'a>) -> ir::Immediate<'a> {
        match self {
            ast::Expr::Int(int) => ir::Immediate::Int(*int),
            ast::Expr::Ident(string) => {
                ir::Immediate::Ident(ir::Ident::User(ir::User(string, None)))
            }
            ast::Expr::BinOp(..) | ast::Expr::Call(..) => {
                let value = self.into_value(state);
                let ident = state.step_ident(ir::Anonymous::Value);
                state.insts.push(ir::Inst::Let(ident.clone(), value));
                ir::Immediate::Ident(ident)
            }
        }
    }
}

impl<'a> IntoValue<'a> for &ast::Expr<'a> {
    fn into_value(self, state: &mut State<'a>) -> ir::Value<'a> {
        match self {
            ast::Expr::Int(..) | ast::Expr::Ident(..) => {
                ir::Value::Immediate(self.into_immediate(state))
            }
            ast::Expr::BinOp(op, exprs) => {
                let left = exprs.0.into_immediate(state);
                let right = exprs.1.into_immediate(state);
                ir::Value::BinOp(op.clone(), left, right)
            }
            ast::Expr::Call(_call) => todo!(),
        }
    }
}

impl<'a> IntoInsts<'a> for &ast::Stmt<'a> {
    fn into_insts(self, state: &mut State<'a>) {
        match self {
            ast::Stmt::Void(call) => {
                let ir::Immediate::Ident(func) = call.0.into_immediate(state) else {
                    todo!()
                };
                let args = call
                    .1
                    .iter()
                    .map(|expr| expr.into_immediate(state))
                    .collect();
                state.insts.push(ir::Inst::Call(func, args));
            }
            ast::Stmt::Let(string, expr) => {
                let value = expr.into_value(state);
                state.insts.push(ir::Inst::Let(
                    ir::Ident::User(ir::User(string, None)),
                    value,
                ));
            }
            ast::Stmt::Set(target, value) => {
                let ir::Immediate::Ident(target) = target.into_immediate(state) else {
                    todo!()
                };
                let value = value.into_value(state);
                state.insts.push(ir::Inst::Set(target, value));
            }
            ast::Stmt::If(expr, scope) => {
                let value = expr.into_value(state);
                let r#true = state.step_label();
                let r#false = state.step_label();
                state
                    .insts
                    .push(ir::Inst::Branch(value, r#true.clone(), r#false.clone()));
                state.insts.push(ir::Inst::Label(r#true));
                scope.into_insts(state);
                state.insts.push(ir::Inst::Label(r#false));
            }
            ast::Stmt::Loop(scope) => {
                let Scope { begin, end: _ } = state.push_stack();
                state.insts.push(ir::Inst::Jump(begin.clone()));
                state.insts.push(ir::Inst::Label(begin));
                scope.into_insts(state);
                let Scope { begin, end } = state.pop_stack();
                state.insts.push(ir::Inst::Jump(begin));
                state.insts.push(ir::Inst::Label(end));
            }
            ast::Stmt::Break => state
                .insts
                .push(ir::Inst::Jump(state.stack.last().unwrap().end.clone())),
            ast::Stmt::Return(expr) => {
                let immediate = expr.as_ref().map(|expr| expr.into_immediate(state));
                state.insts.push(ir::Inst::Return(immediate));
            }
        }
    }
}

impl<'a> IntoInsts<'a> for &ast::Scope<'a> {
    fn into_insts(self, state: &mut State<'a>) {
        for stmt in &(self.0) {
            stmt.into_insts(state);
        }
    }
}
