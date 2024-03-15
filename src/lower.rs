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

type Globals<'a, 'b> = &'b HashSet<&'a str>;
type Locals<'a, 'b> = &'b mut Vec<ir::Ident<'a>>;

impl<'a> ir::Ident<'a> {
    fn used(&self, globals: Globals<'a, '_>, idents: Locals<'a, '_>) {
        if globals.contains(self.0) {
            return;
        }
        idents.push(self.clone());
    }

    fn promote(&mut self, globals: Globals<'a, '_>, locals: &mut HashMap<&'a str, usize>) {
        if globals.contains(self.0) {
            return;
        }
        assert!(self.1.is_none());
        let k = locals.get(&self.0).map_or(0, |k| k + 1);
        self.1 = Some(k);
        locals.insert(self.0, k);
    }

    fn catch_up(&mut self, globals: Globals<'a, '_>, locals: &HashMap<&'a str, usize>) {
        if globals.contains(self.0) {
            return;
        }
        let k = locals[&self.0];
        self.1 = Some(k);
    }
}

impl<'a> ir::Immediate<'a> {
    fn used(&self, globals: Globals<'a, '_>, idents: Locals<'a, '_>) {
        if let Self::Ident(ident) = self {
            ident.used(globals, idents);
        }
    }

    fn catch_up(&mut self, globals: Globals<'a, '_>, locals: &HashMap<&'a str, usize>) {
        if let ir::Immediate::Ident(ident) = self {
            ident.catch_up(globals, locals);
        }
    }
}

impl<'a> ir::Value<'a> {
    fn used(&self, globals: Globals<'a, '_>, idents: Locals<'a, '_>) {
        match self {
            Self::Immediate(immediate) => immediate.used(globals, idents),
            Self::BinOp(_, left, right) => {
                left.used(globals, idents);
                right.used(globals, idents);
            }
        }
    }

    fn catch_up(&mut self, globals: Globals<'a, '_>, locals: &HashMap<&'a str, usize>) {
        match self {
            Self::Immediate(immediate) => immediate.catch_up(globals, locals),
            Self::BinOp(_, left, right) => {
                left.catch_up(globals, locals);
                right.catch_up(globals, locals);
            }
        }
    }
}

impl<'a> ir::Label<'a> {
    fn used(&self, globals: Globals<'a, '_>, idents: Locals<'a, '_>) {
        for ident in &self.1 {
            ident.used(globals, idents);
        }
    }

    fn inject(&mut self, labels: &HashMap<ir::Ident<'a>, Vec<ir::Ident<'a>>>) -> bool {
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

    fn catch_up(&mut self, globals: Globals<'a, '_>, locals: &HashMap<&'a str, usize>) {
        for ident in &mut self.1 {
            ident.catch_up(globals, locals);
        }
    }
}

impl<'a> ir::Inst<'a> {
    const fn is_label(&self) -> bool {
        matches!(self, Self::Label(..))
    }

    fn used(&self, globals: Globals<'a, '_>, idents: Locals<'a, '_>) {
        match self {
            Self::Label(..) => unreachable!(),
            Self::Let(_, value) | Self::Set(_, value) => value.used(globals, idents),
            Self::Jump(label) => label.used(globals, idents),
            Self::Branch(value, r#true, r#false) => {
                value.used(globals, idents);
                r#true.used(globals, idents);
                r#false.used(globals, idents);
            }
            Self::Call(func, args) => {
                func.used(globals, idents);
                for arg in args {
                    arg.used(globals, idents);
                }
            }
            Self::Return(Some(immediate)) => immediate.used(globals, idents),
            Self::Return(None) => (),
        }
    }

    fn declared(&self) -> Option<ir::Ident<'a>> {
        if let Self::Let(ident, _) = self {
            Some(ident.clone())
        } else {
            None
        }
    }

    fn inject(&mut self, labels: &HashMap<ir::Ident<'a>, Vec<ir::Ident<'a>>>) -> bool {
        match self {
            Self::Label(..) => unreachable!(),
            Self::Jump(label) => label.inject(labels),
            Self::Branch(_, r#true, r#false) => r#true.inject(labels) | r#false.inject(labels),
            Self::Let(..) | Self::Set(..) | Self::Call(..) | Self::Return(..) => false,
        }
    }

    fn promote_and_catch_up(
        &mut self,
        globals: Globals<'a, '_>,
        locals: &mut HashMap<&'a str, usize>,
    ) {
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
        Self {
            label: label.clone(),
            insts: insts[1..].to_vec(),
        }
    }
}

impl<'a> Block<'a> {
    fn walk(&mut self, globals: Globals<'a, '_>) {
        let mut locals = HashSet::new();
        let mut used = vec![];

        for inst in &self.insts {
            inst.used(globals, &mut used);
            for ident in &used {
                if locals.contains(ident) || self.label.1.contains(ident) {
                    continue;
                }
                self.label.1.push(ident.clone());
            }
            if let Some(ident) = inst.declared() {
                locals.insert(ident.clone());
            }
        }
    }
}

impl<'a> Blocks<'a> {
    pub fn walk(&mut self, globals: Globals<'a, '_>) {
        let mut labels = HashMap::new();
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

        let mut locals = HashMap::new();
        for block in &mut self.0 {
            for ident in &mut block.label.1 {
                ident.promote(globals, &mut locals);
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

    fn step_ident(&mut self) -> ir::Ident<'a> {
        ir::Ident("_", Some(self.step_k()))
    }

    fn step_label(&mut self) -> ir::Label<'a> {
        ir::Label(self.step_ident(), vec![])
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

    pub fn split_blocks(&mut self) -> Blocks<'a> {
        let mut blocks = vec![];
        let mut i = 0;

        for (j, inst) in self.insts.iter().enumerate() {
            if inst.is_label() {
                if i != j {
                    blocks.push(&self.insts[i..j]);
                }
                i = j;
            }
        }
        blocks.push(&self.insts[i..self.insts.len()]);

        Blocks(blocks.into_iter().map(Block::from).collect())
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
            .map(|ident| ir::Ident(ident, None))
            .collect();
        state
            .insts
            .push(ir::Inst::Label(ir::Label(ir::Ident(self.0, None), args)));
        self.1 .1.into_insts(state);
    }
}

impl<'a> IntoImmediate<'a> for &ast::Expr<'a> {
    fn into_immediate(self, state: &mut State<'a>) -> ir::Immediate<'a> {
        match self {
            ast::Expr::Int(int) => ir::Immediate::Int(*int),
            ast::Expr::Ident(ident) => ir::Immediate::Ident(ir::Ident(ident, None)),
            ast::Expr::BinOp(..) | ast::Expr::Call(..) => {
                let value = self.into_value(state);
                let ident = state.step_ident();
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
            ast::Stmt::Let(ident, expr) => {
                let value = expr.into_value(state);
                state
                    .insts
                    .push(ir::Inst::Let(ir::Ident(ident, None), value));
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
