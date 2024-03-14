use crate::ast;
use crate::ir;

struct Scope<'a> {
    begin: ir::Label<'a>,
    end: ir::Label<'a>,
}

pub struct Block<'a> {
    pub label: ir::Label<'a>,
    pub insts: Vec<ir::Inst<'a>>,
}

pub struct State<'a> {
    pub insts: Vec<ir::Inst<'a>>,
    k: usize,
    stack: Vec<Scope<'a>>,
}

impl<'a> State<'a> {
    pub const fn new() -> Self {
        State {
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

    pub fn split_blocks(&mut self) -> Vec<Block<'a>> {
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

        blocks
            .into_iter()
            .map(|insts| {
                let ir::Inst::Label(ref label) = insts[0] else {
                    unreachable!();
                };
                Block {
                    label: label.clone(),
                    insts: insts[1..].to_vec(),
                }
            })
            .collect()
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
            expr @ (ast::Expr::BinOp(..) | ast::Expr::Call(..)) => {
                let value = expr.into_value(state);
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
            expr @ (ast::Expr::Int(..) | ast::Expr::Ident(..)) => {
                ir::Value::Immediate(expr.into_immediate(state))
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
                state.insts.push(ir::Inst::Label(begin.clone()));
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
