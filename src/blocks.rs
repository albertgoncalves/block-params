use crate::ast;
use crate::ir;
use crate::op;
use std::collections::{HashMap, HashSet};
use std::fmt;

pub struct State<'a> {
    insts: Vec<ir::Inst<'a>>,
    k: usize,
    stack: Vec<Scope<'a>>,
    globals: HashSet<&'a str>,
}

struct Scope<'a> {
    begin: ir::Label<'a>,
    end: ir::Label<'a>,
}

#[derive(Clone)]
struct Block<'a>(ir::Label<'a>, Vec<ir::Inst<'a>>);

pub struct Blocks<'a>(Vec<Block<'a>>);

impl fmt::Display for Blocks<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for block in &self.0 {
            writeln!(f, "    {}:", block.0)?;
            for inst in &block.1 {
                writeln!(f, "    {inst}")?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

const fn make_immediate_expr_int<'a>(int: i64) -> ir::Immediate<'a> {
    ir::Immediate::Int(int)
}

const fn make_ident_expr_ident(ident: &str) -> ir::Ident<'_> {
    ir::Ident::Name(ir::Name(ident, None))
}

impl<'a> State<'a> {
    pub fn new(globals: HashSet<&'a str>) -> Self {
        Self {
            k: 0,
            insts: vec![],
            stack: vec![],
            globals,
        }
    }

    fn step_k(&mut self) -> usize {
        let k = self.k;
        self.k += 1;
        k
    }

    fn step_ident_value(&mut self) -> ir::Ident<'a> {
        ir::Ident::Anonymous(ir::Anonymous::Value(self.step_k()))
    }

    fn step_ident_label(&mut self) -> ir::Ident<'a> {
        ir::Ident::Anonymous(ir::Anonymous::Label(self.step_k()))
    }

    fn step_label(&mut self) -> ir::Label<'a> {
        ir::Label(self.step_ident_label(), vec![])
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

    fn make_ident_expr_call(&mut self, expr: &ast::Expr<'a>) -> ir::Ident<'a> {
        let value = self.make_value_expr(expr);
        let ident = self.step_ident_value();
        self.insts.push(ir::Inst::Let(ident.clone(), value));
        ident
    }

    fn make_ident_expr(&mut self, expr: &ast::Expr<'a>) -> ir::Ident<'a> {
        match expr {
            ast::Expr::Int(..) => unreachable!(),
            ast::Expr::Ident(ident) => make_ident_expr_ident(ident),
            ast::Expr::BinOp(..) | ast::Expr::Call(..) => self.make_ident_expr_call(expr),
        }
    }

    fn make_immediate_expr(&mut self, expr: &ast::Expr<'a>) -> ir::Immediate<'a> {
        match expr {
            ast::Expr::Int(int) => make_immediate_expr_int(*int),
            ast::Expr::Ident(ident) => ir::Immediate::Ident(make_ident_expr_ident(ident)),
            ast::Expr::BinOp(..) | ast::Expr::Call(..) => {
                ir::Immediate::Ident(self.make_ident_expr_call(expr))
            }
        }
    }

    fn make_value_expr(&mut self, expr: &ast::Expr<'a>) -> ir::Value<'a> {
        match expr {
            ast::Expr::Int(int) => ir::Value::Immediate(make_immediate_expr_int(*int)),
            ast::Expr::Ident(ident) => {
                ir::Value::Immediate(ir::Immediate::Ident(make_ident_expr_ident(ident)))
            }
            ast::Expr::BinOp(op, exprs) => {
                let left = self.make_immediate_expr(&exprs.0);
                let right = self.make_immediate_expr(&exprs.1);
                ir::Value::BinOp(op.clone(), left, right)
            }
            ast::Expr::Call(_call) => todo!(),
        }
    }

    fn push_insts_stmt(&mut self, stmt: &ast::Stmt<'a>) {
        match stmt {
            ast::Stmt::Void(call) => {
                let func = self.make_ident_expr(&call.0);
                let args = call
                    .1
                    .iter()
                    .map(|expr| self.make_immediate_expr(expr))
                    .collect();
                self.insts.push(ir::Inst::Void(ir::Value::Call(func, args)));
            }
            ast::Stmt::Let(ident, expr) => {
                let value = self.make_value_expr(expr);
                self.insts
                    .push(ir::Inst::Let(ir::Ident::Name(ir::Name(ident, None)), value));
            }
            ast::Stmt::Set(target, value) => {
                let target = self.make_ident_expr(target);
                let value = self.make_value_expr(value);
                self.insts.push(ir::Inst::Set(target, value));
            }
            ast::Stmt::If(expr, scope) => {
                let value = self.make_value_expr(expr);
                let r#true = self.step_label();
                let r#false = self.step_label();
                self.insts
                    .push(ir::Inst::Branch(value, r#true.clone(), r#false.clone()));
                self.insts.push(ir::Inst::Label(r#true));
                self.push_insts_scope(scope);
                self.insts.push(ir::Inst::Label(r#false));
            }
            ast::Stmt::Loop(scope) => {
                let Scope { begin, end: _ } = self.push_stack();
                self.insts.push(ir::Inst::Jump(begin.clone()));
                self.insts.push(ir::Inst::Label(begin));
                self.push_insts_scope(scope);
                let Scope { begin, end } = self.pop_stack();
                self.insts.push(ir::Inst::Jump(begin));
                self.insts.push(ir::Inst::Label(end));
            }
            ast::Stmt::Break => self
                .insts
                .push(ir::Inst::Jump(self.stack.last().unwrap().end.clone())),
            ast::Stmt::Return(expr) => {
                let immediate = expr.as_ref().map(|expr| self.make_immediate_expr(expr));
                self.insts.push(ir::Inst::Return(immediate));
            }
        }
    }

    fn push_insts_scope(&mut self, scope: &ast::Scope<'a>) {
        for stmt in &scope.0 {
            self.push_insts_stmt(stmt);
        }
    }

    pub fn push_insts_named_func(&mut self, name_func: &ast::NamedFunc<'a>) {
        let args = (name_func.1 .0)
            .iter()
            .map(|name| ir::Name(name, None))
            .collect();
        self.insts.push(ir::Inst::Label(ir::Label(
            ir::Ident::Name(ir::Name(name_func.0, None)),
            args,
        )));
        self.push_insts_scope(&name_func.1 .1);
    }

    fn push_labels_blocks(&self, blocks: &mut Blocks<'a>) {
        let mut labels = HashMap::new();
        let mut repeat = true;

        while repeat {
            for block in &mut blocks.0 {
                let mut locals: HashSet<&ir::Name<'a>> = HashSet::new();
                for inst in &mut block.1 {
                    let mut names = vec![];
                    ir::push_names_inst(&mut names, inst);
                    for name in names {
                        assert!(name.1.is_none());
                        if locals.contains(&name) {
                            continue;
                        }
                        if block.0 .1.contains(&name) || self.globals.contains(name.0) {
                            continue;
                        }
                        block.0 .1.push(name);
                    }
                    if let ir::Inst::Let(ir::Ident::Name(name), _) = inst {
                        locals.insert(name);
                    }
                }
                labels.insert(block.0 .0.clone(), block.0 .1.clone());
            }

            repeat = false;
            for block in &mut blocks.0 {
                for inst in &mut block.1 {
                    match inst {
                        ir::Inst::Jump(label) => {
                            push_labels_label(label, &labels, &mut repeat);
                        }
                        ir::Inst::Branch(_, r#true, r#false) => {
                            push_labels_label(r#true, &labels, &mut repeat);
                            push_labels_label(r#false, &labels, &mut repeat);
                        }
                        _ => (),
                    }
                }
            }
        }
    }

    fn number_idents(&self, blocks: &mut Blocks<'a>) {
        let mut numbers = HashMap::new();
        for block in &mut blocks.0 {
            for name in &mut block.0 .1 {
                assert!(!self.globals.contains(name.0));
                let k = numbers.get(name.0).map_or(0, |k| *k + 1);
                numbers.insert(name.0, k);
                name.1 = Some(k);
            }
            for inst in &mut block.1 {
                if let ir::Inst::Set(ident, value) = inst {
                    *inst = ir::Inst::Let(ident.clone(), value.clone());
                }

                let mut names = vec![];
                ir::push_mut_names_inst(&mut names, inst);
                for name in names {
                    if self.globals.contains(name.0) {
                        continue;
                    }
                    name.1 = Some(numbers[name.0]);
                }
                if let ir::Inst::Let(ir::Ident::Name(name), _) = inst {
                    let k = numbers.get(name.0).map_or(0, |k| *k + 1);
                    numbers.insert(name.0, k);
                    name.1 = Some(k);
                }
            }
        }
    }

    fn check_blocks(&self, blocks: &Blocks<'a>) {
        for block in &blocks.0 {
            let mut locals = HashSet::new();
            for name in &block.0 .1 {
                assert!(locals.insert(ir::Ident::Name(name.clone())));
            }
            for inst in &block.1 {
                let mut idents = vec![];
                ir::push_idents_inst(&mut idents, inst);
                for ident in idents {
                    match ident {
                        ir::Ident::Name(ir::Name(name, None)) if self.globals.contains(name) => {
                            continue;
                        }
                        ir::Ident::Anonymous(ir::Anonymous::Label(_)) => continue,
                        _ => (),
                    }
                    assert!(locals.contains(&ident));
                }
                if let ir::Inst::Let(ident, _) = inst {
                    locals.insert(ident.clone());
                }
            }
        }
    }
}

fn push_labels_label<'a>(
    label: &mut ir::Label<'a>,
    labels: &HashMap<ir::Ident<'a>, Vec<ir::Name<'a>>>,
    repeat: &mut bool,
) {
    for name in &labels[&label.0] {
        if label.1.contains(name) {
            continue;
        }
        label.1.push(name.clone());
        *repeat = true;
    }
}

const fn is_jump(inst: &ir::Inst<'_>) -> bool {
    matches!(
        inst,
        ir::Inst::Jump(..) | ir::Inst::Branch(..) | ir::Inst::Return(..),
    )
}

impl<'a> From<&[ir::Inst<'a>]> for Block<'a> {
    fn from(insts: &[ir::Inst<'a>]) -> Self {
        let ir::Inst::Label(ref label) = insts[0] else {
            unreachable!();
        };

        let n = insts.len() - 1;
        assert!(is_jump(&insts[n]));
        for inst in &insts[1..n] {
            assert!(!is_jump(inst));
        }

        Self(label.clone(), insts[1..].to_vec())
    }
}

impl<'a> From<&[ir::Inst<'a>]> for Blocks<'a> {
    fn from(insts: &[ir::Inst<'a>]) -> Self {
        let mut blocks = vec![];
        let mut i = 0;

        for (j, inst) in insts.iter().enumerate() {
            if matches!(inst, ir::Inst::Label(..)) {
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

fn optimize(blocks: &mut Blocks<'_>) {
    loop {
        let mut count = 0;
        count += coalesce_jumps(blocks);
        for block in &mut blocks.0 {
            count += const_fold_block(block);
        }
        count += remove_unused_idents_blocks(blocks);
        if count == 0 {
            break;
        }
    }
}

impl<'a> From<&mut State<'a>> for Blocks<'a> {
    fn from(state: &mut State<'a>) -> Self {
        let mut blocks = Blocks::from(&state.insts[..]);
        state.push_labels_blocks(&mut blocks);
        state.number_idents(&mut blocks);
        optimize(&mut blocks);
        state.check_blocks(&blocks);
        blocks
    }
}

fn coalesce_jumps(blocks: &mut Blocks<'_>) -> usize {
    let mut idents = HashMap::new();
    for block in &blocks.0 {
        let n = block.1.len();
        assert!(0 < n);
        if block.1.len() != 1 {
            continue;
        }
        if let ir::Inst::Jump(label) = &block.1[0] {
            idents.insert(block.0 .0.clone(), label.0.clone());
        }
    }
    if idents.is_empty() {
        return 0;
    }

    let mut indices = vec![];
    for (i, block) in blocks.0.iter_mut().enumerate() {
        if idents.contains_key(&block.0 .0) {
            indices.push(i);
            continue;
        }
        for inst in &mut block.1 {
            match inst {
                ir::Inst::Jump(label) => {
                    if let Some(ident) = idents.get(&label.0) {
                        label.0 = ident.clone();
                    }
                }
                ir::Inst::Branch(_, r#true, r#false) => {
                    if let Some(ident) = idents.get(&r#true.0) {
                        r#true.0 = ident.clone();
                    }
                    if let Some(ident) = idents.get(&r#false.0) {
                        r#false.0 = ident.clone();
                    }
                }
                _ => (),
            }
        }
    }
    remove_indices(&mut blocks.0, &indices[..])
}

fn const_fold_value(value: &mut ir::Value<'_>) -> usize {
    if let ir::Value::BinOp(op::Op::Add, ir::Immediate::Int(left), ir::Immediate::Int(right)) =
        value
    {
        *value = ir::Value::Immediate(ir::Immediate::Int(*left + *right));
        1
    } else {
        0
    }
}

fn const_fold_block(block: &mut Block<'_>) -> usize {
    let mut consts = HashMap::new();
    let mut count = 0;
    for inst in &mut block.1 {
        if let ir::Inst::Let(_, value) = inst {
            count += const_fold_value(value);
        }
        if let ir::Inst::Let(ident, ir::Value::Immediate(immediate)) = inst {
            consts.insert(ident.clone(), immediate.clone());
        }
        let mut immediates = vec![];
        ir::push_mut_immediates_inst(&mut immediates, inst);
        for immediate in immediates {
            if let ir::Immediate::Ident(ident) = immediate {
                if let Some(replacement) = consts.get(ident) {
                    *immediate = replacement.clone();
                    count += 1;
                }
            }
        }
    }
    count
}

fn remove_unused_idents_blocks(blocks: &mut Blocks<'_>) -> usize {
    let mut reads_name: HashSet<ir::Name> = HashSet::new();
    for block in &blocks.0 {
        for inst in &block.1 {
            let mut names = vec![];
            ir::push_names_inst(&mut names, inst);
            for name in names {
                reads_name.insert(name);
            }
        }
    }

    let mut count = 0;

    let mut labels = HashMap::new();
    for block in &mut blocks.0 {
        {
            let mut indices = vec![];
            for (i, name) in block.0 .1.iter().enumerate() {
                if !reads_name.contains(name) {
                    indices.push(i);
                }
            }
            count += remove_indices(&mut block.0 .1, &indices[..]);
        }
        {
            let mut indices = vec![];
            for (i, inst) in block.1.iter().enumerate() {
                if let ir::Inst::Let(ir::Ident::Name(name), _) = inst {
                    if !reads_name.contains(name) {
                        indices.push(i);
                    }
                }
            }
            count += remove_indices(&mut block.1, &indices[..]);
        }
        labels.insert(block.0 .0.clone(), block.0 .1.clone());
    }

    for block in &mut blocks.0 {
        for inst in &mut block.1 {
            match inst {
                ir::Inst::Jump(label) => count += remove_unused_idents_label(label, &labels),
                ir::Inst::Branch(_, r#true, r#false) => {
                    count += remove_unused_idents_label(r#true, &labels);
                    count += remove_unused_idents_label(r#false, &labels);
                }
                _ => (),
            }
        }
    }

    count
}

fn remove_unused_idents_label<'a>(
    label: &mut ir::Label<'a>,
    labels: &HashMap<ir::Ident<'a>, Vec<ir::Name<'a>>>,
) -> usize {
    let mut indices = vec![];
    for (i, name0) in label.1.iter().enumerate() {
        let mut found = false;
        for name1 in &labels[&label.0] {
            if name0.0 == name1.0 {
                found = true;
                break;
            }
        }
        if !found {
            indices.push(i);
        }
    }
    remove_indices(&mut label.1, &indices[..])
}

fn remove_indices<T>(vec: &mut Vec<T>, indices: &[usize]) -> usize {
    let count = indices.len();
    for i in indices.iter().rev() {
        // NOTE: This could get very slow on large inputs.
        vec.remove(*i);
    }
    count
}
