mod ast;
mod blocks;
mod ir;
mod op;
mod prelude;

use std::collections::HashSet;

fn main() {
    let named_func = ast::NamedFunc(
        "f",
        ast::Func(
            vec![],
            ast::Scope(vec![
                ast::Stmt::Let("x", ast::Expr::Int(0)),
                ast::Stmt::Let("z", ast::Expr::Int(-1)),
                ast::Stmt::Let(
                    "y",
                    ast::Expr::BinOp(
                        op::Op::Add,
                        Box::new((ast::Expr::Ident("z"), ast::Expr::Int(1))),
                    ),
                ),
                ast::Stmt::Loop(ast::Scope(vec![
                    ast::Stmt::If(
                        ast::Expr::BinOp(
                            op::Op::LessEqual,
                            Box::new((ast::Expr::Int(10), ast::Expr::Ident("x"))),
                        ),
                        ast::Scope(vec![ast::Stmt::Break]),
                    ),
                    ast::Stmt::Set(
                        ast::Expr::Ident("y"),
                        ast::Expr::BinOp(
                            op::Op::Add,
                            Box::new((ast::Expr::Ident("x"), ast::Expr::Ident("y"))),
                        ),
                    ),
                    ast::Stmt::Set(
                        ast::Expr::Ident("x"),
                        ast::Expr::BinOp(
                            op::Op::Add,
                            Box::new((ast::Expr::Ident("x"), ast::Expr::Int(1))),
                        ),
                    ),
                ])),
                ast::Stmt::Void(ast::Call(
                    ast::Expr::Ident("console.log"),
                    vec![ast::Expr::Ident("y")],
                )),
                ast::Stmt::Return(None),
            ]),
        ),
    );
    println!("{named_func}\n");

    let mut state = blocks::State::new(HashSet::from(["console.log"]));
    state.push_insts_named_func(&named_func);

    let blocks = blocks::Blocks::from(&mut state);
    print!("{blocks}");
}
