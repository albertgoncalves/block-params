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
                ast::Stmt::Let("y", ast::Expr::Int(0)),
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

    let mut state = blocks::State::new();
    named_func.push_insts(&mut state);

    let globals = HashSet::from(["console.log"]);

    let mut blocks = blocks::Blocks::from(&state.insts[..]);
    blocks.walk(&globals);
    println!("{blocks}");
}
