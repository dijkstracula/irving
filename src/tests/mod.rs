mod extraction;
mod parser;
mod passes;
mod programs;
mod stdlib;
mod typechecker;

#[cfg(test)]
#[ctor::ctor]
fn init() {
    let _ = env_logger::builder()
        .is_test(true)
        .filter_level(log::LevelFilter::Debug)
        .try_init();
}

#[cfg(test)]
pub mod helpers {
    use crate::{
        ast::{
            actions::Action,
            declarations::Decl,
            expressions::{self, Expr},
            logic::{self, Fmla},
            span::Span,
            statements::Stmt,
            toplevels::Prog,
        },
        parser::ivy::{IvyParser, Rule},
        passes::global_lowerer::GlobalLowerer,
        stdlib::load_stdlib,
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    pub fn prog_from_filename(path: &str) -> Prog {
        let prog = std::fs::read_to_string(path).unwrap();
        prog_from_decls(&prog)
    }

    pub fn prog_from_decls(prog: &str) -> Prog {
        let res = IvyParser::parse_with_userdata(Rule::prog, prog, prog.to_string().into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::prog(res).expect("AST generation failed")
    }

    pub fn process_from_decl(prog: &str) -> Decl {
        let res = IvyParser::parse_with_userdata(Rule::process_decl, prog, prog.to_owned().into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let (span, decl) = IvyParser::process_decl(res).expect("AST generation failed");
        Decl::Object { span, decl }
    }

    pub fn module_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse_with_userdata(Rule::module_decl, prog, prog.to_owned().into())
            .expect("Parsing failed")
            .single()
            .unwrap();

        let (span, decl) = IvyParser::module_decl(res).expect("AST generation failed");
        Decl::Module { span, decl }
    }

    #[allow(dead_code)]
    pub fn vector_moduledecl() -> Decl {
        module_from_src(
            "module vector(range) = { 
            type this

            action get(a:this,x:unbounded_sequence) returns (y:range)
            action set(a:this,x:unbounded_sequence,y:range) returns (a:this)
            action empty returns (a: this)
            action append(a: this, x: range) returns (y: this)
        }",
        )
    }

    pub fn tcp_moduledecl() -> Decl {
        module_from_src(
            "module tcp = {
                type endpoint = bv[32]
                module net(msg) = {
                    module socket = {
                        var id: unbounded_sequence

                        action send(dest: endpoint, m: msg)
                        action recv(src: endpoint, m: msg)
                    }
                }
            }",
        )
    }

    pub fn typeinference_from_filename(path: &str) -> Prog {
        let text = std::fs::read_to_string(path).unwrap();
        let res = IvyParser::parse_with_userdata(Rule::prog, &text, text.to_owned().into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut prog = IvyParser::prog(res).expect("AST generation failed");

        let mut gl = GlobalLowerer::new();

        // Fake out the "standard library"
        let mut tc = load_stdlib().unwrap();

        prog.visit(&mut gl)
            .expect("lowering globals")
            .modifying(&mut prog);

        prog.visit(&mut tc)
            .expect("type inference")
            .modifying(&mut prog);

        prog
    }

    pub fn decl_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse_with_userdata(Rule::decl, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect("AST generation failed")
    }

    pub fn stmt_from_src(prog: &str) -> Stmt {
        let res = IvyParser::parse_with_userdata(Rule::stmt, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).expect("AST generation failed")
    }

    pub fn action_from_decl(prog: &str) -> Action {
        let res = IvyParser::parse_with_userdata(Rule::action, prog, prog.to_owned().into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::action(res).expect("AST generation failed")
    }

    pub fn fmla_from_src(prog: &str) -> Fmla {
        let res = IvyParser::parse_with_userdata(Rule::fmla, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res).expect("AST generation failed")
    }

    pub fn rval_from_src(prog: &str) -> Expr {
        let res = IvyParser::parse_with_userdata(Rule::rval, prog, prog.into())
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::rval(res).expect("AST generation failed")
    }

    //

    pub fn number(n: i64) -> Expr {
        expressions::Expr::Number {
            span: Span::IgnoredForTesting,
            val: n,
        }
    }

    pub fn logical_number(n: i64) -> Fmla {
        logic::Fmla::Number {
            span: Span::IgnoredForTesting,
            val: n,
        }
    }

    //

    pub fn inferred_progsym<S>(s: S) -> Expr
    where
        S: Into<String>,
    {
        expressions::Expr::ProgramSymbol {
            span: Span::IgnoredForTesting,

            sym: expressions::Symbol {
                name: s.into(),
                decl: expressions::Sort::ToBeInferred,
            },
        }
    }

    pub fn inferred_logicsym<S>(s: S) -> Fmla
    where
        S: Into<String>,
    {
        logic::Fmla::LogicSymbol {
            span: Span::IgnoredForTesting,

            sym: expressions::Symbol {
                name: s.into(),
                decl: expressions::Sort::ToBeInferred,
            },
        }
    }
}
