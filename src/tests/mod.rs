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
        .filter_level(log::LevelFilter::Trace)
        .try_init();
}

#[cfg(test)]
pub mod helpers {
    use std::{path::PathBuf, rc::Rc};

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
        parser::ivy::{IvyParser, ParserState, Rule},
        passes::{self},
    };
    use pest_consume::Parser;

    pub fn prog_from_filename(path: &str) -> Prog {
        let prog = std::fs::read_to_string(path).unwrap();
        let user_data = Rc::new(ParserState::new(PathBuf::from(path), prog.to_string()));
        let res = IvyParser::parse_with_userdata(Rule::prog, &prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::prog(res).expect("AST generation failed")
    }

    pub fn prog_from_decls(prog: &str) -> Prog {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog.to_string()));
        let res = IvyParser::parse_with_userdata(Rule::prog, prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::prog(res).expect("AST generation failed")
    }

    pub fn process_from_decl(prog: &str) -> Decl {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog.to_string()));
        let res = IvyParser::parse_with_userdata(Rule::process_decl, prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let decl = IvyParser::process_decl(res).expect("AST generation failed");
        Decl::Object { decl }
    }

    pub fn module_from_src(prog: &str) -> Decl {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog.to_string()));
        let res = IvyParser::parse_with_userdata(Rule::module_decl, prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();

        let decl = IvyParser::module_decl(res).expect("AST generation failed");
        Decl::Module { decl }
    }

    #[allow(dead_code)]
    pub fn vector_moduledecl() -> Decl {
        module_from_src(
            "module vector(range) = { 
            type this

            action get(a:this,x:nat) returns (y:range)
            action set(a:this,x:nat,y:range) returns (a:this)
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
                        var id: nat

                        action send(dest: endpoint, m: msg)
                        action recv(src: endpoint, m: msg)
                    }
                }
            }",
        )
    }

    pub fn typeinference_from_filename(path: &str) -> Prog {
        let text = std::fs::read_to_string(path).unwrap();
        let user_data = Rc::new(ParserState::new(PathBuf::from(path), text.clone()));
        let res = IvyParser::parse_with_userdata(Rule::prog, &text, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut prog = IvyParser::prog(res).expect("AST generation failed");
        passes::all_passes(&mut prog).unwrap();
        prog
    }

    pub fn decl_from_src(prog: &str) -> Decl {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog));
        let res = IvyParser::parse_with_userdata(Rule::decl, prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect("AST generation failed")
    }

    pub fn stmt_from_src(prog: &str) -> Stmt {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog));
        let res = IvyParser::parse_with_userdata(Rule::stmt, prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::stmt(res).expect("AST generation failed")
    }

    pub fn action_from_decl(prog: &str) -> Action {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog));
        let res = IvyParser::parse_with_userdata(Rule::action, prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::action(res).expect("AST generation failed")
    }

    pub fn fmla_from_src(prog: &str) -> Fmla {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog));
        let res = IvyParser::parse_with_userdata(Rule::fmla, prog, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res).expect("AST generation failed")
    }

    pub fn rval_from_src(prog: &str) -> Expr {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), prog));
        let res = IvyParser::parse_with_userdata(Rule::rval, prog, user_data)
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
                span: Span::IgnoredForTesting,
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
                span: Span::IgnoredForTesting,
            },
        }
    }
}
