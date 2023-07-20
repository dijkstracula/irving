mod extraction;
mod parser;
mod passes;
mod programs;
mod stdlib;
mod typechecker;

#[cfg(test)]
mod helpers {
    use crate::{
        ast::{declarations::Decl, expressions::Expr, toplevels::Prog},
        parser::ivy::{IvyParser, Rule},
        passes::global_lowerer::GlobalLowerer,
        stdlib::load_stdlib,
        visitor::ast::Visitable,
    };
    use pest_consume::Parser;

    pub fn prog_from_filename(path: &str) -> Prog {
        let prog = std::fs::read_to_string(path).unwrap();
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::prog(res).expect("AST generation failed")
    }

    pub fn process_from_decl(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::process_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Object(IvyParser::process_decl(res).expect("AST generation failed"))
    }

    pub fn module_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::module_decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        Decl::Module(IvyParser::module_decl(res).expect("AST generation failed"))
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

    #[allow(dead_code)]
    pub fn typechecked_from_filename(path: &str) -> Prog {
        let text = std::fs::read_to_string(path).unwrap();
        let res = IvyParser::parse(Rule::prog, &text)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut prog = IvyParser::prog(res).expect("AST generation failed");

        let mut gl = GlobalLowerer::new();

        // Fake out the "standard library"
        let mut tc = load_stdlib().unwrap();

        prog.visit(&mut gl)
            .expect("lowering globals")
            .modifying(&mut prog)
            .unwrap();

        prog.visit(&mut tc)
            .expect("typechecking")
            .modifying(&mut prog)
            .unwrap();

        prog
    }

    pub fn decl_from_src(prog: &str) -> Decl {
        let res = IvyParser::parse(Rule::decl, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res).expect("AST generation failed")
    }

    pub fn expr_from_src(prog: &str) -> Expr {
        let res = IvyParser::parse(Rule::expr, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::expr(res).expect("AST generation failed")
    }
}
