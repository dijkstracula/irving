#[cfg(test)]
mod tests {
    use crate::ast::declarations::Decl;
    use crate::parser::ivy::{IvyParser, Rule};
    use crate::visitor::global_lowerer::GlobalLowerer;
    use pest_consume::Parser;

    #[test]
    fn lower_simple_prog() {
        let prog = "#lang ivy1.8
global {
    instance net : tcp_test.net(msg_t) 
}
object abc = {
    global {
        instance nset : indexset(node)
    }
}
";
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single()
            .unwrap();
        let mut ast = IvyParser::prog(res).expect("AST generation failed");

        assert!(ast.top.body.len() == 2);
        match ast.top.body.get(0) {
            Some(Decl::Globals(gs)) => {
                assert!(gs.len() == 1);
            }
            _ => unreachable!(),
        }

        GlobalLowerer::visit(&mut ast);

        // Implementation detail: the lowerer collects all the globals and then appends
        // the collated collection to the end of the isolate body, so look at the final
        // element here.
        match ast.top.body.last() {
            Some(Decl::Globals(gs)) => {
                //
                assert!(gs.len() == 2);
            }
            _ => unreachable!(),
        }
    }
}
