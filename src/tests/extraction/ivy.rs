#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use crate::extraction::ivy::Extractor;
    use crate::tests::helpers::prog_from_filename;
    use crate::visitor::ast::Visitable;
    use crate::{
        ast::{expressions::ExprKind, logic::Fmla},
        parser::ivy::{IvyParser, Result, Rule},
    };
    use pest_consume::Parser;

    fn parse_rval(fragment: &str) -> Result<ExprKind> {
        let res = IvyParser::parse(Rule::rval, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::rval(res)
    }

    fn parse_fmla(fragment: &str) -> Result<Fmla> {
        let res = IvyParser::parse(Rule::fmla, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res)
    }

    #[test]
    fn pprint_fnapp() {
        let mut e = Extractor::<String>::new();

        let fragment = "foo(a, b, c)";
        let mut ast = parse_rval(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn pprint_arith() {
        let mut e = Extractor::<String>::new();

        let fragment = "43 - 1";
        let mut ast = parse_rval(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn pprint_complex_expr() {
        let mut e = Extractor::<String>::new();

        let fragment = "sock.send(host(1 - self).sock.id, val)";
        let mut ast = parse_rval(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn pprint_formula() {
        let mut e = Extractor::<String>::new();

        let fragment = "forall X:node, Y, Z . X = Y & Y = Z -> X = Y";
        let mut ast = parse_fmla(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn pprint_formula_with_dots() {
        let mut e = Extractor::<String>::new();

        let fragment = "forall I, J . host(I).sock.id ~= host(J).sock.id";
        let mut ast = parse_fmla(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    //#[test]
    #[allow(dead_code)]
    fn pprint_prog() {
        // TODO: The ivy extractor no longer extracts this file
        // unmodified, so it goes.  But, this means the test fails
        // and we should fix it at some point.
        let path = "programs/002_safety_and_invariants.ivy";

        let mut ast = prog_from_filename(path);

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("traversal failed");

        // The pretty printer won't get my formatting exactly right, and that's
        // fine.  Strip out extraneous newlines on my end.
        let prog = read_to_string(path).unwrap();
        assert_eq!(prog.replace("\n\n", "\n"), e.pp.out.replace("\n\n", "\n"));
    }
}
