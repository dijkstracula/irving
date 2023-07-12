#[cfg(test)]
mod tests {
    use std::fs::read_to_string;

    use crate::passes::pprint::PrettyPrinter;
    use crate::tests::helpers::prog_from_filename;
    use crate::visitor::visitor::Visitable;
    use crate::{
        ast::{expressions::Expr, logic::Fmla},
        parser::ivy::{IvyParser, Result, Rule},
    };
    use pest_consume::Parser;

    fn parse_expr(fragment: &str) -> Result<Expr> {
        let res = IvyParser::parse(Rule::expr, fragment)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::expr(res)
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
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "foo(a, b, c)";
        let mut ast = parse_expr(fragment).expect("Parsing failed");
        ast.visit(&mut pp).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }

    #[test]
    fn pprint_arith() {
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "43 - 1";
        let mut ast = parse_expr(fragment).expect("Parsing failed");
        ast.visit(&mut pp).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }

    #[test]
    fn pprint_complex_expr() {
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "sock.send(host(1 - self).sock.id, val)";
        let mut ast = parse_expr(fragment).expect("Parsing failed");
        ast.visit(&mut pp).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }

    #[test]
    fn pprint_formula() {
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "forall X: node, Y, Z . X = Y & Y = Z -> X = Y";
        let mut ast = parse_fmla(fragment).expect("Parsing failed");
        ast.visit(&mut pp).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }

    #[test]
    fn pprint_prog() {
        let path = "programs/002_safety_and_invariants.ivy";

        let mut ast = prog_from_filename(path);

        let mut pp = PrettyPrinter::<String>::new();
        ast.visit(&mut pp).expect("traversal failed");

        // The pretty printer won't get my formatting exactly right, and that's
        // fine.  Strip out extraneous newlines on my end.
        println!("{}", pp.out);
        let prog = read_to_string(path).unwrap();
        assert_eq!(prog.replace("\n\n", "\n"), pp.out.replace("\n\n", "\n"));
    }
}
