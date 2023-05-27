#[cfg(test)]
mod tests {
    use pest_consume::Parser;
    use crate::visitor::visitor::Visitor;
    use crate::visitor::pprint::PrettyPrinter;
    use crate::{parser::ivy::{Result, Rule, IvyParser}, ast::{expressions::Expr, logic::Fmla}};

    fn parse_expr(fragment: &str) -> Result<Expr> {
        let res = IvyParser::parse(Rule::expr, fragment)
            .expect("Parsing failed").single().unwrap();
        IvyParser::expr(res)
    }

    fn parse_fmla(fragment: &str) -> Result<Fmla> {
        let res = IvyParser::parse(Rule::fmla, fragment)
            .expect("Parsing failed").single().unwrap();
        IvyParser::fmla(res)
    }

    #[test]
    fn pprint_fnapp() {
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "foo(a, b, c)";
        let mut _ast = parse_expr(fragment).expect("Parsing failed");

        pp.visit_expr(&mut _ast).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }

    #[test]
    fn pprint_arith() {
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "43 - 1";
        let mut _ast = parse_expr(fragment).expect("Parsing failed");
        pp.visit_expr(&mut _ast).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }


    #[test]
    fn pprint_complex_expr() {
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "sock.send(host(1 - self).sock.id, val)";
        let mut _ast = parse_expr(fragment).expect("Parsing failed");
        pp.visit_expr(&mut _ast).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }

    #[test]
    fn pprint_formula() {
        let mut pp = PrettyPrinter::<String>::new();

        let fragment = "forall X:node, Y, Z . X = Y & Y = Z -> X = Y";
        let mut _ast = parse_fmla(fragment).expect("Parsing failed");
        pp.visit_formula(&mut _ast).expect("traversal failed");
        assert_eq!(fragment, pp.out);
    }

    #[test]
    fn pprint_prog() {
        let prog = include_str!("../programs/002_safety_and_invariants.ivy");
        let res = IvyParser::parse(Rule::prog, &prog)
            .expect("Parsing failed")
            .single().unwrap();
        let mut _ast = IvyParser::prog(res)
            .expect("AST generation failed");

        let mut pp = PrettyPrinter::<String>::new();
        pp.visit_prog(&mut _ast).expect("traversal failed");
        println!("{}", pp.out);
    }

}