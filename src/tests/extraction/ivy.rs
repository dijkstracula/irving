#[cfg(test)]
mod tests {
    use std::fs::read_to_string;
    use std::path::PathBuf;
    use std::rc::Rc;

    use crate::ast::declarations::Decl;
    use crate::extraction::ivy::Extractor;
    use crate::parser::ivy::ParserState;
    use crate::tests::helpers::prog_from_filename;
    use crate::visitor::ast::Visitable;
    use crate::{
        ast::{expressions::Expr, logic::Fmla},
        parser::ivy::{IvyParser, Result, Rule},
    };
    use pest_consume::Parser;

    fn parse_decl(fragment: &str) -> Result<Decl> {
        let user_data = Rc::new(ParserState::new(PathBuf::from(file!()), fragment));
        let res = IvyParser::parse_with_userdata(Rule::decl, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::decl(res)
    }

    fn parse_rval(fragment: &str) -> Result<Expr> {
        let user_data = Rc::new(ParserState::new(
            PathBuf::from(file!()),
            fragment.to_string(),
        ));
        let res = IvyParser::parse_with_userdata(Rule::rval, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::rval(res)
    }

    fn parse_fmla(fragment: &str) -> Result<Fmla> {
        let user_data = Rc::new(ParserState::new(
            PathBuf::from(file!()),
            fragment.to_string(),
        ));
        let res = IvyParser::parse_with_userdata(Rule::fmla, fragment, user_data)
            .expect("Parsing failed")
            .single()
            .unwrap();
        IvyParser::fmla(res)
    }

    #[test]
    fn pprint_class() {
        let mut e = Extractor::<String>::new();

        let fragment = "class pt = {
    field x: unbounded_sequence
    field y: unbounded_sequence

    action norm(self:pt) returns(n:unbounded_sequence)
}
";
        let mut ast = parse_decl(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn pprint_subclass() {
        let mut e = Extractor::<String>::new();

        let fragment = "subclass pt of unbounded_sequence = {
    field z: unbounded_sequence

    action norm(self:pt) returns(n:unbounded_sequence)
}
";
        let mut ast = parse_decl(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
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

        let fragment = "forall X:node, Y:node, Z:node . X = Y & Y = Z -> X = Y";
        let mut ast = parse_fmla(fragment).expect("Parsing failed");
        ast.visit(&mut e).expect("traversal failed");
        assert_eq!(fragment, e.pp.out);
    }

    #[test]
    fn pprint_formula_with_dots() {
        let mut e = Extractor::<String>::new();

        let fragment = "forall I:pid, J:pid . host(I).sock.id ~= host(J).sock.id";
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
