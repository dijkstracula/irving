#[cfg(test)]
mod tests {
    use crate::{
        extraction::vmt::Extractor, tests::helpers, typechecker::inference::SortInferer,
        visitor::ast::Visitable,
    };

    fn extract(fragment: &str) -> String {
        let mut ast = helpers::prog_from_decls(fragment);

        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
        e.pp.out
    }

    #[test]
    fn test_param_dedup() {
        let fragment = "#lang ivy1.8
        type id
        action i(x: id) = {}
        action b(x: bool) = {}";

        let extracted = extract(fragment);
        assert!(extracted.contains("declare-fun x () Int"));
        assert!(extracted.contains("declare-fun x1 () Bool"));
    }
}
