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

    #[test]
    fn test_relation_init() {
        let fragment = "#lang ivy1.8
        type id
        relation client(X: id)
        relation foo(X: bool)

        after init {
            client(X) := false;
            foo(X) := false;
        }
        ";

        let extracted = extract(fragment);
        println!("{:?}", extracted);

        // The first sort (Arr/Const/Read/Write) is for `client`, whereas the
        // second sort (Arr1/Const1/Read1/Write) is for `foo`.
        //
        // TODO: this seems wrong.  The type of a Read and Write function ought
        // to be `(Arr Domain) BoundedSequence` and `(Arr Domain BoundedSequence) Arr`, but Arrays
        // simply must be indexed according to Int, right?  So what if the
        // domain does not boil down to one?
        assert!(extracted.contains("(declare-fun Read (Arr Int) Bool"));
        assert!(extracted.contains("(declare-fun Read1 (Arr1 Bool) Bool"));
        assert!(extracted.contains("(declare-fun Write (Arr Int Bool) Arr"));
        assert!(extracted.contains("(declare-fun Write1 (Arr1 Bool Bool) Arr1"));
    }

    #[test]
    fn test_noninf1() {
        // This is Cole's noninterference program.  We should always be able to
        // extract it.
        let path = "programs/300_noninf1.ivy";
        let mut ast = helpers::prog_from_filename(path);

        let mut tc = SortInferer::new();
        ast.visit(&mut tc).expect("typechecking failed");

        let mut e = Extractor::<String>::new();
        ast.visit(&mut e).expect("extraction failed");
    }
}
