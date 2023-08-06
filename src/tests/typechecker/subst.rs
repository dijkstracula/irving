#[cfg(test)]
mod tests {
    use crate::{
        ast::{
            declarations::{self, Binding},
            expressions::Sort,
            span::Span,
        },
        tests::helpers,
        typechecker::{inference::SortInferer, sorts::IvySort, subst::SortSubstituter},
        visitor::ast::Visitable,
    };

    #[test]
    fn uninterpreted_type_resolution() {
        // From http://microsoft.github.io/ivy/language.html :
        //
        // The types of numerals are inferred from context. For example, if x
        // has type foo, then in the expression x+1, the numeral 1 is inferred
        // to have type foo."
        let mut prog = helpers::prog_from_decls(
            "
#lang ivy1.8

#At this type definition, `node` will have sort ToBeInferred.
type node

action inc(x: node) returns (y: node) = {
    # Here, the sort unifier will know from context that node
    # is unifiable with Number, and the ResolverBinder's context
    # will know this.
    y := x + 1;
}
",
        );

        // Confirming what we know from the program source comments: Prior to
        // typechecking, we know nothing of `node`'s sort.
        assert_eq!(
            Some(&declarations::Decl::Type {
                span: Span::IgnoredForTesting,
                decl: Binding::from("node", Sort::ToBeInferred)
            }),
            prog.top.get(0)
        );

        // Now let's perform phase 1 of typechecking.  We'll lift Sort::ToBeInferred
        // into a concrete IvySort, namely a SortVar.  Further, we should know that
        // the SortVar's index back into the typechecker's context should be an
        // IvySort::Number by virtue of inferring a Node's use in the inc action.
        let mut si = SortInferer::new();
        prog.visit(&mut si).expect("Type inference");
        assert_eq!(
            // There's only one Sort in play in this program, so we know that the
            // SortVar ID must be 0.
            Some(&declarations::Decl::Type {
                span: Span::IgnoredForTesting,
                decl: Binding::from("node", Sort::Resolved(IvySort::SortVar(0)))
            }),
            prog.top.get(0)
        );
        assert_eq!(
            // There's only one Sort in play in this program, so we know that the
            // SortVar ID must be 0.
            Some(&IvySort::Number),
            si.bindings.ctx.get(0)
        );

        // Now let's conclude type checking by resolving the constraints we've
        // uncovered back into the AST.  Now, we should be able to see that we now
        // know the _actual_ underlying sort of `node`.
        let mut sb = SortSubstituter::from_inferer(si);
        prog.visit(&mut sb).expect("Type substitution");
        assert_eq!(
            // There's only one Sort in play in this program, so we know that the
            // SortVar ID must be 0.
            Some(&declarations::Decl::Type {
                span: Span::IgnoredForTesting,
                decl: Binding::from("node", Sort::Resolved(IvySort::Number))
            }),
            prog.top.get(0)
        );
    }
}
