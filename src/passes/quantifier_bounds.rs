use std::collections::BTreeMap;

use crate::{
    ast::{
        expressions::{self, Verb},
        logic::{self, Fmla, LogicBinOp},
        span::Span,
        toplevels,
    },
    typechecker::sorts::IvySort,
    visitor::{
        ast::{Visitable, Visitor},
        ControlMut, VisitorResult,
    },
};

use super::constant_fold::ConstantFold;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Polarity {
    Forall,
    Exists,
}

impl Polarity {
    pub fn negate(self) -> Self {
        match self {
            Polarity::Forall => Polarity::Exists,
            Polarity::Exists => Polarity::Forall,
        }
    }
}

/// Determines the valid inhabitants of a quantified variable X in a formula F
/// by emitting the range of values that we need to check in order to validate
/// the formula.
///
/// Let's assume X is an nat, the only infinite sort we really
/// care about at the moment.  Our goal is to emit a consistent expression of
/// the form:
///
///   forall X . F(X) --> IntStream.range(lo, hi).allMatch(X -> F(X))
///
///   exists X . F(X) --> forall X . ~F(X)
///                   --> IntStream.range(lo, hi).allMatch(X -> !F(X))
///
/// In other words, it's the same procedure for both kinds of quantifiers up to
/// a negation factor.  We have to find bounds on X by inspecting the
/// subexpressions of the formulas to find concrete bounds for X:
///
/// 1) Negations:
///      forall X, !F(X) <-> exists X . F(X)
pub struct QuantBounds {
    /// What subexpressions have we found that bound a quantified variable?
    pub bounds: BTreeMap<String, Vec<(Option<Fmla>, Option<Fmla>)>>,

    /// Are we negating the current subexpression that we're visiting?
    /// (in the ivy_to_cpp implementation, this is `exists`)
    polarity: Polarity,

    /// Are we re-rentering a forall/exists node?
    nested: bool,
}

impl QuantBounds {
    pub fn new_forall() -> Self {
        Self {
            bounds: BTreeMap::new(),
            polarity: Polarity::Forall,
            nested: false,
        }
    }

    // Adds one to an expression and performs some simple simplifications on it.
    pub fn succ(fmla: &Fmla) -> Fmla {
        let mut ret = Fmla::BinOp {
            span: Span::Optimized,
            binop: LogicBinOp {
                lhs: Box::new(fmla.clone()),
                op: expressions::Verb::Plus,
                rhs: Box::new(Fmla::Number {
                    span: Span::Optimized,
                    val: 1,
                }),
            },
        };

        let mut cf = ConstantFold;
        ret.visit(&mut cf).unwrap().modifying(&mut ret);
        ret
    }

    // Produces the interval covered by the given sort, if defined on the sort.
    // TODO: If IvySort::BoundedSequence held two numbers rather than two expressions, we
    // could probably have this return (Option<i64>, Option<i64>) or whatever,
    // which might simplify things.
    pub fn bounds_for_sort(sort: &IvySort) -> (Option<Fmla>, Option<Fmla>) {
        match sort {
            IvySort::Number => (
                Some(Fmla::Number {
                    span: Span::Optimized,
                    val: 0,
                }),
                None,
            ),
            IvySort::BoundedSequence(lo, hi) => (
                Some(Fmla::Number {
                    span: Span::Optimized,
                    val: *lo,
                }),
                Some(Fmla::Number {
                    span: Span::Optimized,
                    val: *hi + 1,
                }),
            ),
            IvySort::Enum(discs) => (
                Some(Fmla::Number {
                    span: Span::Optimized,
                    val: 0,
                }),
                Some(Fmla::Number {
                    span: Span::Optimized,
                    val: discs.len() as i64,
                }),
            ),
            _ => (None, None),
        }
    }

    // Produces the exclusive half-open interval described by the binop, if it's comparing two numerics.
    pub fn bounds_from_ast(
        lvar: &expressions::Symbol,
        lhs: &Fmla,
        op: expressions::Verb,
        rhs: &Fmla,
    ) -> Option<(Option<Fmla>, Option<Fmla>)> {
        let (lo, hi) = match (lhs, &op, rhs) {
            (lhs, expressions::Verb::Le, rhs) => (lhs.clone(), QuantBounds::succ(rhs)),
            (lhs, expressions::Verb::Lt, rhs) => (lhs.clone(), rhs.clone()),
            (lhs, expressions::Verb::Gt, rhs) => (QuantBounds::succ(rhs), lhs.clone()),
            (lhs, expressions::Verb::Ge, rhs) => (rhs.clone(), lhs.clone()),
            _ => return None,
        };

        let lo = match &lo {
            Fmla::LogicSymbol { sym, .. } if lvar == sym => None,
            _ => Some(lo),
        };
        let hi = match &hi {
            Fmla::LogicSymbol { sym, .. } if lvar == sym => None,
            _ => Some(hi),
        };

        Some((lo, hi))
    }

    // Produces whether or not this is a binop that we're interested in (namely
    // one that uses a logical symbol)
    pub fn op_on_logicsym(ast: &LogicBinOp) -> Option<&expressions::Symbol> {
        // XXX: this precludes spanning operations like `X = X` but Ivy doesn't do this
        // either it seems.
        match (ast.lhs.as_ref(), &ast.rhs.as_ref()) {
            (
                Fmla::LogicSymbol {
                    sym: expressions::Symbol { name: lname, .. },
                    ..
                },
                Fmla::LogicSymbol {
                    sym: expressions::Symbol { name: rname, .. },
                    ..
                },
            ) if lname == rname => return None,
            _ => (),
        };

        if let Fmla::LogicSymbol { sym, .. } = ast.lhs.as_ref() {
            return Some(sym);
        }
        if let Fmla::LogicSymbol { sym, .. } = ast.rhs.as_ref() {
            return Some(sym);
        }
        None
    }
}

impl Visitor<(), std::fmt::Error> for QuantBounds {
    fn begin_prog(
        &mut self,
        _ast: &mut toplevels::Prog,
    ) -> VisitorResult<(), std::fmt::Error, toplevels::Prog> {
        panic!("Don't run the QuantBounds pass on a whole program but rather just on a formula")
    }

    //

    fn begin_forall(
        &mut self,
        ast: &mut logic::Forall,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        log::debug!(target: "quantifier-bounds", "Beginning bounds for {:?}", ast);

        if self.nested {
            Ok(ControlMut::SkipSiblings(()))
        } else {
            for var in &ast.vars {
                self.bounds.insert(var.name.clone(), vec![]);
            }

            self.nested = true;
            Ok(ControlMut::Produce(()))
        }
    }

    fn finish_forall(
        &mut self,
        ast: &mut logic::Forall,
        _vars: Vec<()>,
        _fmla: (),
    ) -> VisitorResult<(), std::fmt::Error, Fmla> {
        // In the worst case, we can at least bound each variable according to
        // what the sort permits.  (We do this last so any actual bounds appear
        // first in the list, so we only grab them as a worst-case.)
        for var in &ast.vars {
            let is = match &var.decl {
                expressions::Sort::Resolved(is) => is,
                _ => unreachable!("Did the typechecker run?"),
            };

            self.bounds.get_mut(&var.name).map(|bounds| {
                let sort_range = QuantBounds::bounds_for_sort(is);
                bounds.push(sort_range);
            });
        }

        Ok(ControlMut::Produce(()))
    }

    // ivy_to_cpp.py:3838: "if isinstance(body, il.Not): ..."
    fn begin_logical_unary_op(
        &mut self,
        op: &mut Verb,
        _rhs: &mut Fmla,
    ) -> VisitorResult<(), std::fmt::Error, Fmla> {
        if op == &expressions::Verb::Not {
            self.polarity = self.polarity.negate();
        }
        Ok(ControlMut::Produce(()))
    }

    fn finish_logical_unary_op(
        &mut self,
        op: &mut Verb,
        _rhs: &mut Fmla,
        _rhs_t: (),
    ) -> VisitorResult<(), std::fmt::Error, Fmla> {
        if op == &expressions::Verb::Not {
            self.polarity = self.polarity.negate();
        }
        Ok(ControlMut::Produce(()))
    }

    fn begin_logical_binop(
        &mut self,
        ast: &mut LogicBinOp,
    ) -> VisitorResult<(), std::fmt::Error, Fmla> {
        match ast.op {
            Verb::Arrow if self.polarity == Polarity::Forall => {
                // ivy_to_cpp:3842: "if isinstance(body, il.Implies) and not exists:"
                self.polarity = self.polarity.negate();
                ast.lhs.visit(self)?.modifying(&mut ast.lhs);
                self.polarity = self.polarity.negate();

                ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            }
            Verb::And if self.polarity == Polarity::Exists => {
                // ivy_to_cpp:3850: "if isinstance(body, il.And) and exists:"
                ast.lhs.visit(self)?.modifying(&mut ast.lhs);
                ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            }
            Verb::Or if self.polarity == Polarity::Forall => {
                // ivy_to_cpp:3846: "if isinstance(body, il.Or) and not exists:"
                ast.lhs.visit(self)?.modifying(&mut ast.lhs);
                ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            }
            op if op.is_numeric_cmp() => {
                // ivy_to_cpp:3840: "if body.rep.name in ['<', '<=', '>', '>=']:"
                if let Some(sym) = QuantBounds::op_on_logicsym(ast) {
                    // XXX: unnecessary clone; match out lhs, op, rhs
                    let ast = match self.polarity {
                        // Searching for a counterproof of an existential means
                        // we have to walk all inhabitants in that range, which
                        // we already have from the operation.
                        Polarity::Exists => ast.clone(),

                        // Searching for a counterproof of a universal means we
                        // want to
                        Polarity::Forall => LogicBinOp {
                            lhs: Box::new(*ast.lhs.clone()),
                            op: ast.op.negate(),
                            rhs: Box::new(*ast.rhs.clone()),
                        },
                    };

                    let (lhs, op, rhs) = (ast.lhs.as_ref(), ast.op, ast.rhs.as_ref());
                    if let Some(bound) = QuantBounds::bounds_from_ast(sym, lhs, op, rhs) {
                        log::debug!(target: "quantifier-bounds", "{} ∈ ({:?}, {:?}], {:?}", sym.name, bound.0, bound.1, self.polarity);

                        self.bounds.get_mut(&sym.name).map(|bs| bs.push(bound));
                    }
                }
            }
            _ => (),
        };

        Ok(ControlMut::SkipSiblings(()))
    }
}
