use std::collections::BTreeMap;

use crate::{
    ast::{
        expressions::{self, BinOp, Expr, Verb},
        logic,
        span::Span,
        toplevels,
    },
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
/// Let's assume X is an unbounded_sequence, the only infinite sort we really
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
    pub bounds: BTreeMap<String, Vec<(Option<Expr>, Option<Expr>)>>,

    /// Are we negating the current subexpression that we're visiting?
    /// (in the ivy_to_cpp implementation, this is `exists`)
    polarity: Polarity,
}

impl QuantBounds {
    pub fn new_forall() -> Self {
        Self {
            bounds: BTreeMap::new(),
            polarity: Polarity::Forall,
        }
    }

    // Adds one to an expression and performs some simple simplifications on it.
    pub fn succ(expr: &Expr) -> Expr {
        let mut ret = Expr::BinOp {
            span: Span::Optimized,
            expr: BinOp {
                lhs: Box::new(expr.clone()),
                op: expressions::Verb::Plus,
                rhs: Box::new(Expr::Number {
                    span: Span::Optimized,
                    val: 1,
                }),
            },
        };

        let mut cf = ConstantFold;
        ret.visit(&mut cf).unwrap().modifying(&mut ret);
        ret
    }

    // Produces the exclusive half-open interval described by the binop, if it's comparing two numerics.
    pub fn bounds_from_ast(
        lvar: expressions::Symbol,
        lhs: &Expr,
        op: expressions::Verb,
        rhs: &Expr,
    ) -> Option<(Option<Expr>, Option<Expr>)> {
        let (lo, hi) = match (lhs, &op, rhs) {
            (lhs, expressions::Verb::Le, rhs) => (lhs.clone(), QuantBounds::succ(rhs)),
            (lhs, expressions::Verb::Lt, rhs) => (lhs.clone(), rhs.clone()),
            (lhs, expressions::Verb::Gt, rhs) => (rhs.clone(), lhs.clone()),
            (lhs, expressions::Verb::Ge, rhs) => (rhs.clone(), QuantBounds::succ(lhs)),
            _ => return None,
        };

        let lo = match lo {
            Expr::LogicSymbol { sym, .. } if lvar == sym => None,
            _ => Some(lo),
        };
        let hi = match hi {
            Expr::LogicSymbol { sym, .. } if lvar == sym => None,
            _ => Some(hi),
        };

        Some((lo, hi))
    }

    // Produces whether or not this is a binop that we're interested in (namely
    // one that uses a logical symbol)
    pub fn op_on_logicsym(ast: &expressions::BinOp) -> Option<&expressions::Symbol> {
        // XXX: this precludes spanning operations like `X = X` but Ivy doesn't do this
        // either it seems.
        match (ast.lhs.as_ref(), &ast.rhs.as_ref()) {
            (
                Expr::LogicSymbol {
                    sym: expressions::Symbol { name: lname, .. },
                    ..
                },
                Expr::LogicSymbol {
                    sym: expressions::Symbol { name: rname, .. },
                    ..
                },
            ) if lname == rname => return None,
            _ => (),
        };

        if let Expr::LogicSymbol { sym, .. } = ast.lhs.as_ref() {
            return Some(sym);
        }
        if let Expr::LogicSymbol { sym, .. } = ast.rhs.as_ref() {
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
        for var in &ast.vars {
            self.bounds.insert(var.name.clone(), vec![]);
        }
        Ok(ControlMut::Produce(()))
    }

    // ivy_to_cpp.py:3838: "if isinstance(body, il.Not): ..."
    fn begin_unary_op(
        &mut self,
        op: &mut expressions::Verb,
        _rhs: &mut Expr,
    ) -> VisitorResult<(), std::fmt::Error, Expr> {
        if op == &expressions::Verb::Not {
            self.polarity = self.polarity.negate();
        }
        Ok(ControlMut::Produce(()))
    }

    fn finish_unary_op(
        &mut self,
        op: &mut expressions::Verb,
        _rhs: &mut Expr,
        _rhs_t: (),
    ) -> VisitorResult<(), std::fmt::Error, Expr> {
        if op == &expressions::Verb::Not {
            self.polarity = self.polarity.negate();
        }
        Ok(ControlMut::Produce(()))
    }

    fn begin_binop(
        &mut self,
        ast: &mut expressions::BinOp,
    ) -> VisitorResult<(), std::fmt::Error, Expr> {
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
                    // TODO: don't we need to also visit the other side of the expression (eg. the side
                    // that doesn't have the logic var)?

                    let ast = match self.polarity {
                        Polarity::Forall => ast.clone(),
                        Polarity::Exists => BinOp {
                            lhs: Box::new(*ast.lhs.clone()),
                            op: ast.op.negate(),
                            rhs: Box::new(*ast.rhs.clone()),
                        },
                    };

                    let (lhs, op, rhs) = (ast.lhs.as_ref(), op, ast.rhs.as_ref());
                    if let Some(bound) = QuantBounds::bounds_from_ast(lhs, op, rhs) {
                        log::debug!(target: "quantifier-bounds", "{} ∈ ({:?}, {:?}], {:?}", sym.name, bound.0, bound.1, self.polarity);

                        self.bounds.get_mut(&sym.name).unwrap().push(bound);
                    }
                }
            }
            _ => (),
        };

        Ok(ControlMut::SkipSiblings(()))
    }
}
