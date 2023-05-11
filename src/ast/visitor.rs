use super::actions::*;
use super::declarations::*;
use super::expressions::*;
use super::statements::*;
use super::toplevels::*;

pub trait StatementVisitor<T> {
    fn visit_prog(&mut self, p: &Prog) -> T;

    // Statements
    fn visit_if(&mut self, p: &If) -> T;
    fn visit_while(&mut self, p: &While) -> T;
    fn visit_action_sequence(&mut self, actions: &Vec<Action>) -> T;

    // Actions
    fn visit_assert(&mut self, a: &AssertAction) -> T;
    fn visit_assign(&mut self, a: &AssignAction) -> T;
    fn visit_assume(&mut self, a: &AssumeAction) -> T;
    fn visit_call(&mut self, e: &AppExpr) -> T;
    fn visit_ensure(&mut self, e: &EnsureAction) -> T;
    fn visit_requires(&mut self, e: &RequiresAction) -> T;

    // Declarations
    fn visit_action_decl(&mut self, action: &ActionDecl) -> T;
    fn visit_after(&mut self, action: &AfterDecl) -> T;
    fn visit_alias(&mut self, name: &Symbol, val: &Expr) -> T;
    fn visit_axiom(&mut self, axiom: &Expr) -> T;
    fn visit_before(&mut self, action: &BeforeDecl) -> T;
    fn visit_export(&mut self, action: &ExportDecl) -> T;
    fn visit_function(&mut self, fun: &FunctionDecl) -> T;
    fn visit_globals(&mut self, defs: &Vec<Decl>) -> T;
    fn visit_import(&mut self, action: &ImportDecl) -> T;
    fn visit_include(&mut self, module: &Symbol) -> T;
    fn visit_instance(&mut self, inst: &InstanceDecl) -> T;
    fn visit_invariant(&mut self, inv: &Expr) -> T;
    fn visit_module(&mut self, module: &ModuleDecl) -> T;
    fn visit_object(&mut self, obj: &ObjectDecl) -> T;
    fn visit_relation(&mut self, obj: &Relation) -> T;
    fn visit_vardecl(&mut self, term: &Term) -> T;
    fn visit_typedecl(&mut self, name: &Symbol, sort: &Sort) -> T;

    fn visit_expr<U> (&mut self, e: &Expr) -> U;

    // auto-visitation for intermediary AST nodes


    fn visit_stmt(&mut self, s: &Stmt) -> T {
        match s {
            Stmt::ActionSequence(aa) => self.visit_action_sequence(aa),
            Stmt::If(i) => self.visit_if(i),
            Stmt::While(w) => self.visit_while(w),
            Stmt::Expr(e) => self.visit_expr(e),
        }
    }

    fn visit_action(&mut self, a: &Action) -> T {
        match a {
            Action::Assert(a) => self.visit_assert(a),
            Action::Assign(a) => self.visit_assign(a),
            Action::Assume(a) => self.visit_assume(a),
            Action::Call(e) => self.visit_call(e),
            Action::Ensure(en) => self.visit_ensure(en),
            Action::Requires(req) => self.visit_requires(req)
        }
    }


}

pub trait ExpressionVisitor<T> {
    // Expressions
    fn visit_app(&mut self, a: &AppExpr) -> T;
    fn visit_binop(&mut self, lhs: &Expr, op: &Verb, rhs: &Expr) -> T;
    fn visit_boolean(&mut self, b: &bool) -> T;
    fn visit_formula(&mut self, fmla: &Formula) -> T;
    fn visit_identifier(&mut self, ident: &Ident) -> T;
    fn visit_index(&mut self, idx: &IndexExpr) -> T;
    fn visit_number(&mut self, n: &i64) -> T;
    fn visit_subscript(&mut self, n: &i64) -> T;
    fn visit_unaryop(&mut self, op: &Verb, expr: &Expr) -> T;
    fn visit_term(&mut self, term: &Term) -> T;

    fn visit_expr(&mut self, e: &Expr) -> T {
        match e {
            Expr::App(app) => self.visit_app(app),
            Expr::BinOp{lhs, op, rhs} => self.visit_binop(&lhs, op, &rhs),
            Expr::Boolean(b) => self.visit_boolean(b),
            Expr::Formula(fmla) => self.visit_formula(fmla),
            Expr::Identifier(ident) => self.visit_identifier(ident),
            Expr::Index(idx) => self.visit_index(idx),
            Expr::Number(i) => self.visit_number(i),
            Expr::UnaryOp { op, expr } => self.visit_unaryop(op, expr),
            Expr::Term(t) => self.visit_term(t)
        }
    }
}