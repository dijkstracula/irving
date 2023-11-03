use crate::{
    ast::{
        declarations::{self, Binding},
        logic,
        span::Span,
    },
    extraction::{java::extraction::expressions::Token, ExtractResult},
    passes::quantifier_bounds::QuantBounds,
    typechecker::sorts::{ActionArgs, IvySort},
};
use std::{
    collections::BTreeMap,
    fmt::{self, Write},
};

use thiserror::Error;

use crate::{
    ast::{
        actions,
        expressions::{self, IndexExpr, Verb},
        statements, toplevels,
    },
    extraction::pprint::PrettyPrinter,
    visitor::ast::{Visitable, Visitor},
};

use crate::visitor::*;

use super::types::JavaType;

pub struct Extractor<W>
where
    W: Write,
{
    pub pp: PrettyPrinter<W>,
    type_aliases: BTreeMap<Token, JavaType>,
}

impl Extractor<String> {
    pub fn new() -> Self {
        Self {
            pp: PrettyPrinter::new(),
            type_aliases: BTreeMap::new(),
        }
    }
}

impl<W> Extractor<W>
where
    W: Write,
{
    pub fn write_separated<U>(&mut self, us: &mut Vec<U>, sep: &str) -> ExtractResult<Vec<U>>
    where
        U: Visitable<(), std::fmt::Error>,
    {
        for (i, u) in us.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(sep)?;
            }
            u.visit(self)?;
        }
        Ok(ControlMut::Produce(()))
    }

    pub fn write_paramlist(
        &mut self,
        us: &mut expressions::ParamList,
        sep: &str,
    ) -> ExtractResult<Vec<expressions::Symbol>> {
        for (i, u) in us.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(sep)?;
            }
            self.param(u)?;
        }
        Ok(ControlMut::Produce(()))
    }

    pub fn write_lambda(
        &mut self,
        params: &mut expressions::ParamList,
        ret: &mut Option<expressions::Symbol>,
        body: &mut Vec<statements::Stmt>,
    ) -> ExtractResult<Vec<statements::Stmt>> {
        self.pp.write_str("(")?;
        self.write_paramlist(params, ", ")?;
        self.pp.write_str(") -> {\n")?;

        // The first declaration needs to define the return value.
        ret.as_mut().map(|ret| {
            let mut retdecl = declarations::Decl::Var {
                /* We're just using this to walk the binding, so the span */
                decl: Binding::from(ret.name.clone(), ret.decl.clone(), Span::Optimized),
            };
            retdecl.visit(self).unwrap();
            self.pp.write_str(";\n").unwrap();
        });

        for stmt in body {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }

        match ret {
            None => self.pp.write_str("return null;\n")?,
            Some(ret) => self.pp.write_fmt(format_args!("return {};\n", ret.name))?,
        }

        self.pp.write_str("}")?;

        Ok(ControlMut::Produce(()))
    }

    fn jtype_from_sort(s: &mut expressions::Sort) -> JavaType {
        match s {
            expressions::Sort::ToBeInferred => todo!(),
            expressions::Sort::Annotated(_) => todo!(),
            expressions::Sort::Resolved(ivysort) => {
                let j: JavaType = ivysort.clone().into(); // XXX: poor choices lead to this clone.a
                j
            }
        }
    }

    pub fn write_bounded_long(
        &mut self,
        e: &mut expressions::Expr,
        lo: i64,
        hi: i64,
    ) -> fmt::Result {
        if let expressions::Expr::Number { val, .. } = e {
            let normalized = if *val >= hi {
                hi
            } else if *val < lo {
                lo
            } else {
                *val
            };
            return self.pp.write_fmt(format_args!("{normalized}"));
        }

        // ((e) >= hi ? hi : ((e) < lo ? lo : (e)))
        self.pp.write_str("(")?;
        self.pp.write_str("(")?;
        e.visit(self)?.modifying(e);
        self.pp.write_fmt(format_args!(") >= {hi} ? {hi} : "))?;

        self.pp.write_str("(")?;
        self.pp.write_str("(")?;
        e.visit(self)?.modifying(e);
        self.pp.write_fmt(format_args!(") < {lo} ? {lo} : ("))?;
        e.visit(self)?.modifying(e);
        self.pp.write_str(")")?;
        self.pp.write_str(")")?;

        self.pp.write_str(")")
    }

    pub fn write_unbounded_seq(&mut self, e: &mut expressions::Expr) -> fmt::Result {
        if let expressions::Expr::Number { val, .. } = e {
            let normalized = if *val < 0 { 0 } else { *val };
            return self.pp.write_fmt(format_args!("{normalized}"));
        }

        // ((e) < 0 ? 0 : (e))

        self.pp.write_str("(")?;
        e.visit(self)?.modifying(e);
        self.pp.write_fmt(format_args!(") < 0 ? 0 : ("))?;
        e.visit(self)?.modifying(e);
        self.pp.write_str(")")
    }
}

impl<W> ast::Visitor<(), std::fmt::Error> for Extractor<W>
where
    W: Write,
{
    fn begin_prog(&mut self, ast: &mut toplevels::Prog) -> ExtractResult<toplevels::Prog> {
        self.emit_prog(ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn action_seq(&mut self, ast: &mut Vec<actions::Action>) -> ExtractResult<statements::Stmt> {
        for (i, a) in ast.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(";\n")?;
            }
            a.visit(self)?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn begin_if(&mut self, ast: &mut statements::If) -> ExtractResult<statements::Stmt> {
        self.emit_if(ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_while(&mut self, ast: &mut statements::While) -> ExtractResult<statements::Stmt> {
        self.emit_while(ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    // Declarations

    fn begin_action_decl(
        &mut self,
        span: &Span,
        name: &mut Token,
        ast: &mut declarations::ActionDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("protected ")?;
        self.emit_action_object_declaration(span, name, ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_after_decl(
        &mut self,
        _span: &Span,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        // We have a special case for `after init`: since this is being emitted
        // as part of the constructor, just inline the mixin's body here.
        let init_name = vec!["init".to_owned()];
        if ast.name == init_name {
            for inner in &mut ast.body {
                inner.visit(self)?.modifying(inner);
            }
            return Ok(ControlMut::SkipSiblings(()));
        }

        // Otherwise, emit the callback to the action instance variable.
        ast.name.visit(self)?.modifying(&mut ast.name);
        self.pp.write_str(".onAfter((")?;

        if let Some(params) = &mut ast.params {
            self.write_paramlist(params, ", ")?;
        }
        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_alias_decl(
        &mut self,
        sym: &mut expressions::Token,
        sort: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        let j: JavaType = (sort as &expressions::Sort).into(); // XXX: poor choices lead to this clone.a
        self.type_aliases.insert(sym.clone(), j);
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_before_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        ast.name.visit(self)?.modifying(&mut ast.name);
        self.pp.write_str(".addBefore((")?;

        if let Some(params) = &mut ast.params {
            self.write_paramlist(params, ",")?;
            // XXX: also the return value needs to be bound.
        }
        self.pp.write_str(") -> {\n")?;
        for stmt in &mut ast.body {
            stmt.visit(self)?.modifying(stmt);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_class_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        ast: &mut declarations::ClassDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        self.emit_class_definition(name, ast)?;
        self.emit_class_factory(name, ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_implement_decl(
        &mut self,
        ast: &mut declarations::ActionMixinDecl,
    ) -> ExtractResult<declarations::Decl> {
        ast.name.visit(self)?.modifying(&mut ast.name);
        self.pp.write_str(".on(")?;

        self.write_lambda(ast.params.as_mut().unwrap(), &mut ast.ret, &mut ast.body)?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_import_decl(
        &mut self,
        ast: &mut declarations::ImportDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("private void ")?;
        self.pp.write_fmt(format_args!("{}(", ast.name))?;

        for param in &mut ast.params {
            self.param(param)?.modifying(param);
        }
        self.pp.write_str(") {\n")?;

        self.pp.write_str("System.out.println(")?;
        self.pp.write_fmt(format_args!("\"{}: \" + ", ast.name))?;
        for (i, param) in &mut ast.params.iter_mut().enumerate() {
            if i > 0 {
                self.pp.write_str(" + \", \" + ")?;
            }
            param.name.visit(self)?.modifying(&mut param.name);
        }
        self.pp.write_str(");")?;

        self.pp.write_str("\n}")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_include_decl(&mut self, ast: &mut Token) -> ExtractResult<Token> {
        self.pp
            .write_fmt(format_args!("import ivy.stdlib.{ast}.*"))?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_instance_decl(
        &mut self,
        name: &mut Token,
        ast: &mut declarations::InstanceDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("class {} extends ", name))?;
        ast.sort.visit(self)?.modifying(&mut ast.sort);
        if !ast.args.is_empty() {
            self.pp.write_str("<")?;

            // maybe slightly confusing: because we parameterise modules on their sorts,
            // the `id` field contains the identifier for sort, not the `sort` field.
            let mut sorts = ast.args.iter().map(|a| a.name.clone()).collect::<Vec<_>>();
            self.write_separated(&mut sorts, ", ")?;
            self.pp.write_str(">")?;
        }

        self.pp.write_str(" {\n}")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_interpret_decl(
        &mut self,
        _name: &mut Token,
        _sort: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_invariant_decl(
        &mut self,
        ast: &mut crate::ast::logic::Fmla,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_str("addConjecture(() -> {\n")?;

        ast.visit(self)?.modifying(ast);

        self.pp.write_str("\n})")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_local_vardecl(
        &mut self,
        name: &mut Token,
        sort: &mut expressions::Sort,
    ) -> ExtractResult<statements::Stmt> {
        self.pp
            .write_fmt(format_args!("{} ", Self::jtype_from_sort(sort).as_jval()))?;
        name.visit(self)?.modifying(name);
        Ok(ControlMut::SkipSiblings(()))
    }

    // Expressions

    fn begin_typedecl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        s: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        if let expressions::Sort::ToBeInferred = s {
            return Ok(ControlMut::SkipSiblings(()));
        }
        if let expressions::Sort::Resolved(IvySort::Enum(discs)) = s {
            for (i, d) in discs.iter().enumerate() {
                self.pp.write_fmt(format_args!("int {} = {};\n", d, i))?;
            }
        }
        self.type_aliases
            .insert(name.clone(), (s as &expressions::Sort).into());
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_vardecl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        sort: &mut expressions::Sort,
    ) -> ExtractResult<declarations::Decl> {
        self.pp
            .write_fmt(format_args!("{} ", Self::jtype_from_sort(sort).as_jval()))?;
        name.visit(self)?.modifying(name);
        Ok(ControlMut::SkipSiblings(()))
    }

    // Expressions

    fn begin_app(&mut self, ast: &mut expressions::AppExpr) -> ExtractResult<expressions::Expr> {
        ast.func.visit(self)?;

        self.pp.write_str("(")?;

        // XXX: this is slightly gnarly and duplicates functionality in
        // assignment-emission code, too - how can we make it better?
        let argsorts = match &ast.func_sort {
            expressions::Sort::ToBeInferred | expressions::Sort::Annotated(_) => {
                panic!("Unresolved application sort {:#?}", ast);
            }
            expressions::Sort::Resolved(is) => match is {
                IvySort::Action(_, ActionArgs::List(args), _, _) => Some(args),
                IvySort::Object(_) => todo!(),
                _ => None,
            },
        };
        if let Some(args) = argsorts {
            for (i, (arg, asort)) in ast.args.iter_mut().zip(args.iter()).enumerate() {
                if i > 0 {
                    self.pp.write_str(", ")?;
                }
                match asort {
                    IvySort::Number => self.write_unbounded_seq(arg)?,
                    IvySort::BoundedSequence(lo, hi) => self.write_bounded_long(arg, *lo, *hi)?,
                    _ => arg.visit(self)?.modifying(arg),
                }
            }
        } else {
            ast.args.visit(self)?.modifying(&mut ast.args);
        }

        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_assign(
        &mut self,
        _span: &Span,
        ast: &mut actions::AssignAction,
    ) -> ExtractResult<actions::Action> {
        ast.lhs.visit(self)?;
        self.pp.write_str(" = ")?;

        match &ast.lhs_sort {
            expressions::Sort::ToBeInferred | expressions::Sort::Annotated(_) => {
                unreachable!("Failed to resolve sort of assignment")
            }
            expressions::Sort::Resolved(ivysort) => match ivysort {
                IvySort::Number => self.write_unbounded_seq(&mut ast.rhs)?,
                IvySort::BoundedSequence(lo, hi) => self.write_bounded_long(&mut ast.rhs, *lo, *hi)?,
                _ => ast.rhs.visit(self)?.modifying(&mut ast.rhs),
            },
        };

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_binop(&mut self, ast: &mut expressions::BinOp) -> ExtractResult<expressions::Expr> {
        ast.lhs.visit(self)?;

        let op_str = match ast.op {
            Verb::Plus => "+",
            Verb::Minus => "-",
            Verb::Times => "*",
            Verb::Div => "/",
            Verb::Dot => ".",

            Verb::Equals => "==",
            Verb::Notequals => "!=",
            Verb::Lt => "<",
            Verb::Le => "<=",
            Verb::Gt => ">",
            Verb::Ge => ">=",

            Verb::And => "&&",
            Verb::Or => "||",
            _ => {
                eprintln!("{:?}", ast.op);
                unimplemented!()
            }
        };
        match ast.op {
            Verb::Dot => {
                self.pp.write_str(op_str)?;
            }
            _ => {
                self.pp.write_fmt(format_args!(" {} ", op_str))?;
            }
        }
        ast.rhs.visit(self)?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_call(&mut self, ast: &mut expressions::AppExpr) -> ExtractResult<actions::Action> {
        self.begin_app(ast)?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_ensure(&mut self, _ast: &mut actions::EnsureAction) -> ExtractResult<actions::Action> {
        self.pp.write_str("ensureThat(")?;
        Ok(ControlMut::Produce(()))
    }
    fn finish_ensure(
        &mut self,
        _ast: &mut actions::EnsureAction,
        _pred_t: (),
    ) -> ExtractResult<actions::Action> {
        self.pp.write_str(")")?;
        Ok(ControlMut::Produce(()))
    }

    fn begin_field_access(
        &mut self,
        lhs: &mut expressions::Expr,
        rhs: &mut expressions::Symbol,
    ) -> ExtractResult<expressions::Expr> {
        lhs.visit(self)?;
        self.pp.write_fmt(format_args!(".{}", rhs.name))?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_index(&mut self, expr: &mut IndexExpr) -> ExtractResult<expressions::Expr> {
        expr.lhs.visit(self)?;
        self.pp.write_str("[")?;
        expr.idx.visit(self)?;
        self.pp.write_str("]")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_module_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        _ast: &mut declarations::ModuleDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        // XXX: stupid hack: if this is a collection that has a Melina implementation,
        // don't emit it.  I hate this.
        if name == &"vector" {
            return Ok(ControlMut::SkipSiblings(()));
        }

        // TODO...
        self.pp.write_fmt(format_args!("class IvyMod_{name}"))?;
        self.pp.write_str(" {\n")?;
        self.pp.write_str("\n}\n")?;
        return Ok(ControlMut::SkipSiblings(()));
    }

    fn begin_object_decl(
        &mut self,
        _span: &Span,
        name: &mut Token,
        ast: &mut declarations::ObjectDecl,
    ) -> ExtractResult<declarations::Decl> {
        self.pp.write_fmt(format_args!("class IvyObj_{name}"))?;
        self.pp.write_str(" {\n")?;

        // Declare all action instance variables.
        for decl in ast.actions() {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("\n")?;

        for param in ast.params() {
            self.pp.write_str("private ")?;
            self.param(param)?;
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("\n")?;

        for decl in ast.vars() {
            self.pp.write_str("private ")?;
            decl.visit(self)?.modifying(decl);
            self.pp.write_str(";\n")?;
        }
        self.pp.write_str("\n")?;

        // Constructor
        self.pp.write_fmt(format_args!("public IvyObj_{name}("))?;
        self.write_paramlist(&mut ast.params, ", ")?;
        self.pp.write_fmt(format_args!(") {{\n"))?;

        for param in &ast.params {
            self.pp
                .write_fmt(format_args!("this.{} = {};\n", param.name, param.name))?;
        }

        self.pp.write_str("\n")?;
        for decl in ast
            .body
            .iter_mut()
            .filter(|d| d.name_for_binding().is_none())
        {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str(";")?;
            //self.pp.write_fmt(format_args!(" // {:?}", decl))?;
            self.pp.write_str("\n")?;
        }

        // Lastly, register all exported actions with the environment.
        for action in ast.actions() {
            if let declarations::Decl::Export { decl, .. } = action {
                match decl {
                    declarations::ExportDecl::Action(binding) => {
                        self.pp
                            .write_fmt(format_args!("\naddAction({});", binding.name))?;
                    }
                    declarations::ExportDecl::ForwardRef(name) => {
                        self.pp.write_fmt(format_args!("\naddAction({name});"))?;
                    }
                }
            }
        }

        self.pp.write_str("\n} //cstr \n")?;

        for decl in ast.subobjects() {
            decl.visit(self)?.modifying(decl);
            self.pp.write_str("\n")?;
        }

        self.pp.write_str("\n")?;

        self.pp.write_str("\n}\n")?;

        if ast.params.is_empty() {
            self.pp.write_fmt(format_args!(
                "IvyObj_{name} {name} = new IvyObj_{name}();\n"
            ))?;
        } else {
            if ast.params.len() > 1 {
                // I actually don't know if this would even be valid Ivy.
                todo!()
            }
            let self_sort = match &ast.params.get(0).unwrap().decl {
                expressions::Sort::Resolved(is) => is,
                _ => unreachable!("argument to object not typechecked"),
            };

            let (lo, hi) = match QuantBounds::bounds_for_sort(self_sort) {
                (
                    Some(logic::Fmla::Number { val: 0, .. }),
                    Some(logic::Fmla::Number { val: hi, .. }),
                ) => (0, hi),
                _ => todo!(),
            };

            self.pp.write_fmt(format_args!(
            "List<IvyObj_{name}> {name}_instances [] = LongStream.range({lo}, {hi}).mapToObj(i -> new IvyObj_{name}(i)).collect(Collectors.toList());\n"
            ))?;
            self.pp.write_fmt(format_args!(
                "Function1<Long, IvyObj_{name}> {name} = i -> {name}_instances.get(i.intValue());\n"
            ))?;
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    // logic

    fn begin_logical_app(
        &mut self,
        ast: &mut logic::LogicApp,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        ast.func.visit(self)?;

        self.pp.write_str("(")?;
        self.write_separated(&mut ast.args, ", ")?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_logical_binop(
        &mut self,
        ast: &mut logic::LogicBinOp,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        let op_str = match ast.op {
            Verb::Plus => "+",
            Verb::Minus => "-",
            Verb::Times => "*",
            Verb::Div => "/",
            Verb::Dot => ".",

            Verb::Equals => "==",
            Verb::Notequals => "!=",
            Verb::Lt => "<",
            Verb::Le => "<=",
            Verb::Gt => ">",
            Verb::Ge => ">=",

            Verb::Arrow => "->",

            Verb::And => "&&",
            Verb::Or => "||",
            _ => {
                eprintln!("{:?}", ast.op);
                unimplemented!()
            }
        };
        match ast.op {
            Verb::Arrow => {
                self.pp.write_str("!(")?;
                ast.lhs.visit(self)?.modifying(&mut ast.lhs);
                self.pp.write_str(") || ")?;
                ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            }
            Verb::Dot => {
                ast.lhs.visit(self)?.modifying(&mut ast.lhs);
                self.pp.write_str(op_str)?;
                ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            }
            _ => {
                ast.lhs.visit(self)?.modifying(&mut ast.lhs);
                self.pp.write_fmt(format_args!(" {} ", op_str))?;
                ast.rhs.visit(self)?.modifying(&mut ast.rhs);
            }
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_logical_field_access(
        &mut self,
        lhs: &mut logic::Fmla,
        rhs: &mut expressions::Symbol,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        lhs.visit(self)?;
        self.pp.write_fmt(format_args!(".{}", rhs.name))?;
        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_logical_unary_op(
        &mut self,
        op: &mut Verb,
        rhs: &mut logic::Fmla,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        match op {
            Verb::Not => self.pp.write_str("!")?,
            Verb::Minus => self.pp.write_str("-")?,
            _ => unimplemented!(),
        };

        if let logic::Fmla::BinOp { .. } = rhs {
            self.pp.write_str("(")?;
            rhs.visit(self)?.modifying(rhs);
            self.pp.write_str(")")?;
            Ok(ControlMut::SkipSiblings(()))
        } else {
            Ok(ControlMut::Produce(()))
        }
    }

    fn begin_forall(
        &mut self,
        fmla: &mut logic::Forall,
    ) -> VisitorResult<(), std::fmt::Error, logic::Fmla> {
        let mut qb = QuantBounds::new_forall();

        // XXX: unfortunate that we have to mix Visitable logic in a Visitor.
        // Suggests that compositionality of visitors needs consideration.
        qb.begin_forall(fmla)?.and_then(|_| {
            let vars_t = fmla.vars.visit(&mut qb)?.modifying(&mut fmla.vars);
            let fmla_t = fmla.fmla.visit(&mut qb)?.modifying(&mut fmla.fmla);
            qb.finish_forall(fmla, vars_t, fmla_t)
        })?;

        for (i, var) in fmla.vars.iter().enumerate() {
            let (mut lo, mut hi) = (None, None);
            // XXX: This is almost certainly suboptimal for more complicated formulae.
            // But, this is what ivy_to_cpp does, so ¯\_(ツ)_/¯
            for (l, h) in qb.bounds.get(&var.name).unwrap() {
                match (&lo, l) {
                    (None, Some(_)) => {
                        lo = l.clone();
                    }
                    _ => (),
                };
                match (&hi, h) {
                    (None, Some(_)) => {
                        hi = h.clone();
                    }
                    _ => (),
                };
                log::debug!(target: "quantifier-bounds", "{}: [{:?}, {:?})", var.name, lo, hi);
                match (&lo, &hi) {
                    (Some(_), Some(_)) => break,
                    _ => (),
                }
            }

            // XXX: return an ExtractionError or something rather than
            // std::Fmt::Error so we don't just panic here.
            assert!(lo.is_some(), "Can't find a lower bound");
            assert!(hi.is_some(), "Can't find an upper bound");

            let (mut lo, mut hi) = (lo.unwrap(), hi.unwrap());

            self.pp.write_str("IntStream.range(")?;
            lo.visit(self)?;
            self.pp.write_str(", ")?;
            hi.visit(self)?;
            self.pp.write_str(")")?;

            if i < fmla.vars.len() - 1 {
                self.pp.write_str(".flatMap(")?;
            } else {
                self.pp.write_str(".allMatch(")?;
            }
            self.pp
                .write_fmt(format_args!("{} -> {{ \nreturn ", var.name))?;
        }

        fmla.fmla.visit(self)?.modifying(&mut fmla.fmla);

        //self.pp.write_str("\n")?;
        //self.pp.write_str("return true;")?;

        for _ in &fmla.vars {
            self.pp.write_str(";\n})")?;
        }

        Ok(ControlMut::SkipSiblings(()))
    }

    // Terminals

    fn boolean(&mut self, b: &mut bool) -> ExtractResult<bool> {
        if *b {
            self.pp.write_str("true")?;
        } else {
            self.pp.write_str("false")?;
        }
        Ok(ControlMut::Produce(()))
    }

    fn identifier(&mut self, i: &mut expressions::Ident) -> ExtractResult<expressions::Ident> {
        self.write_separated(i, ".")?;
        Ok(ControlMut::Produce(()))
    }

    fn number(&mut self, _span: &Span, n: &mut i64) -> ExtractResult<i64> {
        self.pp.write_str(&n.to_string())?;
        Ok(ControlMut::Produce(()))
    }

    fn param(&mut self, p: &mut expressions::Symbol) -> ExtractResult<expressions::Symbol> {
        self.sort(&mut p.decl)?;
        self.pp.write_str(" ")?;
        self.pp.write_str(&p.name)?;
        Ok(ControlMut::Produce(()))
    }

    fn sort(&mut self, s: &mut expressions::Sort) -> ExtractResult<expressions::Sort> {
        match s {
            expressions::Sort::ToBeInferred => (),
            expressions::Sort::Annotated(ident) => {
                ident.visit(self)?.modifying(ident);
            }
            expressions::Sort::Resolved(ivysort) => {
                let j: JavaType = ivysort.clone().into(); // XXX: poor choices lead to this clone.a
                self.pp.write_str(j.as_jref().as_str())?;
            }
        }
        Ok(ControlMut::Produce(()))
    }

    fn symbol(
        &mut self,
        _span: &Span,
        p: &mut expressions::Symbol,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Symbol> {
        self.token(&mut p.name)?.modifying(&mut p.name);
        Ok(ControlMut::Produce(()))
    }

    fn token(&mut self, s: &mut expressions::Token) -> ExtractResult<expressions::Token> {
        let alias = self.type_aliases.get_mut(s);
        match alias {
            None => self.pp.write_str(s)?,
            Some(jsort) => {
                // TODO: the reference type is safe but it'd be nice to
                // avoid boxing: can we safely drop to the value type?
                self.pp.write_str(jsort.as_jref().as_str())?;
            }
        }
        Ok(ControlMut::Produce(()))
    }

    fn this(&mut self) -> ExtractResult<expressions::Expr> {
        self.pp.write_str("this")?;
        Ok(ControlMut::Produce(()))
    }
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum JExtractionError {
    #[error("Symbol {0:?} failed to have a type inferred (did the typechecking pass run?)")]
    UnresolvedType(expressions::Token),
}
