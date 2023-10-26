use std::{collections::BTreeMap, fmt::Write};

use crate::{
    ast::{
        declarations::{self, MapDecl},
        expressions, logic, span,
    },
    visitor::{ast::Visitable, *},
};

use super::{pprint::PrettyPrinter, ExtractResult};

pub struct Extractor<W>
where
    W: Write,
{
    pub pp: PrettyPrinter<W>,
    pub emitted_relations: BTreeMap<MapDecl, usize>,
    pub emitted_lvars: BTreeMap<expressions::Token, usize>,
}

impl Extractor<String> {
    pub fn new() -> Self {
        Self {
            pp: PrettyPrinter::new(),
            emitted_relations: BTreeMap::new(),
            emitted_lvars: BTreeMap::new(),
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

    fn lvar(sym: &str, id: &usize) -> String {
        format!("{sym}{id}")
    }

    fn array_sort(id: &usize) -> String {
        format!("Arr{id}")
    }

    fn const_fun(id: &usize) -> String {
        format!("Const{id}")
    }

    fn read_fun(id: &usize) -> String {
        format!("Read{id}")
    }

    fn write_fun(id: &usize) -> String {
        format!("Write{id}")
    }

    fn next_sort(sort: &String) -> String {
        format!("{sort}_next")
    }
}

impl<W> ast::Visitor<(), std::fmt::Error> for Extractor<W>
where
    W: Write,
{
    fn begin_app(&mut self, ast: &mut expressions::AppExpr) -> ExtractResult<expressions::Expr> {
        ast.func.visit(self)?;

        self.pp.write_str("(")?;
        self.write_separated(&mut ast.args, ", ")?;
        self.pp.write_str(")")?;
        Ok(ControlMut::SkipSiblings(()))
    }

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

    // Declarations

    fn begin_map_decl(
        &mut self,
        _span: &span::Span,
        name: &mut expressions::Token,
        ast: &mut declarations::MapDecl,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        assert!(ast.domain.len() == 1);
        let mut dom = &mut ast.domain.get_mut(0).unwrap().decl.clone();

        if !self.emitted_relations.contains_key(ast) {
            let new_id = self.emitted_relations.len() + 1;
            self.emitted_relations.insert(ast.clone(), new_id);
            let array_sort = Self::array_sort(&new_id);

            self.pp
                .write_fmt(format_args!("(declare-sort {} 0)\n", array_sort))?;
            self.pp.write_fmt(format_args!(
                "(declare-fun {} (Int) {})\n",
                Self::const_fun(&new_id),
                array_sort
            ))?;
            self.pp.write_fmt(format_args!(
                "(declare-fun {} ({} Int) Int)\n",
                Self::read_fun(&new_id),
                array_sort,
            ))?;

            self.pp.write_fmt(format_args!(
                "(declare-fun {} ({} Int ",
                Self::write_fun(&new_id),
                array_sort,
            ))?;
            self.sort(&mut dom)?.modifying(&mut dom);
            self.pp.write_fmt(format_args!(") {})\n", array_sort,))?;
            self.pp.write_str("\n")?;
        }

        let array_sort = Self::array_sort(self.emitted_relations.get(ast).unwrap());

        self.pp
            .write_fmt(format_args!("(declare-fun {} () {})\n", name, array_sort))?;
        self.pp.write_fmt(format_args!(
            "(declare-fun {} () {})\n",
            Self::next_sort(name),
            array_sort
        ))?;
        self.pp.write_fmt(format_args!(
            "(define-fun .{} () (! {} :next {}))\n",
            name,
            name,
            Self::next_sort(name)
        ))?;
        self.pp.write_str("\n")?;

        Ok(ControlMut::SkipSiblings(()))
    }

    fn begin_typedecl(
        &mut self,
        _span: &span::Span,
        _name: &mut expressions::Token,
        _ast: &mut expressions::Sort,
    ) -> VisitorResult<(), std::fmt::Error, declarations::Decl> {
        Ok(ControlMut::Produce(()))
    }

    // Terminals

    fn sort(
        &mut self,
        s: &mut expressions::Sort,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Sort> {
        match s {
            expressions::Sort::ToBeInferred => self.pp.write_str("Int")?,
            expressions::Sort::Annotated(_ident) => todo!(),
            expressions::Sort::Resolved(is) => match is {
                crate::typechecker::sorts::IvySort::Uninterpreted => self.pp.write_str("Int")?,
                crate::typechecker::sorts::IvySort::This => todo!(),
                crate::typechecker::sorts::IvySort::Unit => todo!(),
                crate::typechecker::sorts::IvySort::Top => todo!(),
                crate::typechecker::sorts::IvySort::Bool => self.pp.write_str("Bool")?,
                crate::typechecker::sorts::IvySort::Number => todo!(),
                crate::typechecker::sorts::IvySort::BitVec(_) => todo!(),
                crate::typechecker::sorts::IvySort::Vector(_) => todo!(),
                crate::typechecker::sorts::IvySort::Range(_, _) => todo!(),
                crate::typechecker::sorts::IvySort::Enum(_) => todo!(),
                crate::typechecker::sorts::IvySort::Action(_, _, _, _) => todo!(),
                crate::typechecker::sorts::IvySort::Map(_, _) => todo!(),
                crate::typechecker::sorts::IvySort::Class(_) => todo!(),
                crate::typechecker::sorts::IvySort::Module(_) => todo!(),
                crate::typechecker::sorts::IvySort::Object(_) => todo!(),
                crate::typechecker::sorts::IvySort::SortVar(_) => self.pp.write_str("Int")?,
            },
        }
        Ok(ControlMut::Produce(()))
    }

    fn param(
        &mut self,
        p: &mut expressions::Symbol,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Symbol> {
        if !self.emitted_lvars.contains_key(&p.name) {
            let new_id = self.emitted_lvars.len() + 1;
            self.emitted_lvars.insert(p.name.clone(), new_id);

            let lvar = Self::lvar(&p.name, &new_id);

            self.pp
                .write_fmt(format_args!("(declare-fun {} () ", lvar))?;
            p.decl.visit(self)?.modifying(&mut p.decl);
            self.pp.write_str(")\n")?;

            self.pp
                .write_fmt(format_args!("(declare-fun {} () ", Self::next_sort(&lvar)))?;
            p.decl.visit(self)?.modifying(&mut p.decl);
            self.pp.write_str(")\n")?;

            self.pp
                .write_fmt(format_args!("(define-fun .{} () ", lvar))?;
            p.decl.visit(self)?.modifying(&mut p.decl);
            self.pp.write_fmt(format_args!(
                "(! {} :next {})",
                lvar,
                Self::next_sort(&lvar)
            ))?;
            self.pp.write_str(")\n")?;
            self.pp.write_str("\n")?;
        }
        match self.emitted_lvars.get(&p.name) {
            None => self.pp.write_str(&p.name)?,
            Some(id) => self.pp.write_str(&Self::lvar(&p.name, id))?,
        }

        Ok(ControlMut::Produce(()))
    }

    fn symbol(
        &mut self,
        _span: &span::Span,
        p: &mut expressions::Symbol,
    ) -> VisitorResult<(), std::fmt::Error, expressions::Symbol> {
        self.pp.write_str(&p.name)?;
        Ok(ControlMut::Produce(()))
    }
}
