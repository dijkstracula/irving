pub(crate) mod expressions;
pub(crate) mod ivy;
pub(crate) mod logic;

use pest::error;
use pest_consume::Parser;

use crate::{
    ast::toplevels::Prog,
    parser::ivy::{IvyParser, Rule},
};

pub fn prog_from_str(body: &str) -> Result<Prog, error::Error<Rule>> {
    IvyParser::parse(Rule::prog, body)
        .and_then(|res| res.single())
        .and_then(|prog| IvyParser::prog(prog))
}
