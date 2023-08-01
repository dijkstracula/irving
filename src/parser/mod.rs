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
    let user_data = body.to_string().into();
    IvyParser::parse_with_userdata(Rule::prog, body, user_data)
        .and_then(|res| res.single())
        .and_then(IvyParser::prog)
}
