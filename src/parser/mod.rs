pub(crate) mod expressions;
pub(crate) mod ivy;
pub(crate) mod logic;

use std::{path::PathBuf, rc::Rc};

use pest::error;
use pest_consume::Parser;

use crate::{
    ast::toplevels::Prog,
    parser::ivy::{IvyParser, Rule},
};

use self::ivy::ParserState;

pub fn prog_from_str(file_name: PathBuf, file_text: String) -> Result<Prog, error::Error<Rule>> {
    let user_data = Rc::new(ParserState::new(file_name, file_text));

    IvyParser::parse_with_userdata(
        Rule::prog,
        Rc::clone(&user_data.file_text).as_str(),
        user_data,
    )
    .and_then(|res| res.single())
    .and_then(IvyParser::prog)
}
