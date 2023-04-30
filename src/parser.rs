use pest_consume::{Parser, Error, match_nodes};

#[derive(Parser)]
#[grammar = "grammars/lexer.pest"]
#[grammar = "grammars/syntax.pest"]
pub struct IvyParser;

type Result<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[pest_consume::parser]
#[allow(dead_code)]
impl IvyParser {
    fn EOI(_input: Node) -> Result<()> {
        Ok(())
    }

    fn number(input: Node) -> Result<i64> {
        input.as_str()
            .parse::<i64>()
            .map_err(|_| input.error("Expected number"))
    }

    pub fn langver(input: Node) -> Result<(u8, u8)> {
        match_nodes!(
        input.clone().into_children(); // XXX: avoid the clone?
        [number(major)] => {
            if major >= 255 {
                Err(input.error(format!("Invalid major version number {:?}", major)))
            } else {
                Ok((major as u8, 0))
            }
        },
        [number(major), number(minor)] => {
            if major >= 255 {
                Err(input.error(format!("Invalid major version number {:?}.{:?}", major, minor)))
            } else if minor >= 255 { 
                Err(input.error(format!("Invalid minor version number {:?}.{:?}", major, minor)))
            } else {
                Ok((major as u8, minor as u8))
            }
        })
    }
}