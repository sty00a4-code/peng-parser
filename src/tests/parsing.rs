use crate::{error::Error, parse_file, arguments, location::position::Located};

#[test]
fn match_block() -> Result<(), Error> {
    let ast = parse_file("src/tests/files/match_block.peng", &arguments::Arguments::new().flag("ast"))?;
    use crate::parser::ast::*;
    let Chunk(nodes) = ast;
    if let Statment::Match(Located { item: expr, pos: _ }, match_cases, Some(Located { item: else_case, pos: _ })) = &nodes[0].item {
        if let Expression::Atom(Located { item: Atom::Path(Located { item: Path::ID(id), pos: _ }), pos: _ }) = expr {
            assert_eq!(id.0.as_str(), "a");
        } else { panic!() }
        let MatchCase { pattern: Located { item: Pattern::Atom(Located { item: Atom::Int(1), pos: _ }), pos: _ }, guard: None, body } = &match_cases[0].item else { panic!() };
    } else { panic!() }
    Ok(())
}