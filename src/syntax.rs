use tree_sitter::{Parser, Tree};

pub fn c() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_c::language())
        .expect("Error loading C grammar");
    parser
}

pub fn rust() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_rust::language())
        .expect("Error loading rust grammar");
    parser
}

pub fn load_code(parser: Option<&mut Parser>, rows: &[String]) -> Option<Tree> {
    parser.and_then(|p| p.parse(rows.join("\n").as_bytes(), None))
}
