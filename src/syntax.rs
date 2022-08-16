/// https://docs.rs/tree-sitter/latest/tree_sitter/
/// https://deepsource.io/blog/lightweight-linting/
/// https://tree-sitter.github.io/tree-sitter/playground
use tree_sitter::{Parser, Point, Query, QueryCursor, Tree};

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, std::cmp::PartialEq)]
pub enum Highlight {
    Comment,
    CommentMultiline,
    KeywordReserved,
    KeywordType,
    Match,
    Normal,
    Number,
    String,
}

pub fn c() -> Parser {
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_c::language())
        .expect("Error loading C grammar");
    parser
}

pub fn rust() -> Parser {
    let mut parser = Parser::new();
    // let node_types = tree_sitter_rust::NODE_TYPES;
    parser
        .set_language(tree_sitter_rust::language())
        .expect("Error loading rust grammar");
    parser
}

/// Uses UTF16 representation to make it easier to extract Position for wide
/// charaters in utf8 text
pub fn load_code(parser: Option<&mut Parser>, source: &[u8]) -> Option<Tree> {
    let source_utf16: Vec<u16> = std::str::from_utf8(source)
        .unwrap()
        .encode_utf16()
        .collect();

    parser.and_then(|p| p.parse_utf16(source_utf16, None))
}

fn query_rust_number() -> Query {
    Query::new(
        tree_sitter_rust::language(),
        "[ (integer_literal) (float_literal) ] @number",
    )
    .expect("Malformed query")
}

fn find_numbers(source: &[u8], tree: &Tree) -> Vec<(Point, Point)> {
    let mut cursor = QueryCursor::new();
    return cursor
        .captures(&query_rust_number(), tree.root_node(), source)
        .map(|(query_match, capture_index)| query_match.captures[capture_index].node)
        .map(|node| {
            let start_position = node.start_position();
            let end_position = node.end_position();
            (
                Point::new(start_position.row, start_position.column / 2),
                Point::new(end_position.row, end_position.column / 2),
            )
        })
        .collect::<Vec<(Point, Point)>>();
}

pub fn highlight(source: &[u8], hs: &mut [&mut Vec<Highlight>], tree: &Tree) {
    let markers = find_numbers(source, tree);
    for (start, end) in markers {
        if let Some(h) = hs.get_mut(start.row) {
            for i in start.column..end.column {
                h[i] = Highlight::Number;
            }
        }
    }
}

#[cfg(test)]
#[allow(unused_imports)]
mod test_node_types_parsing {
    use super::{find_numbers, load_code, rust};
    use tree_sitter::{InputEdit, Parser, Point, Query, QueryCursor, QueryMatches, Tree};

    #[test]
    fn test_parsing_double() {
        let source_utf8 = [
            "const ØØ:u32 = 123;",
            "const 안:f32 = 4.5;",
            "const AA:u32 = 678;",
        ]
        .join("\n");

        let tree = load_code(Some(&mut rust()), &source_utf8.as_bytes()).unwrap();
        let positions = find_numbers(&source_utf8.as_bytes(), &tree);

        assert_eq!(
            vec![
                (Point::new(0, 15), Point::new(0, 18)),
                (Point::new(1, 14), Point::new(1, 17)),
                (Point::new(2, 15), Point::new(2, 18)),
            ],
            positions
        );
    }
}
