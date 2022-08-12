/// https://deepsource.io/blog/lightweight-linting/
/// https://tree-sitter.github.io/tree-sitter/playground
#[allow(unused_imports)]
use tree_sitter::{InputEdit, Node, Parser, Point, Query, QueryCursor, QueryMatches, Tree};

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

pub fn load_code(parser: Option<&mut Parser>, rows: &[String]) -> Option<Tree> {
    let source_utf16: Vec<u16> = std::str::from_utf8(rows.join("\n").as_bytes())
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

fn find_numbers(rows: &[String], tree: &Tree) -> Vec<(Point, Point)> {
    let source = rows.join("\n");
    let mut cursor = QueryCursor::new();
    return cursor
        .captures(&query_rust_number(), tree.root_node(), source.as_bytes())
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
        .map(String::from);

        let tree = load_code(Some(&mut rust()), &source_utf8).unwrap();
        let positions = find_numbers(&source_utf8, &tree);

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
