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

pub struct Syntax {
    hl_rules: Vec<(Highlight, Query)>,
    pub mode_name: &'static str,
    parser: Parser,
    tree: Option<Tree>,
}

/// https://github.com/tree-sitter/tree-sitter-c
fn c() -> Syntax {
    let mode_name = "C";
    let hl_rules: Vec<(Highlight, Query)> = vec![
        (
            Highlight::Number,
            Query::new(
                tree_sitter_c::language(),
                "[(number_literal) (char_literal)] @number",
            )
            .expect("Malformed query"),
        ),
        (
            Highlight::String,
            Query::new(
                tree_sitter_c::language(),
                "[(string_literal) (system_lib_string)] @string",
            )
            .expect("Malformed query"),
        ),
    ];
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_c::language())
        .expect("Error loading C grammar");
    let tree = None;
    Syntax {
        hl_rules,
        mode_name,
        parser,
        tree,
    }
}

/// https://github.com/tree-sitter/tree-sitter-rust
fn rust() -> Syntax {
    let mode_name = "rust";
    let hl_rules: Vec<(Highlight, Query)> = vec![
        (
            Highlight::Number,
            Query::new(
                tree_sitter_rust::language(),
                "[ (integer_literal) (float_literal) ] @number",
            )
            .expect("Malformed query"),
        ),
        (
            Highlight::String,
            Query::new(
                tree_sitter_rust::language(),
                "[ (string_literal) (raw_string_literal) (char_literal) ] @string",
            )
            .expect("Malformed query"),
        ),
    ];
    let mut parser = Parser::new();
    parser
        .set_language(tree_sitter_rust::language())
        .expect("Error loading rust grammar");
    let tree = None;
    Syntax {
        hl_rules,
        mode_name,
        parser,
        tree,
    }
}

pub fn select_syntax_highlight(extension: &str) -> Option<Syntax> {
    match extension {
        "c" | "h" | "cpp" => Some(c()),
        "rs" => Some(rust()),
        _ => None,
    }
}

/// Uses UTF16 representation to make it easier to extract Position for wide
/// charaters in utf8 text
pub fn load_code(syntax_opt: Option<&mut Syntax>, source: &[u8]) {
    if let Some(mut syntax) = syntax_opt {
        let source_utf16: Vec<u16> = std::str::from_utf8(source)
            .unwrap()
            .encode_utf16()
            .collect();
        syntax.tree = syntax.parser.parse_utf16(source_utf16, None);
    }
}

fn find_tokens(source: &[u8], tree: &Tree, query: &Query) -> Vec<(Point, Point)> {
    let mut cursor = QueryCursor::new();
    return cursor
        .captures(query, tree.root_node(), source)
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

pub fn highlight(syntax_opt: Option<&Syntax>, source: &[u8], hs: &mut [&mut Vec<Highlight>]) {
    if syntax_opt.is_none() || syntax_opt.unwrap().tree.is_none() {
        return;
    }

    let syntax = syntax_opt.unwrap();
    let tree = syntax.tree.as_ref().unwrap();
    for (hl, query) in &syntax.hl_rules {
        let markers = find_tokens(source, tree, query);
        for (start, end) in markers {
            if let Some(h) = hs.get_mut(start.row) {
                for i in start.column..end.column {
                    h[i] = *hl;
                }
            }
        }
    }
}

#[cfg(test)]
mod test_manual_highlight {
    use super::{find_tokens, load_code, rust};
    use tree_sitter::Point;

    #[test]
    fn test_parsing_numbers() {
        let mut syntax = rust();

        let source_utf8 = [
            "const ØØ:u32 = 123;",
            "const 안:f32 = 4.5;",
            "const AA:u32 = 678;",
        ]
        .join("\n");

        load_code(Some(&mut syntax), &source_utf8.as_bytes());
        let positions = find_tokens(
            &source_utf8.as_bytes(),
            &syntax.tree.unwrap(),
            &syntax.hl_rules[0].1,
        );

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
