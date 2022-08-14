use crate::editor_update_row;
use crate::editor_update_syntax;
use unicode_segmentation::UnicodeSegmentation;

pub const TAB_STOP: u16 = 4;

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
    /// Name of the type displayed in the  status bar
    pub file_type: &'static str,
    /// Patterns to match the filename against
    pub file_match: &'static [&'static str],
    /// Highlights information for the type
    pub flags: u8,
    pub keyword_reserved: &'static [&'static str],
    pub keyword_type: &'static [&'static str],
    /// Marker for start of comment
    pub single_line_comment_start: Option<&'static str>,
    pub multi_line_comment_markers: Option<(&'static str, &'static str)>,
    /// Parser
    pub parser_fn: &'static dyn Fn() -> tree_sitter::Parser,
}

pub struct Row {
    pub chars: Vec<String>,
    pub render: Vec<String>,
    pub hl: Vec<Highlight>,
    pub hl_close_comment: bool,
    pub hl_open_comment: bool,
}

impl Row {
    pub fn from_vec(syntax_opt: Option<&Syntax>, chars: Vec<String>, is_comment_open: bool) -> Row {
        let render = editor_update_row(TAB_STOP, &chars);
        let hl = Vec::with_capacity(chars.len());
        let mut row = Row {
            chars,
            render,
            hl,
            hl_open_comment: false,
            hl_close_comment: false,
        };
        editor_update_syntax(syntax_opt, &mut row, is_comment_open);
        row
    }

    pub fn from_str(syntax_opt: Option<&Syntax>, chars: &str, is_comment_open: bool) -> Row {
        let chars = chars
            .graphemes(true)
            .map(String::from)
            .collect::<Vec<String>>();
        Row::from_vec(syntax_opt, chars, is_comment_open)
    }

    pub fn is_comment_open(&self) -> bool {
        self.hl_open_comment && !self.hl_close_comment
    }
}
