/*** includes ***/

extern crate nix;
extern crate termios;

use nix::unistd::{read, write};
use std::cmp::min;
use std::os::unix::io::RawFd;
use std::time::{Duration, Instant};
use termios::os::target::{VMIN, VTIME};
use termios::{
    tcgetattr, tcsetattr, Termios, BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, INPCK, ISIG, ISTRIP,
    IXON, OPOST, TCSAFLUSH,
};

/*** define ***/

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const PKG_VERSION: &str = env!("CARGO_PKG_VERSION");
const TAB_STOP: u16 = 4;
const QUIT_TIMES: u8 = 3;

const CTRL_MASK: u8 = 0x60;

/// Ctrl-chars are marked by their bits 5 and 6 set to `0`. Using a Bitwise-AND
/// with `0x60` will return `0x00` for such chars.
const fn is_ctrl(c: u8) -> bool {
    0x00 == c & CTRL_MASK
}

#[derive(Debug, std::cmp::PartialEq)]
enum Direction {
    Down,
    Left,
    Right,
    Up,
}

#[derive(Debug, std::cmp::PartialEq)]
enum Key {
    Arrow(Direction),
    Backspace,
    Char(char),
    Ctrl(char),
    Delete,
    End,
    Enter,
    Escape,
    Home,
    Page(Direction),
}

const BACKSPACE: u8 = 127;

const CLEAR_LINE: &[u8; 3] = b"\x1b[K";
const CLEAR_SCREEN: &[u8; 4] = b"\x1b[2J";
const CURSOR_HOME: &[u8; 3] = b"\x1b[H";
const HIDE_CURSOR: &[u8; 6] = b"\x1b[?25l";
const INVERT_COLOUR: &[u8; 4] = b"\x1b[7m";
const NORMAL_FORMAT: &[u8; 3] = b"\x1b[m";
const SHOW_CURSOR: &[u8; 6] = b"\x1b[?25h";

#[allow(dead_code)]
mod colors {
    pub const BLACK_FOREGROUND: &[u8; 9] = b"\x1b[38;5;0m";
    pub const RED_FOREGROUND: &[u8; 9] = b"\x1b[38;5;1m";
    pub const GREEN_FOREGROUND: &[u8; 9] = b"\x1b[38;5;2m";
    pub const YELLOW_FOREGROUND: &[u8; 9] = b"\x1b[38;5;3m";
    pub const BLUE_FOREGROUND: &[u8; 9] = b"\x1b[38;5;4m";
    pub const MAGENTA_FOREGROUND: &[u8; 9] = b"\x1b[38;5;5m";
    pub const CYAN_FOREGROUND: &[u8; 9] = b"\x1b[38;5;6m";
    pub const WHITE_FOREGROUND: &[u8; 9] = b"\x1b[38;5;7m";
    pub const DEFAULT_FOREGROUND: &[u8; 5] = b"\x1b[39m";
}

const HL_HIGHLIGHT_NUMBERS: u8 = 1 << 0;
const HL_HIGHLIGHT_STRINGS: u8 = 1 << 1;

/*** data ***/

type Line = u32;
type Column = u32;

/// Defines an application specific `Error` which will be used to wrap and
/// manage the errors from the different used libraries.
#[derive(Debug)]
enum Error {
    IO(std::io::Error),
    Nix(nix::Error),
    ParseInt(std::num::ParseIntError),
    Simple(Line, Column),
}

macro_rules! simple_error {
    () => {
        Error::Simple(line!(), column!())
    };
}

macro_rules! simple_err {
    () => {
        Result::Err(simple_error!())
    };
}

impl From<std::io::Error> for Error {
    fn from(error: std::io::Error) -> Self {
        Error::IO(error)
    }
}

impl From<nix::Error> for Error {
    fn from(error: nix::Error) -> Self {
        Error::Nix(error)
    }
}

impl From<std::num::ParseIntError> for Error {
    fn from(error: std::num::ParseIntError) -> Self {
        Error::ParseInt(error)
    }
}

enum Event {
    CursorMove(Direction, u16),
    DeleteBackwardChar,
    DeleteForwardChar,
    Find,
    InsertChar(char),
    InsertNewline,
    None,
    Quit,
    Save,
}

#[derive(Clone, Copy, Debug, std::cmp::PartialEq)]
enum Highlight {
    Comment,
    CommentMultiline,
    KeywordReserved,
    KeywordType,
    Match,
    Normal,
    Number,
    String,
}

struct EditorSyntax {
    /// Name of the type displayed in the  status bar
    file_type: &'static str,
    /// Patterns to match the filename against
    file_match: &'static [&'static str],
    /// Highlights information for the type
    flags: u8,
    keyword_reserved: &'static [&'static str],
    keyword_type: &'static [&'static str],
    /// Marker for start of comment
    single_line_comment_start: Option<&'static str>,
    multi_line_comment_markers: Option<(&'static str, &'static str)>,
}

struct ERow {
    chars: String,
    render: String,
    hl: Vec<Highlight>,
    hl_close_comment: bool,
    hl_open_comment: bool,
}

impl ERow {
    fn from(syntax_opt: Option<&EditorSyntax>, chars: &str, is_comment_open: bool) -> ERow {
        let chars = chars.to_string();
        let render = editor_update_row(TAB_STOP, &chars);
        let hl = Vec::with_capacity(chars.len());
        let mut row = ERow {
            chars,
            render,
            hl,
            hl_open_comment: false,
            hl_close_comment: false,
        };
        editor_update_syntax(syntax_opt, &mut row, is_comment_open);
        row
    }

    fn is_comment_open(&self) -> bool {
        self.hl_open_comment && !self.hl_close_comment
    }
}

fn is_row_comment_open(rows: &[ERow], cy: usize) -> bool {
    if 0 == cy {
        false
    } else {
        rows.get(cy - 1).map_or(false, ERow::is_comment_open)
    }
}

struct StatusMessage {
    message: String,
    time: Instant,
}

#[allow(dead_code)]
struct EditorConfig {
    cursor: (u16, u16),  // chars cursor
    rcursor: (u16, u16), // render cursor
    original_termios: Termios,
    col_offset: u16,
    row_offset: u16,
    rows: Vec<ERow>,
    dirty: bool,
    stdin: RawFd,
    stdout: RawFd,
    screen_rows: u16,
    screen_cols: u16,
    filename: Option<String>,
    status_message: Option<StatusMessage>,
    syntax: Option<&'static EditorSyntax>,
}

impl Drop for EditorConfig {
    fn drop(&mut self) {
        match write(self.stdout, CLEAR_SCREEN) {
            Ok(_) => (),
            Err(_) => eprintln!("Couldn't clear screen"),
        }
        match write(self.stdout, CURSOR_HOME) {
            Ok(_) => (),
            Err(_) => eprintln!("Couldn't move cursor back"),
        }
        disable_raw_mode(self.stdin, self.original_termios).expect("Couldn't disable raw mode");
    }
}

/*** filetypes ***/

const C_HL_EXT: [&str; 3] = ["c", "h", "cpp"];
const RS_HL_EXT: [&str; 1] = ["rs"];

const C_HL_KW_RESERVED: [&str; 15] = [
    "break", "case", "class", "continue", "else", "enum", "for", "if", "return", "static",
    "struct", "switch", "typedef", "union", "while",
];
const C_HL_KW_TYPE: [&str; 8] = [
    "char", "double", "float", "int", "long", "signed", "unsigned", "void",
];
const RS_HL_KW_RESERVED: [&str; 51] = [
    "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn", "for",
    "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref", "return",
    "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe", "use", "where",
    "while", "async", "await", "dyn", "abstract", "become", "box", "do", "final", "macro",
    "override", "priv", "typeof", "unsized", "virtual", "yield", "try",
];
const RS_HL_KW_TYPE: [&str; 17] = [
    "bool", "char", "f32", "f64", "i128", "i16", "i32", "i64", "i8", "isize", "str", "u128", "u16",
    "u32", "u64", "u8", "usize",
];

const HLDB: &[EditorSyntax] = &[
    EditorSyntax {
        file_type: "C",
        file_match: &C_HL_EXT,
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        keyword_reserved: &C_HL_KW_RESERVED,
        keyword_type: &C_HL_KW_TYPE,
        single_line_comment_start: Some("//"),
        multi_line_comment_markers: Some(("/*", "*/")),
    },
    EditorSyntax {
        file_type: "rust",
        file_match: &RS_HL_EXT,
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        keyword_reserved: &RS_HL_KW_RESERVED,
        keyword_type: &RS_HL_KW_TYPE,
        single_line_comment_start: Some("//"),
        multi_line_comment_markers: Some(("/*", "*/")),
    },
];

/*** terminal ***/

/// The `ECHO` feature prints each key typed in the terminal. This is the
/// default behaviour in cannonical mode. This function makes sure the feature
/// is deactivated. The `ICANON` flag is used to deactivate the canonical mode.
/// This will allow the input to be read byte-by-byte instead of line-by-line.
/// The `IEXTEN` flag is used to deactivate the additional special characters
/// such as `EOL2` or `LNEXT`. The `ISIG` flag is used to deactivate the signal
/// chars. The program will be able to process the `ÌNTR`, `QUIT` etc.
/// characters as inputs instead of signals. The `ICRNL` flag is used to
/// deactivate the transformation of all carriage return characters to newlines.
/// The `IXON` flag is used to deactivate the software control flow control
/// characters (C-s and C-q). Those were used to pause transmission of input.
/// The `OPOST` flag is used to turn off the output processing features (such as
/// moving the cursor back to the begining of the line when the carriage return
/// '\r' input is received). Because of the cursor needs to be explicitly
/// "returned" to the begining of the line when calling `print!`. The `BRKINT`,
/// `INPCK` and `ISTRIP` flags and the `CS8` bitmask are all legacy features
/// that are most likely turned off by default.
///
/// The array `c_cc` contains the "control characters" settings. The `VMIN`
/// value defines the minimum number of bytes need before `read()` can return.
/// Setting it to `0` means the `read()` will return as soon as there is an
/// input to be read. The `VTIME` value defines the amount of time `read()` will
/// wait for an input, in 1/10 of a second. If no input has been provided before
/// the timeout `read()` will return `0` (i.e. no bytes read).
///
/// Terminal attributes can be read with `tcgetattr` and changed with
/// `tcsetattr`. `TCSAFLUSH` specifies that the changes will be applied once all
/// pending output have been written to the terminal and discards any unread
/// inputs.
fn enable_raw_mode(fd: RawFd, mut termios: Termios) -> Result<(), std::io::Error> {
    tcgetattr(fd, &mut termios)?;
    termios.c_cflag |= CS8;
    termios.c_iflag &= !(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    termios.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    termios.c_oflag &= !(OPOST);
    termios.c_cc[VMIN] = 0;
    termios.c_cc[VTIME] = 1;

    // Returns Result::Ok if none of the previous function calls triggered an
    // error. Errors will get automatically propagated thanks to the `?` try
    // operator.
    tcsetattr(fd, TCSAFLUSH, &termios)
}

/// Sets the ternimal attributes back to the original
fn disable_raw_mode(fd: RawFd, original: Termios) -> Result<(), std::io::Error> {
    tcsetattr(fd, TCSAFLUSH, &original)
}

fn editor_read_key(editor_config: &EditorConfig) -> Result<Key, Error> {
    let stdin = editor_config.stdin;
    let mut buffer = [0u8; 1];
    // nix's `read` implementation reads a maximum of as many bytes as the
    // buffer passed in.
    while 1 != read(stdin, &mut buffer)? {}

    let c = buffer[0];
    let key = if b'\x1b' == c {
        // Some escape sequences are 2 bytes long (like arrow keys), others are
        // 3 (like page up/down).
        let mut seq = [0u8; 3];
        let bytes_read = read(stdin, &mut seq)?;
        if !(bytes_read == 2 || bytes_read == 3) {
            return Result::Ok(Key::Escape);
        }

        #[allow(clippy::match_same_arms)]
        match seq {
            [b'[', b'A', _] => Key::Arrow(Direction::Up),
            [b'[', b'B', _] => Key::Arrow(Direction::Down),
            [b'[', b'C', _] => Key::Arrow(Direction::Right),
            [b'[', b'D', _] => Key::Arrow(Direction::Left),
            [b'[', b'3', b'~'] => Key::Delete,
            // End: `<esc>[F`, `<esc>OF`, `<esc>[4~`, `<esc>[8~`
            [b'[', b'F', _] => Key::End,
            [b'[', b'O', b'F'] => Key::End,
            [b'[', b'4', b'~'] => Key::End,
            [b'[', b'8', b'~'] => Key::End,
            // Home: `<esc>[H`, `<esc>OH`, `<esc>[1~` or `<esc>[7~`
            [b'[', b'H', _] => Key::Home,
            [b'[', b'O', b'H'] => Key::Home,
            [b'[', b'1', b'~'] => Key::Home,
            [b'[', b'7', b'~'] => Key::Home,
            // Page Up/Down
            [b'[', b'5', b'~'] => Key::Page(Direction::Up),
            [b'[', b'6', b'~'] => Key::Page(Direction::Down),
            _ => Key::Escape,
        }
    } else if is_ctrl(c) {
        // Get the char out of the value to make it easier to match
        Key::Ctrl(char::from(c | CTRL_MASK))
    } else if b'\r' == c {
        Key::Enter
    } else if BACKSPACE == c {
        Key::Backspace
    } else {
        Key::Char(char::from(c))
    };

    Result::Ok(key)
}

/// Uses DSR (Device Status Report) to get the cursor position. The sequence
/// printed will be something like `^[[71;140R` which will be represented as
/// `[27, 91, 55, 49, 59, 49, 52, 48, 82]` in a u8 array.
fn get_cursor_position(stdin: RawFd, stdout: RawFd) -> Result<(u16, u16), Error> {
    let dsr_active_position = b"\x1b[6n";
    if 4 != write(stdout, dsr_active_position)? {
        return simple_err!();
    }

    let mut buffer = [0u8; 32];
    let mut i = 0;
    loop {
        // `read()` reads as much as the buffer so we need to provide a
        // buffer of size 1
        let mut buf = [0u8; 1];
        let bytes_read = read(stdin, &mut buf)?;
        // if we couldn't read enough bytes
        if 1 != bytes_read {
            return simple_err!();
        }
        // save char
        buffer[i] = buf[0];
        // keep reading until we get to the 'R'
        if b'R' == buf[0] {
            break;
        }
        // or error out if the buffer was too small
        if buffer.len() <= i {
            return simple_err!();
        }
        i += 1;
    }
    // Check the content of the buffer for the DSR result
    if b'\x1b' != buffer[0] {
        return simple_err!();
    }
    if b'[' != buffer[1] {
        return simple_err!();
    }
    if b'R' != buffer[i] {
        return simple_err!();
    }

    let mut iter = std::str::from_utf8(&buffer[2..i]).unwrap().split(';');
    // `next()` returns an Option, `parse()` returns a `Result`. The
    // iterator is expected to return 2 elements. `ok_or()` will convert the
    // iterator's option to a `Result` with a `SimplerError` if the
    // iteration failed.
    let rows = iter.next().ok_or(simple_error!())?.parse::<u16>()?;
    let cols = iter.next().ok_or(simple_error!())?.parse::<u16>()?;
    Result::Ok((rows, cols))
}

/// Tries to get the Terminal size through the `ioctl` TIOCGWINSZ. If this
/// fails, tries to fetch the size by moving the cursor to arbitrary big values
/// and reading the cursor position.
fn get_window_size(stdin: RawFd, stdout: RawFd) -> Result<(u16, u16), Error> {
    use nix::libc::{ioctl, winsize, TIOCGWINSZ};

    let res;
    let mut ws = winsize {
        ws_col: 0,
        ws_row: 0,
        ws_xpixel: 0,
        ws_ypixel: 0,
    };

    unsafe {
        res = ioctl(stdout, TIOCGWINSZ, &mut ws);
    }

    if res == -1 || ws.ws_col == 0 {
        // Cursor Forward 999, Cursor Down 999
        let bytes_written = write(stdout, b"\x1b[999C\x1b[999B")?;
        if 12 != bytes_written {
            return simple_err!();
        }

        get_cursor_position(stdin, stdout)
    } else {
        Result::Ok((ws.ws_row, ws.ws_col))
    }
}

/*** syntax highlighting ***/

trait IsSeparator {
    fn is_separator(&self) -> bool;
}

impl IsSeparator for char {
    fn is_separator(&self) -> bool {
        " 	,.()+-/*=~%<>[];".find(*self).is_some()
    }
}

fn highlight_digits(
    hl: &mut [Highlight],
    chars: &[char],
    mut i: usize,
    syntax_flags: u8,
) -> Option<usize> {
    let should_highlight =
        (HL_HIGHLIGHT_NUMBERS == (syntax_flags & HL_HIGHLIGHT_NUMBERS)) && chars[i].is_digit(10);
    if !should_highlight {
        return None;
    }

    hl[i] = Highlight::Number;
    i += 1;
    while i < chars.len() {
        let c = chars[i];
        if c.is_digit(10) || c == '.' {
            hl[i] = Highlight::Number;
            i += 1;
        } else {
            break;
        }
    }
    Some(i)
}

fn highlight_string(
    hl: &mut [Highlight],
    chars: &[char],
    mut i: usize,
    syntax_flags: u8,
) -> Option<usize> {
    let open_char = chars[i];
    let should_highlight = (HL_HIGHLIGHT_STRINGS == (syntax_flags & HL_HIGHLIGHT_STRINGS))
        && ('"' == open_char || '\'' == open_char);

    if !should_highlight {
        return None;
    }

    hl[i] = Highlight::String;
    i += 1;
    // Process following chars until the end of the string
    while i < chars.len() {
        let c = chars[i];
        hl[i] = Highlight::String;

        if c == open_char {
            // End of string
            return Some(i + 1);
        } else if '\\' == c {
            // Whatever it is, next char is escaped and part of the string
            if let Some(h) = hl.get_mut(i + 1) {
                i += 1;
                *h = Highlight::String;
            }
        }
        i += 1;
    }
    // Reaching this statement means that the string doesn't have a closing char
    // on the same line. In this case, `i` is returned so that
    // `editor_update_syntax` knows that the whole line has been highlighted.
    Some(i)
}

fn highlight_single_line_comment(
    hl: &mut [Highlight],
    chars: &[char],
    mut i: usize,
    single_line_comment_start: Option<&str>,
) -> Option<usize> {
    let should_highlight = if let Some(comment_start) = single_line_comment_start {
        let slice_start = i;
        let slice_end = i + comment_start.len();
        if chars.len() < slice_end {
            // No space left to have the comments
            false
        } else {
            let slice_range = slice_start..slice_end;
            let cs: String = chars[slice_range].iter().collect();
            comment_start.len() == cs.len() && comment_start.starts_with(&cs)
        }
    } else {
        false
    };

    if !should_highlight {
        return None;
    }

    // Set the rest of the line as comment
    while i < hl.len() {
        hl[i] = Highlight::Comment;
        i += 1;
    }
    Some(i)
}

fn highlight_multi_line_comment(
    row: &mut ERow,
    chars: &[char],
    mut i: usize,
    multi_line_comment_markers: Option<(&str, &str)>,
) -> Option<usize> {
    let (comment_start, comment_end) = multi_line_comment_markers?;
    let cs_len = comment_start.len();

    // Only look for the start of a comment if it is not already open
    if !row.is_comment_open() && i + cs_len <= chars.len() {
        let hl = &mut row.hl;
        let cs: String = chars.iter().skip(i).take(cs_len).collect();
        if comment_start == cs {
            row.hl_open_comment = true;
            row.hl_close_comment = false;
            // Mark `comment_start` as comment
            for h in hl.iter_mut().skip(i).take(cs_len) {
                *h = Highlight::CommentMultiline;
            }
            i += cs_len;
        }
    }

    if row.is_comment_open() {
        // Look for `comment_end` in the rest of the line
        let hl = &mut row.hl;
        let ce_len = comment_end.len();
        while i + ce_len <= hl.len() {
            let part: String = chars[i..i + ce_len].iter().collect();
            if part == comment_end {
                row.hl_close_comment = true;
                for h in hl.iter_mut().skip(i).take(ce_len) {
                    *h = Highlight::CommentMultiline;
                }
                return Some(i + ce_len);
            }
            hl[i] = Highlight::CommentMultiline;
            i += 1;
        }
        // No marker for EOC and no space left to have one. Comment the
        // rest of the line.
        while i < hl.len() {
            hl[i] = Highlight::CommentMultiline;
            i += 1;
        }
        Some(i)
    } else {
        // Neither the start of a comment was found nor a comment was opened on
        // a previous line
        None
    }
}

fn highlight_keyword(
    hl: &mut [Highlight],
    chars: &[char],
    i: usize,
    syntax_keyword_type: &[&str],
    syntax_keyword_reserved: &[&str],
) -> Option<usize> {
    let mut j: usize = i + 1;

    while j < chars.len() {
        if chars[j].is_separator() {
            break;
        }
        j += 1;
    }

    let word: String = chars[i..j].iter().collect();

    for kw in syntax_keyword_reserved {
        if *kw == word {
            for h in hl.iter_mut().take(j).skip(i) {
                *h = Highlight::KeywordReserved;
            }
            return Some(j);
        }
    }

    for kw in syntax_keyword_type {
        if *kw == word {
            for h in hl.iter_mut().take(j).skip(i) {
                *h = Highlight::KeywordType;
            }
            return Some(j);
        }
    }

    None
}

fn highlight_separator(chars: &[char], i: usize) -> Option<usize> {
    if chars[i].is_separator() {
        Some(i + 1)
    } else {
        None
    }
}

fn highlight_default(chars: &[char], mut i: usize) -> usize {
    while i < chars.len() && !(chars[i].is_separator() || chars[i] == '"' || chars[i] == '\'') {
        i += 1;
    }
    i
}

fn editor_update_syntax(syntax_opt: Option<&EditorSyntax>, row: &mut ERow, is_comment_open: bool) {
    let render = &row.render;
    row.hl.clear();
    row.hl.resize(render.len(), Highlight::Normal);

    if syntax_opt.is_none() {
        return;
    }
    let syntax = syntax_opt.unwrap();

    row.hl_open_comment = is_comment_open;
    row.hl_close_comment = false;
    let chars: Vec<char> = render.chars().collect();
    let mut i: usize = 0;

    while i < chars.len() {
        i = highlight_string(&mut row.hl, &chars, i, syntax.flags)
            .or_else(|| highlight_digits(&mut row.hl, &chars, i, syntax.flags))
            .or_else(|| {
                highlight_single_line_comment(
                    &mut row.hl,
                    &chars,
                    i,
                    syntax.single_line_comment_start,
                )
            })
            .or_else(|| {
                highlight_multi_line_comment(row, &chars, i, syntax.multi_line_comment_markers)
            })
            .or_else(|| {
                highlight_keyword(
                    &mut row.hl,
                    &chars,
                    i,
                    syntax.keyword_type,
                    syntax.keyword_reserved,
                )
            })
            .or_else(|| highlight_separator(&chars, i))
            .unwrap_or_else(|| highlight_default(&chars, i));
    }
}

#[cfg(test)]
mod tests_editor_update_syntax {
    use super::{ERow, Highlight, HLDB};

    macro_rules! assert_hl {
        ($hl_array: expr, $chars: expr) => {
            let syntax = Some(&HLDB[0]);
            let row = ERow::from(syntax, $chars, false);
            assert_eq!($hl_array.to_vec(), row.hl, "pattern=`{}`", $chars);
        };
        ($hl_array: expr, $previous_chars: expr, $chars: expr) => {
            let syntax = Some(&HLDB[0]);
            let previous_row = ERow::from(syntax, $previous_chars, false);
            let row = ERow::from(syntax, $chars, previous_row.is_comment_open());
            assert_eq!($hl_array.to_vec(), row.hl, "pattern=`{}`", $chars);
        };
    }

    const C: Highlight = Highlight::Comment;
    const M: Highlight = Highlight::CommentMultiline;
    const R: Highlight = Highlight::KeywordReserved;
    const T: Highlight = Highlight::KeywordType;
    // Only used during the match process and not the `editor_update_syntax`
    // const M: Highlight = Highlight::Match;
    const H: Highlight = Highlight::Normal;
    const N: Highlight = Highlight::Number;
    const S: Highlight = Highlight::String;

    #[test]
    fn test_text() {
        assert_hl!([H, H, H], "abc");
    }

    #[test]
    fn test_digits() {
        assert_hl!([N, N, N], "123");
        assert_hl!([H, H, H, H, N, N, N], "abc 123");
        // Invalid number but we decided to color it anyway
        assert_hl!([N, N, N, H, H, H], "123abc");
        assert_hl!([H, H, H, H, H, H], "abc123");
        assert_hl!([N, N, N, N], "1.23");
        // `kilo` will highlight this as a single number
        assert_hl!([N, N, N, N, N, N, N], "1.23.45");
    }

    #[test]
    fn test_comments() {
        assert_hl!([H, H, H, H, C, C, C, C, C, C], "abc // abc");
        assert_hl!([C, C, C, C], "//12");
        assert_hl!([N, H, N], "1/2");
        assert_hl!([H, C, C], "a//");
    }

    #[test]
    fn test_multi_lines_comments() {
        assert_hl!([M, M, M, M], "/**/");
        assert_hl!([M, M, M, M, M], "/* */");
        assert_hl!([H, H, M, M, M, M, M], "a /*b*/");
        assert_hl!([M, M, M, M, M, M, M, H, H], "/* a */ b");
        assert_hl!([M, M, M, M, M, M, M, M, H, H], "/* // */ b");
        assert_hl!([M, M, M, M, M, M, M, M, H, H], "/* \"\" */ b");
        assert_hl!([H, M, M, M, M], " /* a");
    }

    #[test]
    fn test2_multi_lines_comments() {
        assert_hl!([M, M, M, M, H, H], "a /* b", "c */ d");
        assert_hl!([M, M, M, M, M, M, M, M], "/*", "c *//* d");
        assert_hl!([M, M, M, M, M, M, M, M, M, M, H], "/*", "*//**//**/d");
    }

    #[test]
    fn test_strings() {
        assert_hl!([H, H, S, S, S, H, H], "a \"b\" c");
        assert_hl!([H, H, S, S, S, S, S, H, H], "a \"123\" c");
        assert_hl!([H, H, S, S, S, S], "a \"123");
    }

    #[test]
    fn test_keywords() {
        assert_hl!([R, R], "if");

        assert_hl!([T, T, T], "int");
        assert_hl!([R, R, H, H, H, H, H, H, N, H], "if (a < 3)");
        assert_hl!([T, T, T, H, H, H, H, H, N, N, H], "int n = 20;");
        assert_hl!(
            [T, T, T, H, H, H, H, H, N, N, H, H, C, C, C, C],
            "int n = 20; // a"
        );

        assert_hl!(
            [H, H, H, H, R, R, H, H, H, H, C, C, C, C, C],
            "abc if ee // ad"
        );

        assert_hl!(
            [H, H, H, H, H, R, R, H, H, H, H, C, C, C, C, C],
            "abcd if ee // ad"
        );
    }
}

/// Used to go through subsequent rows and re-apply syntax if a previous row has
/// opened or closed a comment
fn editor_propagate_update_syntax(syntax_opt: Option<&EditorSyntax>, rows: &mut [ERow], cy: usize) {
    if syntax_opt.is_none() {
        // No need to check for comments if no syntax is activated
        return;
    }

    if rows[cy].is_comment_open() {
        // Comment was opened on the previous line
        for row in rows.iter_mut().skip(cy + 1) {
            editor_update_syntax(syntax_opt, row, true);
            if !row.is_comment_open() {
                break;
            }
        }
    } else {
        // Comment was closed on the previous line
        for row in rows.iter_mut().skip(cy + 1) {
            if row.is_comment_open() {
                editor_update_syntax(syntax_opt, row, false);
            } else {
                break;
            }
        }
    }
}

fn editor_syntax_to_color(h: Highlight) -> &'static [u8] {
    use colors::{
        BLUE_FOREGROUND, CYAN_FOREGROUND, DEFAULT_FOREGROUND, GREEN_FOREGROUND, MAGENTA_FOREGROUND,
        RED_FOREGROUND, YELLOW_FOREGROUND,
    };
    use Highlight::{
        Comment, CommentMultiline, KeywordReserved, KeywordType, Match, Normal, Number, String,
    };

    match h {
        Comment | CommentMultiline => CYAN_FOREGROUND,
        KeywordReserved => YELLOW_FOREGROUND,
        KeywordType => GREEN_FOREGROUND,
        Match => BLUE_FOREGROUND,
        Normal => DEFAULT_FOREGROUND,
        Number => RED_FOREGROUND,
        String => MAGENTA_FOREGROUND,
    }
}

fn editor_select_syntax_highlight(extension: &str) -> Option<&'static EditorSyntax> {
    for syntax in HLDB {
        for file_ext in syntax.file_match {
            if *file_ext == extension {
                return Some(syntax);
            }
        }
    }
    Option::None
}

/*** row operation ***/

/// Converts a `chars` index into a `render` index. For each tab characters
/// located left of the cursor, we add to `cx` the number of columns needed to
/// reach the next tab stop.
fn editor_row_cx_to_rx(chars: &str, cx: u16) -> u16 {
    let mut rx: u16 = 0;
    for (i, c) in chars.char_indices() {
        if cx as usize <= i {
            break;
        }
        if '\t' == c {
            rx += TAB_STOP - 1 - (rx % TAB_STOP);
        }
        rx += 1;
    }
    rx
}

fn editor_row_rx_to_cx(chars: &str, rx: u16) -> u16 {
    use std::convert::TryFrom;

    let mut r = 0;
    for (cx, c) in chars.char_indices() {
        if '\t' == c {
            r += TAB_STOP - (r % TAB_STOP);
        } else {
            r += 1;
        }

        if rx < r {
            return u16::try_from(cx).unwrap_or(u16::MAX);
        }
    }
    u16::try_from(chars.len()).unwrap_or(u16::MAX)
}

/// Test cx <-> rx conversions for possible positions while moving (left or
/// right) on a line. The conversion does not take into account "in between"
/// positions that could result from moving up/down.
#[cfg(test)]
mod tests_cx_rx_conversions {
    use super::*;
    #[test]
    fn test1_editor_cx_to_rx() {
        let chars = "	1";
        assert_eq!(0, editor_row_cx_to_rx(chars, 0));
        assert_eq!(TAB_STOP, editor_row_cx_to_rx(chars, 1));
        assert_eq!(TAB_STOP + 1, editor_row_cx_to_rx(chars, 2));
        assert_eq!(TAB_STOP + 1, editor_row_cx_to_rx(chars, 3));

        let chars = "	1	";
        assert_eq!(0, editor_row_cx_to_rx(chars, 0));
        assert_eq!(TAB_STOP, editor_row_cx_to_rx(chars, 1));
        assert_eq!(TAB_STOP + 1, editor_row_cx_to_rx(chars, 2));
        assert_eq!(2 * TAB_STOP, editor_row_cx_to_rx(chars, 3));

        let chars = "12	456	8";
        assert_eq!(0, editor_row_cx_to_rx(chars, 0));
        assert_eq!(1, editor_row_cx_to_rx(chars, 1));
        assert_eq!(2, editor_row_cx_to_rx(chars, 2));
        assert_eq!(TAB_STOP, editor_row_cx_to_rx(chars, 3));
        assert_eq!(1 + TAB_STOP, editor_row_cx_to_rx(chars, 4));
        assert_eq!(2 + TAB_STOP, editor_row_cx_to_rx(chars, 5));
        assert_eq!(3 + TAB_STOP, editor_row_cx_to_rx(chars, 6));
        assert_eq!(2 * TAB_STOP, editor_row_cx_to_rx(chars, 7));
        assert_eq!(1 + 2 * TAB_STOP, editor_row_cx_to_rx(chars, 8));
    }

    #[test]
    fn test2_editor_rx_to_cx() {
        let chars = "	1";
        assert_eq!(0, editor_row_rx_to_cx(chars, 0));
        assert_eq!(1, editor_row_rx_to_cx(chars, TAB_STOP));
        assert_eq!(2, editor_row_rx_to_cx(chars, 1 + TAB_STOP));

        let chars = "12	";
        assert_eq!(0, editor_row_rx_to_cx(chars, 0));
        assert_eq!(1, editor_row_rx_to_cx(chars, 1));
        assert_eq!(2, editor_row_rx_to_cx(chars, 2));
        assert_eq!(3, editor_row_rx_to_cx(chars, 2 + TAB_STOP));

        let chars = "	1	";
        assert_eq!(0, editor_row_rx_to_cx(chars, 0));
        assert_eq!(1, editor_row_rx_to_cx(chars, TAB_STOP));
        assert_eq!(2, editor_row_rx_to_cx(chars, 1 + TAB_STOP));
        assert_eq!(3, editor_row_rx_to_cx(chars, 2 * TAB_STOP));
    }

    #[test]
    fn test3_editor_cx_to_rx_to_cx() {
        let chars = "	1";
        assert_eq!(0, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 0)));
        assert_eq!(1, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 1)));
        assert_eq!(2, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 2)));

        let chars = "	1	";
        assert_eq!(0, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 0)));
        assert_eq!(1, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 1)));
        assert_eq!(2, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 2)));
        assert_eq!(3, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 3)));

        let chars = "12	456	8";
        assert_eq!(0, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 0)));
        assert_eq!(1, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 1)));
        assert_eq!(2, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 2)));
        assert_eq!(3, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 3)));
        assert_eq!(4, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 4)));
        assert_eq!(5, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 5)));
        assert_eq!(6, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 6)));
        assert_eq!(7, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 7)));
        assert_eq!(8, editor_row_rx_to_cx(chars, editor_row_cx_to_rx(chars, 8)));
    }
}

/// Replaces tabs with enough spaces to reach the next tab stop. This way the
/// tab's display size can be controlled by the editor instead of using on the
/// size set by the terminal.
fn editor_update_row(tab_stop: u16, line: &str) -> String {
    let tab_stop = tab_stop as usize;
    let mut s = String::new();
    for c in line.chars() {
        if '\t' == c {
            s.push(' ');
            let end = (tab_stop - s.len() % tab_stop) % tab_stop;
            for _ in 0..end {
                s.push(' ');
            }
        } else {
            s.push_str(&c.to_string());
        }
    }
    s
}

/// Test the conversion of '\t' characters to spaces depending on the tabstop
/// parameter.
#[cfg(test)]
mod tests_update_row {
    use super::*;
    #[test]
    fn test40_editor_update_row() {
        assert_eq!("    |", editor_update_row(4, "	|"));
    }
    #[test]
    fn test41_editor_update_row() {
        assert_eq!("1   |", editor_update_row(4, "1	|"));
    }
    #[test]
    fn test42_editor_update_row() {
        assert_eq!("12  |", editor_update_row(4, "12	|"));
    }
    #[test]
    fn test43_editor_update_row() {
        assert_eq!("123 |", editor_update_row(4, "123	|"));
    }
    #[test]
    fn test44_editor_update_row() {
        assert_eq!("1234    |", editor_update_row(4, "1234	|"));
    }

    #[test]
    fn test20_editor_update_row() {
        assert_eq!("  |", editor_update_row(2, "	|"));
    }
    #[test]
    fn test21_editor_update_row() {
        assert_eq!("1 |", editor_update_row(2, "1	|"));
    }
    #[test]
    fn test22_editor_update_row() {
        assert_eq!("12  |", editor_update_row(2, "12	|"));
    }

    #[test]
    fn test80_editor_update_row() {
        assert_eq!("        |", editor_update_row(8, "	|"));
    }
    #[test]
    fn test81_editor_update_row() {
        assert_eq!("1       |", editor_update_row(8, "1	|"));
    }
    #[test]
    fn test82_editor_update_row() {
        assert_eq!("12      |", editor_update_row(8, "12	|"));
    }
    #[test]
    fn test83_editor_update_row() {
        assert_eq!("1234567 |", editor_update_row(8, "1234567	|"));
    }
    #[test]
    fn test84_editor_update_row() {
        assert_eq!("12345678        |", editor_update_row(8, "12345678	|"));
    }
}

fn editor_delete_row(editor_config: &mut EditorConfig, at: usize) -> Option<ERow> {
    if editor_config.rows.len() < at {
        None
    } else {
        editor_config.dirty = true;
        Some(editor_config.rows.remove(at))
    }
}

fn editor_row_insert_char(
    syntax: Option<&EditorSyntax>,
    is_comment_open: bool,
    row: &mut ERow,
    at: usize,
    c: char,
) {
    if row.chars.is_char_boundary(at) {
        row.chars.insert(at, c);
    } else {
        row.chars.push(c);
    }
    row.render = editor_update_row(TAB_STOP, &row.chars);
    editor_update_syntax(syntax, row, is_comment_open);
}

fn editor_row_append_string(syntax: Option<&EditorSyntax>, row: &mut ERow, s: &str) {
    row.chars.push_str(s);
    row.render = editor_update_row(TAB_STOP, &row.chars);
    // Adding to the end of a known row, we don't need to look at the previous
    // to know whether we are in a comment or not
    let is_comment_open = row.is_comment_open();
    editor_update_syntax(syntax, row, is_comment_open);
}

/// Removes the character on the left of the cursor.
fn editor_row_delete_char(
    syntax: Option<&EditorSyntax>,
    is_comment_open: bool,
    row: &mut ERow,
    at: usize,
) {
    if at == 0 || row.chars.len() < at {
        return;
    }
    row.chars.remove(at - 1);
    row.render = editor_update_row(TAB_STOP, &row.chars);
    editor_update_syntax(syntax, row, is_comment_open);
}

/*** editor operations ***/

/// Inserts a character at current cursor position. Contrary to the original
/// implementation, does not update the position of the cursor. The original
/// version expects `editor_refresh_screen` to update the rendered position of
/// the cursor. In this version `editor_refresh_screen` does not mutate any
/// state so the cursor needs to be moved with `editor_move_cursor` explicitly
/// instead. This is done by the InsertChar event instead.
fn editor_insert_char(editor_config: &mut EditorConfig, c: char) {
    let (cursor_x, cursor_y) = editor_config.cursor;
    let cx = cursor_x as usize;
    let cy = cursor_y as usize;
    let is_comment_open = is_row_comment_open(&editor_config.rows, cy);
    if editor_config.rows.len() == cy {
        // Add a new line at the end of the file
        let row = ERow::from(editor_config.syntax, &c.to_string(), is_comment_open);
        editor_config.rows.push(row);
    } else if let Some(row) = editor_config.rows.get_mut(cy) {
        // Insert char in existing line
        editor_row_insert_char(editor_config.syntax, is_comment_open, row, cx, c);
    }
    editor_config.dirty = true;
    editor_propagate_update_syntax(editor_config.syntax, &mut editor_config.rows, cy);
}

fn editor_insert_newline(editor_config: &mut EditorConfig) {
    let syntax = editor_config.syntax;
    let (cursor_x, cursor_y) = editor_config.cursor;
    let mut cy = cursor_y as usize;
    let is_comment_open = is_row_comment_open(&editor_config.rows, cy);
    if (0, 0) == (cursor_x, cursor_y) || editor_config.rows.len() == cy {
        // Add an empty line at the beginning or the end of the file
        let new_row = ERow::from(syntax, "", is_comment_open);
        editor_config.rows.insert(cy, new_row);
    } else if let Some(mut row) = editor_config.rows.get_mut(cy) {
        let cx = cursor_x as usize;
        let chars = row.chars.split_off(cx);
        row.render = editor_update_row(TAB_STOP, &row.chars);
        editor_update_syntax(syntax, row, is_comment_open);
        // New line
        cy += 1;
        let new_row = ERow::from(syntax, &chars, row.is_comment_open());
        editor_config.rows.insert(cy, new_row);
    }
    editor_propagate_update_syntax(editor_config.syntax, &mut editor_config.rows, cy);
}

fn editor_delete_char(editor_config: &mut EditorConfig) {
    use std::convert::TryFrom;

    let (cursor_x, cursor_y) = editor_config.cursor;
    let cx = cursor_x as usize;
    let mut cy = cursor_y as usize;
    let is_comment_open = is_row_comment_open(&editor_config.rows, cy);
    if editor_config.rows.len() == cy || (0, 0) == editor_config.cursor {
        return;
    } else if 0 == cursor_x {
        if let Some(current_row) = editor_delete_row(editor_config, cy) {
            // Get previous line length so we can set the cursor at the junction
            // of the two lines
            let line_length = editor_config.rows[cy - 1].chars.len();
            let cursor_x = u16::try_from(line_length).unwrap_or(u16::MAX);
            // `cursor_x` needs to be offset by one to compensate for the
            // default "move one position right"
            editor_config.cursor = (cursor_x + 1, cursor_y - 1);
            // Merge previous and current lines
            cy -= 1;
            editor_row_append_string(
                editor_config.syntax,
                &mut editor_config.rows[cy],
                &current_row.chars,
            );
        }
    } else if let Some(row) = editor_config.rows.get_mut(cy) {
        editor_row_delete_char(editor_config.syntax, is_comment_open, row, cx);
    }
    editor_config.dirty = true;
    editor_propagate_update_syntax(editor_config.syntax, &mut editor_config.rows, cy);
}

/*** file i/o ***/

fn editor_rows_to_string(editor_config: &EditorConfig) -> String {
    let mut text = editor_config
        .rows
        .iter()
        .map(|erow| erow.chars.as_ref())
        .collect::<Vec<&str>>()
        .join("\n");
    text.push('\n');
    text
}

/// All the size and allocation are handled by the Vect type in Rust. The
/// `BufRead::lines()` splits the lines on carriage returns and removes them
/// from the result. We don't need to read the input one char at the time and
/// can just save the String in the rows.
fn editor_open(editor_config: &mut EditorConfig, filename: &str) -> Result<(), Error> {
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    use std::path::Path;

    let file_path = Path::new(filename);
    editor_config.filename = file_path
        .file_name()
        .and_then(std::ffi::OsStr::to_str)
        .map(std::string::ToString::to_string);
    let ext_opt = file_path.extension().and_then(std::ffi::OsStr::to_str);

    if let Some(extension) = ext_opt {
        editor_config.syntax = editor_select_syntax_highlight(extension);
    }

    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();
    let mut is_comment_open = false;

    while let Some(Ok(chars)) = lines.next() {
        let row = ERow::from(editor_config.syntax, &chars, is_comment_open);
        is_comment_open = row.is_comment_open();
        editor_config.rows.push(row);
    }

    editor_config.dirty = false;
    Result::Ok(())
}

/// Returns the number of bytes written or an Error
fn editor_save(editor_config: &mut EditorConfig) -> Result<(), Error> {
    use std::fs::File;
    use std::io::Write;

    let filename = if let Some(filename) = &editor_config.filename {
        filename
    } else if let Some(buffer) =
        editor_prompt(editor_config, "Save as (C-g to cancel): ", |_, _, _| {})?
    {
        editor_config.filename = Some(buffer);
        editor_config.filename.as_ref().unwrap()
    } else {
        editor_set_status_message(editor_config, "Save aborted");
        return Result::Ok(());
    };

    // `File::create` creates or truncats the file if it already exists.
    let mut file = File::create(filename)?;
    let buffer = editor_rows_to_string(editor_config).into_bytes();
    file.write_all(&buffer)?;
    editor_config.dirty = false;
    editor_set_status_message(
        editor_config,
        format!("{} bytes written to disk", buffer.len()).as_ref(),
    );
    Result::Ok(())
}

/*** find ***/

/// "Dumb" implementation that will iterate over all the lines of the files for
/// each new call. Iterates over all the matches found before selecting the next
/// cursor position to colour all the items matching the query.
fn editor_find_callback(editor_config: &mut EditorConfig, query: &str, key: &Key) {
    use std::convert::TryFrom;

    if Key::Enter == *key || Key::Escape == *key || Key::Ctrl('g') == *key {
        return;
    }

    let matches: Vec<(u16, u16)> = editor_config
        .rows
        .iter_mut()
        .enumerate()
        .filter_map(|(cy, row)| row.render.find(&query).map(|rx| (row, rx, cy)))
        .map(|(row, rx, cy)| {
            // Highlight chars marching the query in the row
            for i in row.hl.iter_mut().skip(rx).take(query.len()) {
                *i = Highlight::Match;
            }
            (row, rx, cy)
        })
        .map(|(row, rx, cy)| {
            let cy = u16::try_from(cy).unwrap_or(u16::MAX);
            let rx = u16::try_from(rx).unwrap_or(u16::MAX);
            (row, rx, cy)
        })
        .map(|(row, rx, cy)| {
            let cx = editor_row_rx_to_cx(&row.chars, rx);
            (cx, cy)
        })
        .collect();

    let mut match_locations = matches.iter().copied();
    let cursor = editor_config.cursor;
    let (cx, cy) = cursor;
    editor_config.cursor = match key {
        // Find the last march located before the cursor
        Key::Arrow(Direction::Up) => match_locations
            .filter(|(x, y)| (*y < cy) || (*y == cy && *x < cx))
            .last()
            .unwrap_or(cursor),
        // Find the first match located after the cursor
        Key::Arrow(Direction::Down) => match_locations
            .find(|(x, y)| (cy < *y) || (cy == *y && cx < *x))
            .unwrap_or(cursor),
        // Find a match at the current cursor or after it
        _ => match_locations
            .find(|(x, y)| cx <= *x && cy <= *y)
            .unwrap_or(cursor),
    };
    editor_scroll(editor_config);
}

fn editor_find(editor_config: &mut EditorConfig) -> Result<(), Error> {
    let cursor = editor_config.cursor;
    let hls: Vec<Vec<Highlight>> = editor_config
        .rows
        .iter()
        .map(|row| row.hl.clone())
        .collect();

    let query = editor_prompt(
        editor_config,
        "Search (Use Enter/Arrows/Ctrl-g): ",
        |e, q, k| {
            // Reset highlights to default before searching and applying new
            // Match highlight
            for (row, hl) in e.rows.iter_mut().zip(hls.iter()) {
                row.hl = hl.clone();
            }
            editor_find_callback(e, q, k);
        },
    )?;
    // Query was empty or cancelled, resetting cursor to its original position
    if query.is_none() {
        editor_config.cursor = cursor;
        editor_scroll(editor_config);
        editor_set_status_message(editor_config, "Quit");
    } else {
        editor_set_status_message(editor_config, "");
    }
    Result::Ok(())
}

/*** output ***/

fn editor_scroll(editor_config: &mut EditorConfig) {
    use std::cmp::max;

    let (cx, cy) = editor_config.cursor;
    let col_offset = editor_config.col_offset;
    let row_offset = editor_config.row_offset;
    let screen_cols = editor_config.screen_cols;
    let screen_rows = editor_config.screen_rows;

    let rx: u16 = match editor_config.rows.get(cy as usize) {
        Some(row) => editor_row_cx_to_rx(&row.chars, cx),
        None => 0,
    };

    editor_config.rcursor = (rx, 0);

    if rx < col_offset {
        editor_config.col_offset = rx;
    }

    if rx >= col_offset + screen_cols {
        editor_config.col_offset = max(0, rx + 1 - screen_cols);
    }

    if cy < row_offset {
        editor_config.row_offset = cy;
    }

    if cy >= row_offset + screen_rows {
        editor_config.row_offset = max(0, cy - screen_rows + 1);
    }
}

/// Draws a vertical column of `~` when the screen is empty
fn editor_draw_rows(editor_config: &EditorConfig, ab: &mut Vec<u8>) {
    use std::cmp::Ordering;
    let col_offset = editor_config.col_offset as usize;
    let row_offset = editor_config.row_offset as usize;
    let screen_rows = editor_config.screen_rows as usize;
    let screen_cols = editor_config.screen_cols as usize;
    let rows = &editor_config.rows;

    for y in 0..(screen_rows) {
        let file_row = y + row_offset;
        let file_col = col_offset;
        if let Some(row) = rows.get(file_row) {
            let mut highlight = Highlight::Normal;
            let line = &row.render;
            let hl = &row.hl;
            let line_begin = min(file_col, line.len());
            let line_end = min(line.len(), screen_cols + file_col);
            for (i, b) in line[line_begin..line_end].bytes().enumerate() {
                if is_ctrl(b) {
                    let sym: u8 = if b <= 26 { b'@' + b } else { b'?' };
                    ab.extend(INVERT_COLOUR);
                    ab.push(sym);
                    ab.extend(NORMAL_FORMAT);
                    if Highlight::Normal != highlight {
                        ab.extend(editor_syntax_to_color(highlight));
                    }
                } else if highlight != hl[line_begin + i] {
                    highlight = hl[line_begin + i];
                    ab.extend(editor_syntax_to_color(highlight));
                }
                ab.push(b);
            }
            ab.extend(colors::DEFAULT_FOREGROUND);
        } else if rows.is_empty() && y == screen_rows / 3 {
            let welcome = format!("{} editor -- version {}", PKG_NAME, PKG_VERSION);
            let truncate = min(welcome.len(), screen_cols);
            let padding = (screen_cols - truncate) / 2;
            match padding.cmp(&1) {
                // No space to do padding
                Ordering::Less => (),
                // Only enough space for the initial tilda
                Ordering::Equal => ab.extend(b"~"),
                // Enough space for the tilda and some spaces
                Ordering::Greater => {
                    let mut left_padding = vec![b' '; padding - 1];
                    ab.extend(b"~");
                    ab.append(&mut left_padding);
                }
            }

            ab.extend(&welcome.as_bytes()[..truncate]);
        } else {
            ab.extend(b"~");
        }

        ab.extend(CLEAR_LINE);
        ab.extend(b"\r\n");
    }
}

fn editor_draw_status_bar(editor_config: &EditorConfig, ab: &mut Vec<u8>) {
    let width = editor_config.screen_cols as usize;
    let (_, cy) = editor_config.cursor;
    let no_filename = "[No name]".to_string();
    let filename = editor_config.filename.as_ref().unwrap_or(&no_filename);
    let numrows = editor_config.rows.len();
    let lines_info = if 1 == numrows {
        "1 line".to_string()
    } else {
        format!("{} lines", numrows)
    };
    let dirty = if editor_config.dirty {
        " (modified) "
    } else {
        ""
    };
    let file_info = format!(
        "{filename:.20} - {lines_info}{dirty}",
        filename = filename,
        lines_info = lines_info,
        dirty = dirty,
    );
    let mode = editor_config
        .syntax
        .as_ref()
        .map_or("unknown", |syntax| syntax.file_type);
    let mode_position = format!("{} | {}/{}", mode, cy + 1, numrows);
    let padding = width.saturating_sub(file_info.len());
    let status = format!(
        "{file_info}{mode_position:>padding$}",
        file_info = file_info,
        mode_position = mode_position,
        padding = padding,
    );
    let padded_status = format!("{status:.width$}", status = status, width = width);
    ab.extend(INVERT_COLOUR);
    ab.extend(padded_status.as_bytes());
    ab.extend(NORMAL_FORMAT);
    ab.extend(b"\r\n");
}

fn editor_draw_message_bar(editor_config: &EditorConfig, ab: &mut Vec<u8>) {
    ab.extend(CLEAR_LINE);
    if let Some(StatusMessage { message, .. }) = &editor_config.status_message {
        ab.extend(message.as_bytes());
    }
}

/// Writes the "ED" escape sequence (clear screen [1]) to the terminal. `\x1b`
/// starts the escape sequence and the sequence `[2J` clears the whole screen.
///
/// [1] https://vt100.net/docs/vt100-ug/chapter3.html#ED
fn editor_refresh_screen(editor_config: &EditorConfig) -> Result<(), Error> {
    let col_offset = editor_config.col_offset;
    let row_offset = editor_config.row_offset;
    let stdout = editor_config.stdout;

    let mut ab = Vec::<u8>::with_capacity(22);
    ab.extend(HIDE_CURSOR);
    ab.extend(CURSOR_HOME);
    editor_draw_rows(editor_config, &mut ab);
    editor_draw_status_bar(editor_config, &mut ab);
    editor_draw_message_bar(editor_config, &mut ab);

    let (_, cy) = editor_config.cursor;
    let (rx, _) = editor_config.rcursor;
    // The cursor escape sequence is 1-indexed. The code sequence parameters are
    // (rows, cols) which means (cy, cx). `cy` refers to the position in the the
    // file, so the `row_offset` is used to translate that to a position on the
    // screen.
    let move_cursor = format!("\x1b[{};{}H", cy + 1 - row_offset, rx + 1 - col_offset);
    ab.extend(move_cursor.as_bytes());

    ab.extend(SHOW_CURSOR);

    write(stdout, &ab)?;
    Result::Ok(())
}

/// The original version uses a variadic function. This usecase is left out
/// for now.
fn editor_set_status_message(editor_config: &mut EditorConfig, fmt: &str) {
    editor_config.status_message = Some(StatusMessage {
        message: fmt.to_string(),
        time: Instant::now(),
    });
}

fn editor_clear_status_message_after_timeout(editor_config: &mut EditorConfig) {
    if let Some(StatusMessage { time, .. }) = editor_config.status_message {
        if Duration::new(5, 0) < Instant::now() - time {
            editor_config.status_message = Option::None;
        }
    }
}

/*** input ***/

fn editor_prompt<Callback>(
    editor_config: &mut EditorConfig,
    prompt: &str,
    f: Callback,
) -> Result<Option<String>, Error>
where
    Callback: Fn(&mut EditorConfig, &str, &Key),
{
    let mut buffer = String::with_capacity(128);
    loop {
        editor_set_status_message(editor_config, format!("{}{}", prompt, buffer).as_ref());
        editor_refresh_screen(editor_config)?;
        let key = editor_read_key(editor_config)?;
        match key {
            Key::Char(c) => buffer.push(c),
            Key::Backspace | Key::Ctrl('h') => {
                buffer.pop();
            }
            Key::Enter | Key::Ctrl('m') => {
                editor_set_status_message(editor_config, "");
                f(editor_config, &buffer, &Key::Enter);
                return Result::Ok(Some(buffer));
            }
            Key::Escape | Key::Ctrl('g') => {
                f(editor_config, &buffer, &Key::Escape);
                return Result::Ok(None);
            }
            _ => (),
        }
        f(editor_config, &buffer, &key);
    }
}

/// Moves the cursor within the bounds of the file. Contrary to the original
/// implementation also triggers a refresh of the scrolling status after the
/// update. This limits the mutation of the cursor and keeps the screen refresh
/// free of mutation. Using "saturating" addition and substraction to have
/// controlled overflow. Thanks to that the min check can be skipped for
/// substractions.
fn editor_move_cursor(editor_config: &mut EditorConfig, direction: &Direction, times: u16) {
    use std::convert::TryFrom;
    use Direction::{Down, Left, Right, Up};

    // Make sure not to move the cursor if it needs to be moved 0 times.
    if 0 == times {
        return;
    }

    let capped_line_length = |y| {
        let line_length = editor_config
            .rows
            .get(y as usize)
            .map_or(0, |row| row.chars.len());
        u16::try_from(line_length).unwrap_or(u16::MAX)
    };
    let (current_cx, current_cy) = editor_config.cursor;
    let expected_cx = current_cx.saturating_add(times);
    let expected_cy = current_cy.saturating_add(times);
    let max_x = capped_line_length(current_cy);
    let max_y = u16::try_from(editor_config.rows.len()).unwrap_or(u16::MAX);

    let cursor = match direction {
        Down => {
            let cy = min(max_y, expected_cy);
            let cx = min(current_cx, capped_line_length(cy));
            (cx, cy)
        }
        Up => {
            let cy = current_cy.saturating_sub(times);
            let cx = min(current_cx, capped_line_length(cy));
            (cx, cy)
        }
        Right => {
            if current_cx == max_x && current_cy < max_y {
                // Moving right at the end of the line wraps to the beginning of the
                // next line.
                (0, current_cy + 1)
            } else {
                (min(max_x, expected_cx), current_cy)
            }
        }
        Left => {
            if 0 == current_cx && 0 < current_cy {
                // Moving left at the beginning of the line wraps to the end of the
                // previous line.
                let cy = current_cy - 1;
                (capped_line_length(cy), cy)
            } else {
                (current_cx.saturating_sub(times), current_cy)
            }
        }
    };

    editor_config.cursor = cursor;

    // After a cursor move, might need to scroll
    editor_scroll(editor_config);
}

fn editor_process_keypress(editor_config: &EditorConfig) -> Result<Event, Error> {
    use std::convert::TryFrom;

    let screen_rows = editor_config.screen_rows;
    let row_offset = editor_config.row_offset as u16;
    let (_, cy) = editor_config.cursor;
    let line_length = editor_config
        .rows
        .get(cy as usize)
        .map(|l| l.chars.len())
        .map_or(0, |len| u16::try_from(len).unwrap_or(u16::MAX));

    #[allow(clippy::match_same_arms)]
    let result = match editor_read_key(editor_config)? {
        Key::Arrow(direction) => Event::CursorMove(direction, 1),
        Key::Ctrl('f') => Event::Find,
        Key::Ctrl('q') => Event::Quit,
        Key::Ctrl('s') => Event::Save,
        Key::Backspace | Key::Ctrl('h') => Event::DeleteBackwardChar,
        Key::Delete => Event::DeleteForwardChar,
        Key::End => Event::CursorMove(Direction::Right, line_length),
        Key::Enter | Key::Ctrl('m') => Event::InsertNewline,
        Key::Home => Event::CursorMove(Direction::Left, line_length),
        Key::Page(Direction::Up) => Event::CursorMove(
            // Moving up by one screen plus cursor_y's offset from the top of
            // the screen
            Direction::Up,
            screen_rows.saturating_add(cy).saturating_sub(row_offset),
        ),
        Key::Page(Direction::Down) => Event::CursorMove(
            // Moving down by one screen plus enough to put the cursor to the
            // last line of the screen
            Direction::Down,
            screen_rows
                .saturating_add(screen_rows)
                .saturating_add(cy.saturating_sub(row_offset))
                .saturating_sub(1),
        ),
        // No need to refresh on `C-l` as the screen is already updated on every
        // key press
        Key::Ctrl('l') => Event::None,
        // <escape> does nothing
        Key::Char('\x1b') => Event::None,
        Key::Char(c) => Event::InsertChar(c),
        _ => Event::None,
    };
    Result::Ok(result)
}

/*** init ***/

fn init_editor(stdin: RawFd, stdout: RawFd) -> Result<EditorConfig, Error> {
    let original_termios = Termios::from_fd(stdin)?;
    enable_raw_mode(stdin, original_termios)?;
    let (screen_rows, screen_cols) = get_window_size(stdin, stdout)?;
    let editor_config = EditorConfig {
        cursor: (0, 0),
        rcursor: (0, 0),
        original_termios,
        row_offset: 0,
        col_offset: 0,
        rows: vec![],
        dirty: false,
        stdin,
        stdout,
        screen_rows: screen_rows.saturating_sub(2),
        screen_cols,
        filename: Option::None,
        status_message: Option::None,
        syntax: Option::None,
    };
    Result::Ok(editor_config)
}

fn main() -> Result<(), Error> {
    use std::os::unix::io::AsRawFd;

    let stdin: RawFd = std::io::stdin().as_raw_fd();
    let stdout: RawFd = std::io::stdout().as_raw_fd();
    let mut editor_config = init_editor(stdin, stdout)?;
    let mut quit_times: u8 = QUIT_TIMES;

    let args: Vec<String> = std::env::args().collect();
    if let Some(filename) = args.get(1) {
        editor_open(&mut editor_config, filename)?;
    }

    editor_set_status_message(&mut editor_config, "HELP: Ctrl-S = save | Ctrl-Q = quit");

    loop {
        editor_clear_status_message_after_timeout(&mut editor_config);
        editor_refresh_screen(&editor_config)?;
        match editor_process_keypress(&editor_config)? {
            Event::Quit => {
                if editor_config.dirty && quit_times > 0 {
                    editor_set_status_message(
                        &mut editor_config,
                        format!(
                            "{warning} {press} {quit_times} {to_quit}",
                            warning = "WARNING!!! File has unsaved changes.",
                            press = "Press Ctrl-Q",
                            quit_times = quit_times,
                            to_quit = "more times to quit.",
                        )
                        .as_ref(),
                    );
                    quit_times -= 1;
                    continue;
                }
                break;
            }
            Event::CursorMove(direction, amount) => {
                editor_move_cursor(&mut editor_config, &direction, amount);
            }
            Event::DeleteBackwardChar => {
                editor_delete_char(&mut editor_config);
                editor_move_cursor(&mut editor_config, &Direction::Left, 1);
            }
            Event::DeleteForwardChar => {
                editor_move_cursor(&mut editor_config, &Direction::Right, 1);
                editor_delete_char(&mut editor_config);
                editor_move_cursor(&mut editor_config, &Direction::Left, 1);
            }
            Event::InsertChar(c) => {
                editor_insert_char(&mut editor_config, c);
                editor_move_cursor(&mut editor_config, &Direction::Right, 1);
            }
            Event::Find => {
                editor_find(&mut editor_config)?;
            }
            Event::InsertNewline => {
                editor_insert_newline(&mut editor_config);
                editor_move_cursor(&mut editor_config, &Direction::Down, 1);
                // Go to the beginning of the line if the cursor is not already
                // there.
                let (cursor_x, _) = editor_config.cursor;
                editor_move_cursor(&mut editor_config, &Direction::Left, cursor_x);
            }
            Event::Save => {
                if let Err(e) = editor_save(&mut editor_config) {
                    editor_set_status_message(
                        &mut editor_config,
                        format!("Can't save! I/O error: {:?}", e).as_ref(),
                    );
                }
            }
            Event::None => continue,
        }
        quit_times = QUIT_TIMES;
    }

    Result::Ok(())
}
