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
    KeywordReserved,
    KeywordType,
    Match,
    Normal,
    Number,
    String,
}

/// struct editorSyntax {
///   char *filetype;
///   char **filematch;
///   char **keywords;
///   char *singleline_comment_start;
///   int flags;
/// };
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
}

/// typedef struct erow {
///   int size;
///   int rsize;
///   char *chars;
///   char *render;
///   unsigned char *hl;
/// } erow;
struct ERow {
    chars: String,
    hl: Vec<Highlight>,
    render: String,
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

/// char *C_HL_extensions[] = { ".c", ".h", ".cpp", NULL };
const C_HL_EXT: [&str; 3] = ["c", "h", "cpp"];
const RS_HL_EXT: [&str; 1] = ["rs"];

/// char *C_HL_keywords[] = {
///   "switch", "if", "else", ...
///   "int|", "long|"
///   NULL
/// };
const C_HL_KW_RESERVED: [&str; 4] = ["switch", "if", "else", "return"];
const C_HL_KW_TYPE: [&str; 5] = ["byte", "char", "double", "float", "int"];
const RS_HL_KW_RESERVED: [&str; 4] = ["fn", "match", "if", "else"];
const RS_HL_KW_TYPE: [&str; 3] = ["bool", "u8", "i8"];

/// struct editorSyntax HLDB[] = {
///   {
///     "c",
///     C_HL_extensions,
///     C_HL_keywords,
///     "//",
///     HL_HIGHLIGHT_NUMBERS
///   },
/// };
const HLDB: &[EditorSyntax] = &[
    EditorSyntax {
        file_type: "C",
        file_match: &C_HL_EXT,
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        keyword_reserved: &C_HL_KW_RESERVED,
        keyword_type: &C_HL_KW_TYPE,
        single_line_comment_start: Some("//"),
    },
    EditorSyntax {
        file_type: "rust",
        file_match: &RS_HL_EXT,
        flags: HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS,
        keyword_reserved: &RS_HL_KW_RESERVED,
        keyword_type: &RS_HL_KW_TYPE,
        single_line_comment_start: Some("//"),
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
/// ---
/// if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1) die("tcgetattr");
/// struct termios raw = E.orig_termios;
/// tcgetattr(STDIN_FILENO, &raw);
/// raw.c_cflag |= (CS8);
/// raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
/// raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
/// raw.c_oflag &= ~(OPOST);
/// raw.c_cc[VMIN] = 0;
/// raw.c_cc[VTIME] = 1;
/// tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
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
/// ---
/// if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
///   die("tcsetattr");
fn disable_raw_mode(fd: RawFd, original: Termios) -> Result<(), std::io::Error> {
    tcsetattr(fd, TCSAFLUSH, &original)
}

/// int nread;
/// char c;
/// while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
///   if (nread == -1 && errno != EAGAIN) die("read");
/// }
/// if (c == '\x1b') {
///   char seq[3];
///   if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
///   if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';
///
///   if (seq[0] == '[') {
///     if (seq[1] >= '0' && seq[1] <= '9') {
///       if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
///       if (seq[2] == '~') {
///         switch (seq[1]) {
///           case '1': return HOME_KEY;
///           case '3': return DEL_KEY;
///           case '4': return END_KEY;
///           case '5': return PAGE_UP;
///           case '6': return PAGE_DOWN;
///           case '7': return HOME_KEY;
///           case '8': return END_KEY;
///         }
///       }
///     } else {
///       switch (seq[1]) {
///         case 'A': return ARROW_UP;
///         case 'B': return ARROW_DOWN;
///         case 'C': return ARROW_RIGHT;
///         case 'D': return ARROW_LEFT;
///         case 'H': return HOME_KEY;
///         case 'F': return END_KEY;
///       }
///     }
///   } else if (seq[0] == 'O') {
///     switch (seq[1]) {
///       case 'H': return HOME_KEY;
///       case 'F': return END_KEY;
///     }
///   }
///
///   return '\x1b';
/// } else {
///   return c;
/// }
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
///---
/// char buf[32];
/// unsigned int i = 0;
/// if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;
/// while (i < sizeof(buf) - 1) {
///   if (read(STDIN_FILENO, &buf[i], 1) != 1) break;
///   if (buf[i] == 'R') break;
///   i++;
/// }
/// buf[i] = '\0';
/// if (buf[0] != '\x1b' || buf[1] != '[') return -1;
/// if (sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;
/// return 0;
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
///---
/// struct winsize ws;
/// if (1 || ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0) {
///   if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
///   editorReadKey();
/// } else {
///   *cols = ws.ws_col;
///   *rows = ws.ws_row;
///   return 0;
/// }
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

/// return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
impl IsSeparator for char {
    fn is_separator(&self) -> bool {
        " 	,.()+-/*=~%<>[];".find(*self).is_some()
    }
}

fn should_highlight_as_digits(
    syntax_flags: u8,
    c: char,
    previous_highlight: Highlight,
    previous_is_separator: bool,
) -> bool {
    (HL_HIGHLIGHT_NUMBERS == (syntax_flags & HL_HIGHLIGHT_NUMBERS))
        && ((c.is_digit(10) && (previous_is_separator || Highlight::Number == previous_highlight))
            || ('.' == c && Highlight::Number == previous_highlight))
}

fn highlight_digits(
    hl: &mut [Highlight],
    i: &mut usize,
    previous_highlight: &mut Highlight,
    previous_is_separator: &mut bool,
) {
    hl[*i] = Highlight::Number;
    *i += 1;
    *previous_highlight = Highlight::Number;
    *previous_is_separator = false;
}

fn should_highlight_as_string(syntax_flags: u8, c: char) -> bool {
    (HL_HIGHLIGHT_STRINGS == (syntax_flags & HL_HIGHLIGHT_STRINGS)) && ('"' == c || '\'' == c)
}

fn highlight_string(
    hl: &mut [Highlight],
    chars: &[char],
    i: &mut usize,
    previous_highlight: &mut Highlight,
    previous_is_separator: &mut bool,
) {
    let open_char = chars[*i];
    hl[*i] = Highlight::String;
    *i += 1;
    // Process following chars until the end of the string
    while *i < chars.len() {
        let c = chars[*i];
        hl[*i] = Highlight::String;

        if c == open_char {
            // End of string
            *previous_is_separator = c.is_separator();
            *i += 1;
            return;
        } else if '\\' == c {
            // Whatever it is, next char is escaped and part of the string
            if let Some(h) = hl.get_mut(*i + 1) {
                *i += 1;
                *h = Highlight::String;
            }
        }
        *i += 1;
    }
    *previous_highlight = Highlight::String;
    *previous_is_separator = true;
}

fn should_highlight_as_single_line_comment(
    single_line_comment_start: Option<&str>,
    chars: &[char],
    i: usize,
) -> bool {
    if let Some(comment_start) = single_line_comment_start {
        let slice_start = i;
        let slice_end = i + comment_start.len();
        if chars.len() < slice_end {
            // No space left to have the comments
            return false;
        }
        let slice_range = slice_start..slice_end;
        let cs: String = chars[slice_range].iter().collect();
        comment_start.len() == cs.len() && comment_start.starts_with(&cs)
    } else {
        false
    }
}

fn highlight_single_line_comment(
    hl: &mut [Highlight],
    i: &mut usize,
    previous_highlight: &mut Highlight,
    previous_is_separator: &mut bool,
) {
    // Set the rest of the line as comment
    while *i < hl.len() {
        hl[*i] = Highlight::Comment;
        *i += 1;
    }
    // End of line so it doesn't matter much
    *previous_highlight = Highlight::Normal;
    *previous_is_separator = true;
}

fn should_highlight_as_keyword(
    chars: &[char],
    i: usize,
    previous_is_separator: bool,
    syntax_keyword_type: &[&str],
    syntax_keyword_reserved: &[&str],
) -> bool {
    if !previous_is_separator {
        return false;
    }

    let mut j = i + 1;

    while j < chars.len() {
        if chars[j].is_separator() {
            break;
        }
        j += 1;
    }

    let word: String = chars[i..j].iter().collect();

    for kw in syntax_keyword_reserved {
        if *kw == word {
            return true;
        }
    }

    for kw in syntax_keyword_type {
        if *kw == word {
            return true;
        }
    }

    false
}

fn highlight_keyword(
    hl: &mut [Highlight],
    chars: &[char],
    i: &mut usize,
    syntax_keyword_type: &[&str],
    syntax_keyword_reserved: &[&str],
    previous_highlight: &mut Highlight,
    previous_is_separator: &mut bool,
) {
    let mut j: usize = *i + 1;

    while j < chars.len() {
        if chars[j].is_separator() {
            break;
        }
        j += 1;
    }

    let word: String = chars[*i..j].iter().collect();

    for kw in syntax_keyword_reserved {
        if *kw == word {
            for h in hl.iter_mut().take(j).skip(*i) {
                *h = Highlight::KeywordReserved;
            }
            *i = j;
            *previous_highlight = Highlight::KeywordReserved;
            *previous_is_separator = false;
            return;
        }
    }

    for kw in syntax_keyword_type {
        if *kw == word {
            for h in hl.iter_mut().take(j).skip(*i) {
                *h = Highlight::KeywordType;
            }
            *i = j;
            *previous_highlight = Highlight::KeywordReserved;
            *previous_is_separator = false;
            return;
        }
    }
}

/// row->hl = realloc(row->hl, row->rsize);
/// memset(row->hl, HL_NORMAL, row->rsize);
/// if (E.syntax == NULL) return;
/// char **keywords = E.syntax->keywords;
/// char *scs = E.syntax->singleline_comment_start;
/// int scs_len = scs ? strlen(scs) : 0;
/// int prev_sep = 1;
/// int in_string = 0;
/// int i = 0;
/// while (i < row->rsize) {
///   char c = row->render[i];
///   unsigned char prev_hl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;
///   if (scs_len && !in_string) {
///     if (!strncmp(&row->render[i], scs, scs_len)) {
///       memset(&row->hl[i], HL_COMMENT, row->rsize - i);
///       break;
///     }
///   }
///   if (E.syntax->flags & HL_HIGHLIGHT_STRINGS) {
///     if (in_string) {
///       row->hl[i] = HL_STRING;
///       if (c == '\\' && i + 1 < row->rsize) {
///         row->hl[i + 1] = HL_STRING;
///         i += 2;
///         continue;
///       }
///       if (c == in_string) in_string = 0;
///       i++;
///       prev_sep = 1;
///       continue;
///     } else {
///       if (c == '"' || c == '\'') {
///         in_string = c;
///         row->hl[i] = HL_STRING;
///         i++;
///         continue;
///       }
///     }
///   }
///   if (E.syntax->flags & HL_HIGHLIGHT_NUMBERS) {
///     if ((isdigit(c) && (prev_sep || prev_hl == HL_NUMBER)) ||
///         (c == '.' && prev_hl == HL_NUMBER)) {
///       row->hl[i] = HL_NUMBER;
///       i++;
///       prev_sep = 0;
///       continue;
///     }
///   }
///   if (prev_sep) {
///     int j;
///     for (j = 0; keywords[j]; j++) {
///       int klen = strlen(keywords[j]);
///       int kw2 = keywords[j][klen - 1] == '|';
///       if (kw2) klen--;
///       if (!strncmp(&row->render[i], keywords[j], klen) &&
///           is_separator(row->render[i + klen])) {
///         memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
///         i += klen;
///         break;
///       }
///     }
///     if (keywords[j] != NULL) {
///       prev_sep = 0;
///       continue;
///     }
///   }
///   prev_sep = is_separator(c);
///   i++;
/// }
fn editor_update_syntax(syntax_opt: Option<&EditorSyntax>, render: &str) -> Vec<Highlight> {
    let mut hl = vec![Highlight::Normal; render.len()];

    let syntax = if let Some(syntax) = syntax_opt {
        syntax
    } else {
        return hl;
    };

    let chars: Vec<char> = render.chars().collect();
    let mut i: usize = 0;
    let mut previous_is_separator = true;
    let mut previous_highlight = Highlight::Normal;

    while i < chars.len() {
        let c = chars[i];

        let is_digits =
            should_highlight_as_digits(syntax.flags, c, previous_highlight, previous_is_separator);
        let is_string = should_highlight_as_string(syntax.flags, c);

        let is_single_line_comment =
            should_highlight_as_single_line_comment(syntax.single_line_comment_start, &chars, i);
        let is_keyword = should_highlight_as_keyword(
            &chars,
            i,
            previous_is_separator,
            syntax.keyword_type,
            syntax.keyword_reserved,
        );

        if is_string {
            highlight_string(
                &mut hl,
                &chars,
                &mut i,
                &mut previous_highlight,
                &mut previous_is_separator,
            );
        } else if is_digits {
            highlight_digits(
                &mut hl,
                &mut i,
                &mut previous_highlight,
                &mut previous_is_separator,
            );
        } else if is_single_line_comment {
            highlight_single_line_comment(
                &mut hl,
                &mut i,
                &mut previous_highlight,
                &mut previous_is_separator,
            );
        } else if is_keyword {
            highlight_keyword(
                &mut hl,
                &chars,
                &mut i,
                syntax.keyword_type,
                syntax.keyword_reserved,
                &mut previous_highlight,
                &mut previous_is_separator,
            );
        } else {
            previous_highlight = Highlight::Normal;
            previous_is_separator = c.is_separator();
            i += 1;
        }
    }

    hl
}

#[cfg(test)]
mod tests_editor_update_syntax {
    use super::{editor_update_syntax, EditorSyntax, Highlight, HLDB};

    const C: Highlight = Highlight::Comment;
    const R: Highlight = Highlight::KeywordReserved;
    const T: Highlight = Highlight::KeywordType;
    // Only used during the match process and not the `editor_update_syntax`
    // const M: Highlight = Highlight::Match;
    const H: Highlight = Highlight::Normal;
    const N: Highlight = Highlight::Number;
    const S: Highlight = Highlight::String;

    #[test]
    fn test_text() {
        let syntax: Option<&EditorSyntax> = Some(&HLDB[0]);
        assert_eq!([H, H, H].to_vec(), editor_update_syntax(syntax, "abc"));
    }

    #[test]
    fn test_digits() {
        let syntax: Option<&EditorSyntax> = Some(&HLDB[0]);
        assert_eq!([N, N, N].to_vec(), editor_update_syntax(syntax, "123"));
        assert_eq!(
            [H, H, H, H, N, N, N].to_vec(),
            editor_update_syntax(syntax, "abc 123")
        );
        // Invalid number but we decided to color it anyway
        assert_eq!(
            [N, N, N, H, H, H].to_vec(),
            editor_update_syntax(syntax, "123abc")
        );
        assert_eq!(
            [H, H, H, H, H, H].to_vec(),
            editor_update_syntax(syntax, "abc123")
        );
    }

    #[test]
    fn test_comments() {
        let syntax: Option<&EditorSyntax> = Some(&HLDB[0]);
        assert_eq!(
            [H, H, H, H, C, C, C, C, C, C].to_vec(),
            editor_update_syntax(syntax, "abc // abc")
        );
        assert_eq!([C, C, C, C].to_vec(), editor_update_syntax(syntax, "//12"));
        assert_eq!([N, H, N].to_vec(), editor_update_syntax(syntax, "1/2"));
        assert_eq!([H, C, C].to_vec(), editor_update_syntax(syntax, "a//"));
    }

    #[test]
    fn test_strings() {
        let syntax: Option<&EditorSyntax> = Some(&HLDB[0]);
        assert_eq!(
            [H, H, S, S, S, H, H].to_vec(),
            editor_update_syntax(syntax, "a \"b\" c")
        );
        assert_eq!(
            [H, H, S, S, S, S, S, H, H].to_vec(),
            editor_update_syntax(syntax, "a \"123\" c")
        );
    }

    #[test]
    fn test_keywords() {
        let syntax: Option<&EditorSyntax> = Some(&HLDB[0]);

        assert_eq!([R, R].to_vec(), editor_update_syntax(syntax, "if"));

        assert_eq!([T, T, T].to_vec(), editor_update_syntax(syntax, "int"));
        assert_eq!(
            [R, R, H, H, H, H, H, H, N, H].to_vec(),
            editor_update_syntax(syntax, "if (a < 3)")
        );
        assert_eq!(
            [T, T, T, H, H, H, H, H, N, N, H].to_vec(),
            editor_update_syntax(syntax, "int n = 20;")
        );
        assert_eq!(
            [T, T, T, H, H, H, H, H, N, N, H, H, C, C, C, C].to_vec(),
            editor_update_syntax(syntax, "int n = 20; // a")
        );

        assert_eq!(
            [H, H, H, H, R, R, H, H, H, H, C, C, C, C, C].to_vec(),
            editor_update_syntax(syntax, "abc if ee // ad")
        );

        assert_eq!(
            [H, H, H, H, H, R, R, H, H, H, H, C, C, C, C, C].to_vec(),
            editor_update_syntax(syntax, "abcd if ee // ad")
        );
    }
}

/// switch (hl) {
///   case HL_COMMENT: return 36;
///   case HL_KEYWORD1: return 33;
///   case HL_KEYWORD2: return 32;
///   case HL_NUMBER: return 31;
///   case HL_MATCH: return 34;
///   case HL_STRING: return 35;
///   default: return 37;
/// }
fn editor_syntax_to_color(h: Highlight) -> &'static [u8] {
    use colors::{
        BLUE_FOREGROUND, CYAN_FOREGROUND, DEFAULT_FOREGROUND, GREEN_FOREGROUND, MAGENTA_FOREGROUND,
        RED_FOREGROUND, YELLOW_FOREGROUND,
    };
    use Highlight::{Comment, KeywordReserved, KeywordType, Match, Normal, Number, String};

    match h {
        Comment => CYAN_FOREGROUND,
        KeywordReserved => YELLOW_FOREGROUND,
        KeywordType => GREEN_FOREGROUND,
        Match => BLUE_FOREGROUND,
        Normal => DEFAULT_FOREGROUND,
        Number => RED_FOREGROUND,
        String => MAGENTA_FOREGROUND,
    }
}

/// E.syntax = NULL;
/// if (E.filename == NULL) return;
/// char *ext = strrchr(E.filename, '.');
/// for (unsigned int j = 0; j < HLDB_ENTRIES; j++) {
///   struct editorSyntax *s = &HLDB[j];
///   unsigned int i = 0;
///   while (s->filematch[i]) {
///     int is_ext = (s->filematch[i][0] == '.');
///     if ((is_ext && ext && !strcmp(ext, s->filematch[i])) ||
///         (!is_ext && strstr(E.filename, s->filematch[i]))) {
///       E.syntax = s;
///       int filerow;
///       for (filerow = 0; filerow < E.numrows; filerow++) {
///         editorUpdateSyntax(&E.row[filerow]);
///       }
///       return;
///     }
///     i++;
///   }
/// }
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
/// ---
/// int editorRowCxToRx(erow *row, int cx) {
///   int rx = 0;
///   int j;
///   for (j = 0; j < cx; j++) {
///     if (row->chars[j] == '\t')
///       rx += (KILO_TAB_STOP - 1) - (rx % KILO_TAB_STOP);
///     rx++;
///   }
///   return rx;
/// }
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

/// int cur_rx = 0;
/// int cx;
/// for (cx = 0; cx < row->size; cx++) {
///   if (row->chars[cx] == '\t')
///     cur_rx += (KILO_TAB_STOP - 1) - (cur_rx % KILO_TAB_STOP);
///   cur_rx++;
///   if (cur_rx > rx) return cx;
/// }
/// return cx;
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
///---
/// void editorUpdateRow(erow *row) {
///   int tabs = 0;
///   int j;
///   for (j = 0; j < row->size; j++)
///     if (row->chars[j] == '\t') tabs++;
///   free(row->render);
///   row->render = malloc(row->size + tabs*(KILO_TAB_STOP - 1) + 1);
///   int idx = 0;
///   for (j = 0; j < row->size; j++) {
///     if (row->chars[j] == '\t') {
///       row->render[idx++] = ' ';
///       while (idx % KILO_TAB_STOP != 0) row->render[idx++] = ' ';
///     } else {
///       row->render[idx++] = row->chars[j];
///     }
///   }
///   row->render[idx] = '\0';
///   row->rsize = idx;
/// }
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

/// if (at < 0 || at >= E.numrows) return;
/// editorFreeRow(&E.row[at]);
/// memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numrows - at - 1));
/// E.numrows--;
/// E.dirty++;
fn editor_delete_row(editor_config: &mut EditorConfig, at: usize) -> Option<ERow> {
    if editor_config.rows.len() < at {
        None
    } else {
        editor_config.dirty = true;
        Some(editor_config.rows.remove(at))
    }
}

/// if (at < 0 || at > row->size) at = row->size;
/// row->chars = realloc(row->chars, row->size + 2);
/// memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
/// row->size++;
/// row->chars[at] = c;
/// editorUpdateRow(row);
fn editor_row_insert_char(syntax: Option<&EditorSyntax>, row: &mut ERow, at: usize, c: char) {
    if row.chars.is_char_boundary(at) {
        row.chars.insert(at, c);
    } else {
        row.chars.push(c);
    }
    row.render = editor_update_row(TAB_STOP, &row.chars);
    row.hl = editor_update_syntax(syntax, &row.render);
}

/// row->chars = realloc(row->chars, row->size + len + 1);
/// memcpy(&row->chars[row->size], s, len);
/// row->size += len;
/// row->chars[row->size] = '\0';
/// editorUpdateRow(row);
/// E.dirty++;
fn editor_row_append_string(syntax: Option<&EditorSyntax>, row: &mut ERow, s: &str) {
    row.chars.push_str(s);
    row.render = editor_update_row(TAB_STOP, &row.chars);
    row.hl = editor_update_syntax(syntax, &row.render);
}

/// Removes the character on the left of the cursor.
/// ---
/// if (at < 0 || at >= row->size) return;
/// memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
/// row->size--;
/// editorUpdateRow(row);
/// E.dirty++;
fn editor_row_delete_char(syntax: Option<&EditorSyntax>, row: &mut ERow, at: usize) {
    if at == 0 || row.chars.len() < at {
        return;
    }
    row.chars.remove(at - 1);
    row.render = editor_update_row(TAB_STOP, &row.chars);
    row.hl = editor_update_syntax(syntax, &row.render);
}

/*** editor operations ***/

/// Inserts a character at current cursor position. Contrary to the original
/// implementation, does not update the position of the cursor. The original
/// version expects `editor_refresh_screen` to update the rendered position of
/// the cursor. In this version `editor_refresh_screen` does not mutate any
/// state so the cursor needs to be moved with `editor_move_cursor` explicitly
/// instead. This is done by the InsertChar event instead.
/// ---
/// if (E.cy == E.numrows) {
///   editorInsertRow(E.numrows, "", 0);
/// }
/// editorRowInsertChar(&E.row[E.cy], E.cx, c);
/// E.cx++;
fn editor_insert_char(editor_config: &mut EditorConfig, c: char) {
    let (cursor_x, cursor_y) = editor_config.cursor;
    let cx = cursor_x as usize;
    let cy = cursor_y as usize;
    if editor_config.rows.len() == cy {
        let chars = c.to_string();
        let render = editor_update_row(TAB_STOP, &chars);
        let hl = editor_update_syntax(editor_config.syntax, &render);
        editor_config.rows.push(ERow { chars, hl, render });
    } else if let Some(row) = editor_config.rows.get_mut(cy) {
        editor_row_insert_char(editor_config.syntax, row, cx, c);
    }
    editor_config.dirty = true;
}

/// if (E.cx == 0) {
///   editorInsertRow(E.cy, "", 0);
/// } else {
///   erow *row = &E.row[E.cy];
///   editorInsertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
///   row = &E.row[E.cy];
///   row->size = E.cx;
///   row->chars[row->size] = '\0';
///   editorUpdateRow(row);
/// }
/// E.cy++;
/// E.cx = 0;
fn editor_insert_newline(editor_config: &mut EditorConfig) {
    let (cursor_x, cursor_y) = editor_config.cursor;
    let cy = cursor_y as usize;
    if (0, 0) == (cursor_x, cursor_y) || editor_config.rows.len() == cy {
        editor_config.rows.insert(
            cy,
            ERow {
                chars: "".to_string(),
                hl: Vec::new(),
                render: "".to_string(),
            },
        );
    } else if let Some(mut row) = editor_config.rows.get_mut(cy) {
        let cx = cursor_x as usize;
        let chars = row.chars.split_off(cx);
        row.render = editor_update_row(TAB_STOP, &row.chars);
        row.hl = editor_update_syntax(editor_config.syntax, &row.render);
        // New line
        let render = editor_update_row(TAB_STOP, &chars);
        let hl = editor_update_syntax(editor_config.syntax, &render);
        editor_config
            .rows
            .insert(cy + 1, ERow { chars, hl, render });
    }
}

/// if (E.cy == E.numrows) return;
/// if (E.cx == 0 && E.cy == 0) return;
/// erow *row = &E.row[E.cy];
/// if (E.cx > 0) {
///   editorRowDelChar(row, E.cx - 1);
///   E.cx--;
/// } else {
///   E.cx = E.row[E.cy - 1].size;
///   editorRowAppendString(&E.row[E.cy - 1], row->chars, row->size);
///   editorDelRow(E.cy);
///   E.cy--;
/// }
fn editor_delete_char(editor_config: &mut EditorConfig) {
    use std::convert::TryFrom;

    let (cursor_x, cursor_y) = editor_config.cursor;
    let cx = cursor_x as usize;
    let cy = cursor_y as usize;
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
            editor_row_append_string(
                editor_config.syntax,
                &mut editor_config.rows[cy - 1],
                &current_row.chars,
            );
        }
    } else if let Some(row) = editor_config.rows.get_mut(cy) {
        editor_row_delete_char(editor_config.syntax, row, cx);
    }
    editor_config.dirty = true;
}

/*** file i/o ***/

/// int totlen = 0;
/// int j;
/// for (j = 0; j < E.numrows; j++)
///   totlen += E.row[j].size + 1;
/// *buflen = totlen;
/// char *buf = malloc(totlen);
/// char *p = buf;
/// for (j = 0; j < E.numrows; j++) {
///   memcpy(p, E.row[j].chars, E.row[j].size);
///   p += E.row[j].size;
///   *p = '\n';
///   p++;
/// }
/// return buf;
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
/// ---
/// free(E.filename);
/// E.filename = strdup(filename);
/// FILE *fp = fopen(filename, "r");
/// if (!fp) die("fopen");
/// char *line = NULL;
/// size_t linecap = 0;
/// ssize_t linelen;
/// while ((linelen = getline(&line, &linecap, fp)) != -1) {
///   while (linelen > 0 && (line[linelen - 1] == '\n' ||
///                          line[linelen - 1] == '\r'))
///     linelen--;
///   editorAppendRow(line, linelen);
/// }
/// free(line);
/// fclose(fp);
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

    while let Some(Ok(chars)) = lines.next() {
        let render = editor_update_row(TAB_STOP, &chars);
        let hl = editor_update_syntax(editor_config.syntax, &render);
        editor_config.rows.push(ERow { chars, hl, render });
    }

    editor_config.dirty = false;
    Result::Ok(())
}

/// Returns the number of bytes written or an Error
/// ---
/// if (E.filename == NULL) {
///   E.filename = editorPrompt("Save as: %s (ESC to cancel)");
///   if (E.filename == NULL) {
///     editorSetStatusMessage("Save aborted");
///     return;
///   }
///   editorSelectSyntaxHighlight();
/// }
/// int len;
/// char *buf = editorRowsToString(&len);
/// int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
/// if (fd != -1) {
///   if (ftruncate(fd, len) != -1) {
///     if (write(fd, buf, len) == len) {
///       close(fd);
///       free(buf);
///       editorSetStatusMessage("%d bytes written to disk", len);
///       return;
///     }
///   }
///   close(fd);
/// }
/// free(buf);
/// editorSetStatusMessage("Can't save! I/O error: %s", strerror(errno));
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
/// ---
/// static int last_match = -1;
/// static int direction = 1;
/// static int saved_hl_line;
/// static char *saved_hl = NULL;
/// if (saved_hl) {
///   memcpy(E.row[saved_hl_line].hl, saved_hl, E.row[saved_hl_line].rsize);
///   free(saved_hl);
///   saved_hl = NULL;
/// }
/// if (key == '\r' || key == '\x1b') {
///   last_match = -1;
///   direction = 1;
///   return;
/// } else if (key == ARROW_RIGHT || key == ARROW_DOWN) {
///   direction = 1;
/// } else if (key == ARROW_LEFT || key == ARROW_UP) {
///   direction = -1;
/// } else {
///   last_match = -1;
///   direction = 1;
/// }
/// if (last_match == -1) direction = 1;
/// int current = last_match;
/// int i;
/// for (i = 0; i < E.numrows; i++) {
///   current += direction;
///   if (current == -1) current = E.numrows - 1;
///   else if (current == E.numrows) current = 0;
///   erow *row = &E.row[current];
///   char *match = strstr(row->render, query);
///   if (match) {
///     last_match = current;
///     E.cy = current;
///     E.cx = editorRowRxToCx(row, match - row->render);
///     E.rowoff = E.numrows;
///     saved_hl_line = current;
///     saved_hl = malloc(row->rsize);
///     memcpy(saved_hl, row->hl, row->rsize);
///     memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
///     break;
///   }
/// }
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

/// int saved_cx = E.cx;
/// int saved_cy = E.cy;
/// int saved_coloff = E.coloff;
/// int saved_rowoff = E.rowoff;
/// char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)",
///                            editorFindCallback);
/// if (query) {
///   free(query);
/// } else {
///   E.cx = saved_cx;
///   E.cy = saved_cy;
///   E.coloff = saved_coloff;
///   E.rowoff = saved_rowoff;
/// }
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
/// E.rx = 0;
/// if (E.cy < E.numrows) {
///   E.rx = editorRowCxToRx(&E.row[E.cy], E.cx);
/// }
/// if (E.cy < E.rowoff) {
///   E.rowoff = E.cy;
/// }
/// if (E.cy >= E.rowoff + E.screenrows) {
///   E.rowoff = E.cy - E.screenrows + 1;
/// }
/// if (E.rx < E.coloff) {
///   E.coloff = E.rx;
/// }
/// if (E.rx >= E.coloff + E.screencols) {
///   E.coloff = E.rx - E.screencols + 1;
/// }
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
/// ---
/// int y;
/// for (y = 0; y < E.screenrows; y++) {
///   int filerow = y + E.rowoff;
///   if (filerow >= E.numrows) {
///     if (E.numrows == 0 && y == E.screenrows / 3) {
///       char welcome[80];
///       int welcomelen = snprintf(welcome, sizeof(welcome),
///         "Kilo editor -- version %s", KILO_VERSION);
///       if (welcomelen > E.screencols) welcomelen = E.screencols;
///       int padding = (E.screencols - welcomelen) / 2;
///       if (padding) {
///         abAppend(ab, "~", 1);
///         padding--;
///       }
///       while (padding--) abAppend(ab, " ", 1);
///       abAppend(ab, welcome, welcomelen);
///     } else {
///       abAppend(ab, "~", 1);
///     }
///   } else {
///     int len = E.row[filerow].rsize - E.coloff;
///     if (len < 0) len = 0;
///     if (len > E.screencols) len = E.screencols;
///     char *c = &E.row[filerow].render[E.coloff];
///     unsigned char *hl = &E.row[filerow].hl[E.coloff];
///     int current_color = -1;
///     int j;
///     for (j = 0; j < len; j++) {
///       if (hl[j] == HL_NORMAL) {
///         if (current_color != -1) {
///           abAppend(ab, "\x1b[39m", 5);
///           current_color = -1;
///         }
///         abAppend(ab, &c[j], 1);
///       } else {
///         int color = editorSyntaxToColor(hl[j]);
///         if (color != current_color) {
///           current_color = color;
///           char buf[16];
///           int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
///           abAppend(ab, buf, clen);
///         }
///         abAppend(ab, &c[j], 1);
///       }
///     }
///     abAppend(ab, "\x1b[39m", 5);
///   }
///   abAppend(ab, "\x1b[K", 3);
///   abAppend(ab, "\r\n", 2);
/// }
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
                if highlight != hl[line_begin + i] {
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

/// abAppend(ab, "\x1b[7m", 4);
/// char status[80], rstatus[80];
/// int len = snprintf(status, sizeof(status), "%.20s - %d lines %s",
///   E.filename ? E.filename : "[No Name]", E.numrows,
///   E.dirty ? "(modified)" : "");
/// int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d",
///   E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numrows);
/// if (len > E.screencols) len = E.screencols;
/// abAppend(ab, status, len);
/// while (len < E.screencols) {
///   if (E.screencols - len == rlen) {
///     abAppend(ab, rstatus, rlen);
///     break;
///   } else {
///     abAppend(ab, " ", 1);
///     len++;
///   }
/// }
/// abAppend(ab, "\x1b[m", 3);
/// abAppend(ab, "\r\n", 2);
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

/// abAppend(ab, "\x1b[K", 3);
/// int msglen = strlen(E.statusmsg);
/// if (msglen > E.screencols) msglen = E.screencols;
/// if (msglen && time(NULL) - E.statusmsg_time < 5)
/// abAppend(ab, E.statusmsg, msglen);
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
/// ---
/// editorScroll();
/// struct abuf ab = ABUF_INIT;
/// abAppend(&ab, "\x1b[?25l", 6);
/// abAppend(&ab, "\x1b[H", 3);
/// editorDrawRows(&ab);
/// editorDrawStatusBar(&ab);
/// char buf[32];
/// snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoff) + 1,
///                                           (E.cx - E.coloff) + 1);
/// abAppend(&ab, buf, strlen(buf));
/// abAppend(&ab, "\x1b[?25h", 6);
/// write(STDOUT_FILENO, ab.b, ab.len);
/// abFree(&ab);
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
/// ---
/// va_list ap;
/// va_start(ap, fmt);
/// vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
/// va_end(ap);
/// E.statusmsg_time = time(NULL);
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

/// size_t bufsize = 128;
/// char *buf = malloc(bufsize);
/// size_t buflen = 0;
/// buf[0] = '\0';
/// while (1) {
///   editorSetStatusMessage(prompt, buf);
///   editorRefreshScreen();
///   int c = editorReadKey();
///   if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
///     if (buflen != 0) buf[--buflen] = '\0';
///   } else if (c == '\x1b') {
///     editorSetStatusMessage("");
///     free(buf);
///     return NULL;
///   } else if (c == '\r') {
///     if (buflen != 0) {
///       editorSetStatusMessage("");
///       return buf;
///     }
///   } else if (!iscntrl(c) && c < 128) {
///     if (buflen == bufsize - 1) {
///       bufsize *= 2;
///       buf = realloc(buf, bufsize);
///     }
///     buf[buflen++] = c;
///     buf[buflen] = '\0';
///   }
/// }
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
/// ---
/// erow *row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
/// switch (key) {
///   case ARROW_LEFT:
///     if (E.cx != 0) {
///       E.cx--;
///     } else if (E.cy > 0) {
///       E.cy--;
///       E.cx = E.row[E.cy].size;
///     }
///     break;
///   case ARROW_RIGHT:
///     if (row && E.cx < row->size) {
///       E.cx++;
///     } else if (row && E.cx == row->size) {
///       E.cy++;
///       E.cx = 0;
///     }
///     break;
///   case ARROW_UP:
///     if (E.cy != 0) {
///       E.cy--;
///     }
///     break;
///   case ARROW_DOWN:
///     if (E.cy < E.numrows) {
///       E.cy++;
///     }
///     break;
/// }
/// row = (E.cy >= E.numrows) ? NULL : &E.row[E.cy];
/// int rowlen = row ? row->size : 0;
/// if (E.cx > rowlen) {
///   E.cx = rowlen;
/// }
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

/// int c = editorReadKey();
/// switch (c) {
///   case '\r':
///     editorInsertNewline();
///     break;
///   case CTRL_KEY('q'):
///     write(STDOUT_FILENO, "\x1b[2J", 4);
///     write(STDOUT_FILENO, "\x1b[H", 3);
///     exit(0);
///     break;
///   case CTRL_KEY('s'):
///     editorSave();
///     break;
///   case HOME_KEY:
///     E.cx = 0;
///     break;
///   case END_KEY:
///     if (E.cy < E.numrows)
///       E.cx = E.row[E.cy].size;
///     break;
///   case CTRL_KEY('f'):
///     editorFind();
///     break;
///   case BACKSPACE:
///   case CTRL_KEY('h'):
///   case DEL_KEY:
///     if (c == DEL_KEY) editorMoveCursor(ARROW_RIGHT);
///     editorDelChar();
///     break;
///   case PAGE_UP:
///   case PAGE_DOWN:
///     {
///       if (c == PAGE_UP) {
///         E.cy = E.rowoff;
///       } else if (c == PAGE_DOWN) {
///         E.cy = E.rowoff + E.screenrows - 1;
///         if (E.cy > E.numrows) E.cy = E.numrows;
///       }
///       int times = E.screenrows;
///       while (times--)
///         editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
///     }
///     break;
///   case ARROW_UP:
///   case ARROW_DOWN:
///   case ARROW_LEFT:
///   case ARROW_RIGHT:
///     editorMoveCursor(c);
///     break;
///   case CTRL_KEY('l'):
///   case '\x1b':
///     break;
///   default:
///     editorInsertChar(c);
///     break;
/// }
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

/// E.cx = 0;
/// E.cy = 0;
/// E.rx = 0;
/// E.rowoff = 0;
/// E.coloff = 0;
/// E.numrows = 0;
/// E.row = NULL;
/// if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
/// E.screenrows -= 2;
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
