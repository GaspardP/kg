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

enum Direction {
    Down,
    Left,
    Right,
    Up,
}

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
    InsertChar(char),
    InsertNewline,
    None,
    Quit,
    Save,
}

/// typedef struct erow {
///   int size;
///   int rsize;
///   char *chars;
///   char *render;
/// } erow;
struct ERow {
    chars: String,
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

#[cfg(test)]
mod tests {
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
fn editor_row_insert_char(row: &mut ERow, at: usize, c: char) {
    if row.chars.is_char_boundary(at) {
        row.chars.insert(at, c);
    } else {
        row.chars.push(c);
    }
    row.render = editor_update_row(TAB_STOP, &row.chars);
}

/// row->chars = realloc(row->chars, row->size + len + 1);
/// memcpy(&row->chars[row->size], s, len);
/// row->size += len;
/// row->chars[row->size] = '\0';
/// editorUpdateRow(row);
/// E.dirty++;
fn editor_row_append_string(row: &mut ERow, s: &str) {
    row.chars.push_str(s);
    row.render = editor_update_row(TAB_STOP, &row.chars);
}

/// Removes the character on the left of the cursor.
/// ---
/// if (at < 0 || at >= row->size) return;
/// memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
/// row->size--;
/// editorUpdateRow(row);
/// E.dirty++;
fn editor_row_delete_char(row: &mut ERow, at: usize) {
    if at == 0 || row.chars.len() < at {
        return;
    }
    row.chars.remove(at - 1);
    row.render = editor_update_row(TAB_STOP, &row.chars);
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
        let cs = c.to_string();
        editor_config.rows.push(ERow {
            render: editor_update_row(TAB_STOP, &cs),
            chars: cs,
        });
    } else if let Some(row) = editor_config.rows.get_mut(cy) {
        editor_row_insert_char(row, cx, c);
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
                render: "".to_string(),
            },
        );
    } else if let Some(mut row) = editor_config.rows.get_mut(cy) {
        let cx = cursor_x as usize;
        let new_line = row.chars.split_off(cx);
        row.render = editor_update_row(TAB_STOP, &row.chars);
        editor_config.rows.insert(
            cy + 1,
            ERow {
                render: editor_update_row(TAB_STOP, &new_line),
                chars: new_line,
            },
        );
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
            editor_row_append_string(&mut editor_config.rows[cy - 1], &current_row.chars);
        }
    } else if let Some(row) = editor_config.rows.get_mut(cy) {
        editor_row_delete_char(row, cx);
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

    editor_config.filename = Some(filename.to_string());

    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    while let Some(Ok(line)) = lines.next() {
        editor_config.rows.push(ERow {
            render: editor_update_row(TAB_STOP, &line),
            chars: line,
        });
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
    } else if let Some(buffer) = editor_prompt(editor_config, "Save as (C-g to cancel): ")? {
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

/// Draws a vertical column of 24 `~`
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
///     int len = E.row[filerow].size - E.coloff;
///     if (len < 0) len = 0;
///     if (len > E.screencols) len = E.screencols;
///     abAppend(ab, &E.row[filerow].chars[E.coloff], len);
///   }
///   abAppend(ab, "\x1b[K", 3);
///   if (y < E.screenrows - 1) {
///     abAppend(ab, "\r\n", 2);
///   }
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
        if let Some(erow) = rows.get(file_row) {
            let line = &erow.render;
            let line_begin = min(file_col, line.len());
            let line_end = min(line.len(), screen_cols + file_col);
            ab.extend(&line.as_bytes()[line_begin..line_end]);
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
/// int len = snprintf(status, sizeof(status), "%.20s - %d lines",
///   E.filename ? E.filename : "[No Name]", E.numrows);
/// int rlen = snprintf(rstatus, sizeof(rstatus), "%d/%d",
///   E.cy + 1, E.numrows);
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
    let position = format!("{}/{}", cy + 1, numrows);
    let padding = width.saturating_sub(file_info.len());
    let status = format!(
        "{file_info}{position:>padding$}",
        file_info = file_info,
        position = position,
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
///   if (c == '\x1b') {
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
fn editor_prompt(editor_config: &mut EditorConfig, prompt: &str) -> Result<Option<String>, Error> {
    let mut buffer = String::with_capacity(128);
    loop {
        editor_set_status_message(editor_config, format!("{}{}", prompt, buffer).as_ref());
        editor_refresh_screen(editor_config)?;
        match editor_read_key(editor_config)? {
            Key::Char(c) => buffer.push(c),
            Key::Enter | Key::Ctrl('m') => {
                editor_set_status_message(editor_config, "");
                return Result::Ok(Some(buffer));
            }
            Key::Escape | Key::Ctrl('g') => {
                return Result::Ok(None);
            }
            _ => continue,
        }
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
