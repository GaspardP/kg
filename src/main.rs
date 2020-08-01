/*** includes ***/

extern crate nix;
extern crate termios;

use nix::unistd::{read, write};
use std::cmp::min;
use std::os::unix::io::RawFd;
use termios::os::target::{VMIN, VTIME};
use termios::{
    tcgetattr, tcsetattr, Termios, BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, INPCK, ISIG, ISTRIP,
    IXON, OPOST, TCSAFLUSH,
};

/*** define ***/

const PKG_NAME: &str = env!("CARGO_PKG_NAME");
const PKG_VERSION: &str = env!("CARGO_PKG_VERSION");

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
    Char(u8),
    Ctrl(u8),
    Delete,
    End,
    Home,
    Page(Direction),
}

const CLEAR_LINE: &[u8; 3] = b"\x1b[K";
const CLEAR_SCREEN: &[u8; 4] = b"\x1b[2J";
const CURSOR_HOME: &[u8; 3] = b"\x1b[H";
const HIDE_CURSOR: &[u8; 6] = b"\x1b[?25l";
const SHOW_CURSOR: &[u8; 6] = b"\x1b[?25h";

/*** data ***/

type Line = u32;
type Column = u32;

/// Defines an application specific `Error` which will be used to wrap and
/// manage the errors from the different used libraries.
#[derive(Debug)]
enum Error {
    Io(std::io::Error),
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
        Error::Io(error)
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
    None,
    Quit,
}

#[allow(dead_code)]
struct EditorConfig {
    cursor: (u16, u16),
    original_termios: Termios,
    row_offset: u16,
    rows: Vec<String>,
    stdin: RawFd,
    stdout: RawFd,
    screen_rows: u16,
    screen_cols: u16,
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
            return Result::Ok(Key::Char(c));
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
            _ => Key::Char(c),
        }
    } else if is_ctrl(c) {
        // Get the char out of the value to make it easier to match
        Key::Ctrl(c | CTRL_MASK)
    } else {
        Key::Char(c)
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

/*** file i/o ***/

/// All the size and allocation are handled by the Vect type in Rust. The
/// `BufRead::lines()` splits the lines on carriage returns and removes them
/// from the result. We don't need to read the input one char at the time and
/// can just save the String in the rows.
/// ---
/// FILE *fp = fopen(filename, "r");
/// if (!fp) die("fopen");
/// char *line = NULL;
/// size_t linecap = 0;
/// ssize_t linelen;
/// linelen = getline(&line, &linecap, fp);
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

    let file = File::open(filename)?;
    let reader = BufReader::new(file);
    let mut lines = reader.lines();

    while let Some(Ok(line)) = lines.next() {
        editor_config.rows.push(line);
    }

    Result::Ok(())
}

/*** output ***/

/// if (E.cy < E.rowoff) {
///   E.rowoff = E.cy;
/// }
/// if (E.cy >= E.rowoff + E.screenrows) {
///   E.rowoff = E.cy - E.screenrows + 1;
/// }
fn editor_scroll(editor_config: &mut EditorConfig) {
    let (_cx, cy) = editor_config.cursor;
    let row_offset = editor_config.row_offset;
    let screen_rows = editor_config.screen_rows;

    if cy < row_offset {
        editor_config.row_offset = cy;
    }

    if cy >= row_offset + screen_rows {
        editor_config.row_offset = cy - screen_rows + 1;
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
///     int len = E.row[filerow].size;
///     if (len > E.screencols) len = E.screencols;
///     abAppend(ab, E.row[filerow].chars, len);
///   }
///   abAppend(ab, "\x1b[K", 3);
///   if (y < E.screenrows - 1) {
///     abAppend(ab, "\r\n", 2);
///   }
/// }
fn editor_draw_rows(editor_config: &EditorConfig, ab: &mut Vec<u8>) {
    use std::cmp::Ordering;
    let row_offset = editor_config.row_offset as usize;
    let screen_rows = editor_config.screen_rows as usize;
    let screen_cols = editor_config.screen_cols as usize;
    let rows = &editor_config.rows;

    for y in 0..(screen_rows) {
        let file_row = y + row_offset;
        if let Some(line) = rows.get(file_row) {
            let truncate = min(line.len(), screen_cols);
            ab.extend(&line.as_bytes()[..truncate]);
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
        if y < screen_rows - 1 {
            ab.extend(b"\r\n");
        }
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
/// char buf[32];
/// snprintf(buf, sizeof(buf), "\x1b[%d;%dH", E.cy + 1, E.cx + 1);
/// abAppend(&ab, buf, strlen(buf));
/// abAppend(&ab, "\x1b[?25h", 6);
/// write(STDOUT_FILENO, ab.b, ab.len);
/// abFree(&ab);
fn editor_refresh_screen(editor_config: &EditorConfig) -> Result<(), Error> {
    let row_offset = editor_config.row_offset;
    let stdout = editor_config.stdout;

    let mut ab = Vec::<u8>::with_capacity(22);
    ab.extend(HIDE_CURSOR);
    ab.extend(CURSOR_HOME);
    editor_draw_rows(editor_config, &mut ab);

    let (cx, cy) = editor_config.cursor;
    // The cursor escape sequence is 1-indexed. The code sequence parameters are
    // (rows, cols) which means (cy, cx). `cy` refers to the position in the the
    // file, so the `row_offset` is used to translate that to a position on the
    // screen.
    let move_cursor = format!("\x1b[{};{}H", cy + 1 - row_offset, cx + 1);
    ab.extend(move_cursor.as_bytes());

    ab.extend(SHOW_CURSOR);

    write(stdout, &ab)?;
    Result::Ok(())
}

/*** input ***/

/// Using "saturating" addition and substraction to have controlled overflow.
/// Thanks to that the min check can be skipped for substractions.
/// ---
/// switch (key) {
///   case ARROW_LEFT:
///     if (E.cx != 0) {
///       E.cx--;
///     }
///     break;
///   case ARROW_RIGHT:
///     if (E.cx != E.screencols - 1) {
///       E.cx++;
///     }
///     break;
///   case ARROW_UP:
///     if (E.cy != 0) {
///       E.cy--;
///     }
///     break;
///   case ARROW_DOWN:
///     if (E.cy != E.screenrows - 1) {
///       E.cy++;
///     }
///     break;
/// }
fn editor_move_cursor(editor_config: &mut EditorConfig, direction: &Direction, times: u16) {
    use std::convert::TryFrom;
    use Direction::{Down, Left, Right, Up};

    let (cx, cy) = editor_config.cursor;
    let max_cx = cx.saturating_add(times);
    let max_cy = cy.saturating_add(times);
    let max_x = editor_config.screen_cols - 1;
    let max_y = u16::try_from(editor_config.rows.len()).unwrap_or(u16::MAX);

    let cursor = match direction {
        Down => (cx, min(max_y, max_cy)),
        Up => (cx, cy.saturating_sub(times)),
        Right => (min(max_x, max_cx), cy),
        Left => (cx.saturating_sub(times), cy),
    };

    editor_config.cursor = cursor;

    // After a cursor move, might need to scroll
    editor_scroll(editor_config);
}

/// char c = editorReadKey();
/// switch (c) {
///   case CTRL_KEY('q'):
///     exit(0);
///     break;
///
///   case HOME_KEY:
///     E.cx = 0;
///     break;
///   case END_KEY:
///     E.cx = E.screencols - 1;
///     break;
///
///   case PAGE_UP:
///   case PAGE_DOWN:
///     {
///       int times = E.screenrows;
///       while (times--)
///         editorMoveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
///     }
///     break;
///
///   case ARROW_UP:
///   case ARROW_DOWN:
///   case ARROW_LEFT:
///   case ARROW_RIGHT:
///     editorMoveCursor(c);
///     break;
/// }
fn editor_process_keypress(editor_config: &EditorConfig) -> Result<Event, Error> {
    let cols = editor_config.screen_cols;
    let rows = editor_config.screen_rows;
    let result = match editor_read_key(editor_config)? {
        Key::Arrow(direction) => Event::CursorMove(direction, 1),
        Key::Ctrl(b'q') => Event::Quit,
        Key::End => Event::CursorMove(Direction::Right, cols),
        Key::Home => Event::CursorMove(Direction::Left, cols),
        Key::Page(direction) => Event::CursorMove(direction, rows),
        _ => Event::None,
    };
    Result::Ok(result)
}

/*** init ***/

/// if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
fn init_editor(stdin: RawFd, stdout: RawFd) -> Result<EditorConfig, Error> {
    let original_termios = Termios::from_fd(stdin)?;
    enable_raw_mode(stdin, original_termios)?;
    let (screen_rows, screen_cols) = get_window_size(stdin, stdout)?;
    let editor_config = EditorConfig {
        cursor: (0, 0),
        original_termios,
        row_offset: 0,
        rows: vec![],
        stdin,
        stdout,
        screen_rows,
        screen_cols,
    };
    Result::Ok(editor_config)
}

fn main() -> Result<(), Error> {
    use std::os::unix::io::AsRawFd;

    let stdin: RawFd = std::io::stdin().as_raw_fd();
    let stdout: RawFd = std::io::stdout().as_raw_fd();
    let mut editor_config = init_editor(stdin, stdout)?;

    let args: Vec<String> = std::env::args().collect();
    if let Some(filename) = args.get(1) {
        editor_open(&mut editor_config, filename)?;
    }

    loop {
        editor_refresh_screen(&editor_config)?;
        match editor_process_keypress(&editor_config)? {
            Event::Quit => break,
            Event::CursorMove(direction, amount) => {
                editor_move_cursor(&mut editor_config, &direction, amount);
            }
            Event::None => continue,
        }
    }

    Result::Ok(())
}
