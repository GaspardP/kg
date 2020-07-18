/*** includes ***/

extern crate nix;
extern crate termios;

use nix::unistd::{read, write};
use std::os::unix::io::RawFd;
use termios::os::target::{VMIN, VTIME};
use termios::{
    tcgetattr, tcsetattr, Termios, BRKINT, CS8, ECHO, ICANON, ICRNL, IEXTEN, INPCK, ISIG, ISTRIP,
    IXON, OPOST, TCSAFLUSH,
};

/*** define ***/

/// Bitwise-AND with `00011111` or `0x1f` to set the upper 3 bits characters to
/// `0`. By convention the terminal strips bits 5 and 6 of the key pressed
/// together with `Ctrl`.
///---
/// #define CTRL_KEY(k) ((k) & 0x1f)
fn ctrl_key(c: u8) -> u8 {
    c & 0x1f
}

const CLEAR_SCREEN: &[u8; 4] = b"\x1b[2J";
const CURSOR_HOME: &[u8; 3] = b"\x1b[H";

/*** data ***/

type Line = u32;
type Column = u32;

/// Defines an application specific `Error` which will be used to wrap and
/// manage the errors from the different used libraries.
#[derive(Debug)]
enum Error {
    Io(std::io::Error),
    Nix(nix::Error),
    Simple(Line, Column),
}

macro_rules! simple_error {
    () => {
        Result::Err(Error::Simple(line!(), column!()))
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

enum Event {
    Quit,
    None,
}

#[allow(dead_code)]
struct EditorConfig {
    original_termios: Termios,
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
/// return c;
fn editor_read_key(editor_config: &EditorConfig) -> Result<u8, Error> {
    let stdin = editor_config.stdin;
    let mut buffer = [0u8; 1];
    // nix's `read` implementation reads a maximum of as many bytes as the
    // buffer passed in.
    while 1 != read(stdin, &mut buffer)? {}

    let c = buffer[0] as char;
    if c.is_control() {
        eprint!("read: {:?}\r\n", buffer);
    } else {
        eprint!("read: {:?} ('{}')\r\n", buffer, c);
    }

    Result::Ok(buffer[0])
}

// temp function to not have to pass the whole editor to the get_window_size
// function
fn read_key(stdin: RawFd) -> Result<char, Error> {
    let mut buffer = [0u8; 1];
    let bytes_read = read(stdin, &mut buffer)?;
    let c = buffer[0] as char;

    if c.is_control() {
        eprint!("number of bytes read {:?}: {:?}\r\n", bytes_read, buffer);
    } else {
        eprint!(
            "number of bytes read {:?}: {:?} ('{}')\r\n",
            bytes_read, buffer, c
        );
    }

    Result::Ok(c)
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
/// printf("\r\n&buf[1]: '%s'\r\n", &buf[1]);
/// editorReadKey();
/// return -1;
fn get_cursor_position(stdin: RawFd, stdout: RawFd) -> Result<(u16, u16), Error> {
    let dsr_active_position = b"\x1b[6n";
    if 4 != write(stdout, dsr_active_position)? {
        return simple_error!();
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
            return simple_error!();
        }
        // save char
        buffer[i] = buf[0];
        // keep reading until we get to the 'R'
        if b'R' == buf[0] {
            break;
        }
        // or error out if the buffer was too small
        if buffer.len() <= i {
            return simple_error!();
        }
        i += 1;
    }
    print!("\r\n&buffer[0..i]: '{:?}'\r\n", &buffer[0..i]);
    let s = std::str::from_utf8(&buffer[0..i]).unwrap();
    print!("\r\n&buffer[0..i]: {:?}\r\n", s);
    read_key(stdin)?;
    simple_error!()
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
fn get_window_size(stdout: RawFd) -> Result<(u16, u16), Error> {
    use nix::libc::{ioctl, winsize, TIOCGWINSZ};
    use std::os::unix::io::AsRawFd;

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

    // TODO Remove this when the implementation is fixed
    if true || res == -1 || ws.ws_col == 0 {
        // Cursor Forward 999, Cursor Down 999
        let bytes_written = write(stdout, b"\x1b[999C\x1b[999B")?;
        if 12 != bytes_written {
            return simple_error!();
        }

        // TODO Remove. Importing stdin here directly change across the
        // whole program
        let stdin = std::io::stdin().as_raw_fd();
        get_cursor_position(stdin, stdout)
    } else {
        Result::Ok((ws.ws_row, ws.ws_col))
    }
}

/*** output ***/

/// Draws a vertical column of 24 `~`
/// ---
/// int y;
/// for (y = 0; y < E.screenrows; y++) {
///   write(STDOUT_FILENO, "~\r\n", 3);
/// }
fn editor_draw_rows(editor_config: &EditorConfig) -> Result<(), Error> {
    let stdout = editor_config.stdout;
    for _ in 0..editor_config.screen_rows {
        write(stdout, b"~\r\n")?;
    }
    Result::Ok(())
}

/// Writes the "ED" escape sequence (clear screen [1]) to the terminal. `\x1b`
/// starts the escape sequence and the sequence `[2J` clears the whole screen.
///
/// [1] https://vt100.net/docs/vt100-ug/chapter3.html#ED
/// ---
/// write(STDOUT_FILENO, "\x1b[2J", 4);
/// write(STDOUT_FILENO, "\x1b[H", 3);
/// editorDrawRows();
/// write(STDOUT_FILENO, "\x1b[H", 3);
fn editor_refresh_screen(editor_config: &EditorConfig) -> Result<(), Error> {
    let stdout = editor_config.stdout;
    write(stdout, CLEAR_SCREEN)?;
    write(stdout, CURSOR_HOME)?;
    editor_draw_rows(editor_config)?;
    write(stdout, CURSOR_HOME)?;
    Result::Ok(())
}

/*** input ***/

/// char c = editorReadKey();
/// switch (c) {
///   case CTRL_KEY('q'):
///     exit(0);
///     break;
/// }
fn editor_process_keypress(editor_config: &EditorConfig) -> Result<Event, Error> {
    let result = if editor_read_key(editor_config)? == ctrl_key(b'q') {
        eprint!("no more input, exiting\r\n");
        Event::Quit
    } else {
        Event::None
    };
    Result::Ok(result)
}

/*** init ***/

/// if (getWindowSize(&E.screenrows, &E.screencols) == -1) die("getWindowSize");
fn init_editor(stdin: RawFd, stdout: RawFd) -> Result<EditorConfig, Error> {
    let original_termios = Termios::from_fd(stdin)?;
    enable_raw_mode(stdin, original_termios)?;
    let (screen_rows, screen_cols) = get_window_size(stdout)?;
    let editor_config = EditorConfig {
        original_termios,
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
    let editor_config = init_editor(stdin, stdout)?;

    loop {
        editor_refresh_screen(&editor_config)?;
        match editor_process_keypress(&editor_config)? {
            Event::Quit => break,
            Event::None => continue,
        }
    }

    Result::Ok(())
}
