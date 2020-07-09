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

/*** data ***/

enum Event {
    Quit,
    None,
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
/// struct termios raw;
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
/// tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
fn disable_raw_mode(fd: RawFd, original: Termios) -> Result<(), std::io::Error> {
    tcsetattr(fd, TCSAFLUSH, &original)
}

/// int nread;
/// char c;
/// while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
///   if (nread == -1 && errno != EAGAIN) die("read");
/// }
/// return c;
fn editor_read_key(stdin: RawFd) -> Result<u8, std::io::Error> {
    fn to_io_err(_error: nix::Error) -> std::io::Error {
        use std::io::{Error, ErrorKind};
        Error::new(ErrorKind::InvalidInput, "Error during `read()`")
    }

    let mut buffer = [0u8; 1];
    // nix's `read` implementation reads a maximum of as many bytes as the
    // buffer passed in.
    while 1 != read(stdin, &mut buffer).map_err(to_io_err)? {}
    Result::Ok(buffer[0])
}

/*** output ***/

/// Writes the "ED" escape sequence (clear screen [1]) to the terminal. `\x1b`
/// starts the escape sequence and the sequence `[2J` clears the whole screen.
///
/// [1] https://vt100.net/docs/vt100-ug/chapter3.html#ED
/// ---
/// write(STDOUT_FILENO, "\x1b[2J", 4);
fn editor_refresh_screen(stdout: RawFd) -> Result<usize, std::io::Error> {
    fn to_io_err(_error: nix::Error) -> std::io::Error {
        use std::io::{Error, ErrorKind};
        Error::new(ErrorKind::Other, "Error during `write()`")
    }

    let clear_sequence = b"\x1b[2J";
    write(stdout, clear_sequence).map_err(to_io_err)
}

/*** input ***/

/// char c = editorReadKey();
/// switch (c) {
///   case CTRL_KEY('q'):
///     exit(0);
///     break;
/// }
fn editor_process_keypress(stdin: RawFd) -> Result<Event, std::io::Error> {
    let result = if editor_read_key(stdin)? == ctrl_key(b'q') {
        eprint!("no more input, exiting\r\n");
        Event::Quit
    } else {
        Event::None
    };
    Result::Ok(result)
}

/*** init ***/

fn main() -> Result<(), std::io::Error> {
    use std::os::unix::io::AsRawFd;
    let stdin: RawFd = std::io::stdin().as_raw_fd();
    let stdout: RawFd = std::io::stdout().as_raw_fd();

    let original_termios = Termios::from_fd(stdin)?;

    enable_raw_mode(stdin, original_termios)?;

    loop {
        editor_refresh_screen(stdout)?;
        match editor_process_keypress(stdin)? {
            Event::Quit => break,
            Event::None => continue,
        }
    }

    disable_raw_mode(stdin, original_termios)?;

    Result::Ok(())
}
