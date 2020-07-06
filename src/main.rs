extern crate nix;
extern crate termios;

use nix::unistd::read;
use std::os::unix::io::RawFd;
use termios::{
    tcgetattr, tcsetattr, Termios, ECHO, ICANON, ICRNL, IEXTEN, ISIG, IXON, OPOST, TCSAFLUSH,
};

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
/// "returned" to the begining of the line when calling `print!`.
///
/// Terminal attributes can be read with `tcgetattr` and changed with
/// `tcsetattr`. `TCSAFLUSH` specifies that the changes will be applied once all
/// pending output have been written to the terminal and discards any unread
/// inputs.
/// ---
/// struct termios raw;
/// tcgetattr(STDIN_FILENO, &raw);
/// raw.c_iflag &= ~(ICRNL | IXON);
/// raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
/// raw.c_oflag &= ~(OPOST);
/// tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
fn enable_raw_mode(fd: RawFd, mut termios: Termios) -> Result<(), std::io::Error> {
    tcgetattr(fd, &mut termios)?;
    termios.c_iflag &= !(ICRNL | IXON);
    termios.c_lflag &= !(ECHO | ICANON | IEXTEN | ISIG);
    termios.c_oflag &= !(OPOST);
    tcsetattr(fd, TCSAFLUSH, &termios)?;
    // Returns Result::Ok if none of the previous function calls triggered an
    // error. Errors will get automatically propagated thanks to the `?` try
    // operator.
    Ok(())
}

/// Sets the ternimal attributes back to the original
/// ---
/// tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
fn disable_raw_mode(fd: RawFd, original: Termios) -> Result<(), std::io::Error> {
    tcsetattr(fd, TCSAFLUSH, &original)?;
    Ok(())
}

/// char c;
/// while (read(STDIN_FILENO, &c, 1) == 1);
fn main() {
    let stdin: RawFd = 0;
    let original_termios = if let Result::Ok(termios) = Termios::from_fd(stdin) {
        termios
    } else {
        print!("Could not create termios instance\r\n");
        return;
    };

    if let Result::Err(e) = enable_raw_mode(stdin, original_termios) {
        print!("Could not activate raw mode: {:?}\r\n", e);
        return;
    }

    // Using an array instead of a `char` as the `read` function expects a
    // slice.
    let mut c = [0u8; 1];

    loop {
        // nix's `read` implementation reads as many bytes as the buffer passed
        // in.
        if let Ok(bytes_read) = read(stdin, &mut c) {
            let ch = c[0] as char;

            if ch.is_control() {
                print!("number of bytes read {:?}: {:?}\r\n", bytes_read, c);
            } else {
                print!(
                    "number of bytes read {:?}: {:?} ('{}')\r\n",
                    bytes_read, c, ch
                );
            }

            if 0 == bytes_read || b'q' == c[0] {
                print!("no more input, exiting\r\n");
                // breaking out of the loop instead of returning to make sure we
                // run the clean up functions.
                break;
            }
        } else {
            print!("Error while reading stdin\r\n");
        }
    }

    if let Result::Err(e) = disable_raw_mode(stdin, original_termios) {
        print!("Could not deactivate raw mode: {:?}\r\n", e);
    }
}
