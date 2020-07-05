extern crate nix;
extern crate termios;

use nix::unistd::read;
use std::os::unix::io::RawFd;
use termios::{tcgetattr, tcsetattr, Termios, ECHO, TCSAFLUSH};

/// The `ECHO` feature prints each key typed in the terminal. This is the
/// default behaviour in cannonical mode. This function makes sure the feature
/// is deactivated.
///
/// Terminal attributes can be read with `tcgetattr` and changed with
/// `tcsetattr`. `TCSAFLUSH` specifies that the changes will be applied once all
/// pending output have been written to the terminal and discards any unread
/// inputs.
/// ---
/// struct termios raw;
/// tcgetattr(STDIN_FILENO, &raw);
/// raw.c_lflag &= ~(ECHO);
/// tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
fn enable_raw_mode(fd: RawFd) -> Result<(), std::io::Error> {
    let mut termios = Termios::from_fd(fd)?;
    tcgetattr(fd, &mut termios)?;
    termios.c_lflag &= !(ECHO);
    tcsetattr(fd, TCSAFLUSH, &termios)?;
    // Returns Result::Ok if none of the previous function calls triggered an
    // error. Errors will get automatically propagated thanks to the `?` try
    // operator.
    Ok(())
}

/// char c;
/// while (read(STDIN_FILENO, &c, 1) == 1);
fn main() {
    let stdin: RawFd = 0;

    if enable_raw_mode(stdin).is_err() {
        println!("Could not activate raw mode");
        return;
    }

    // Using an array instead of a `char` as the `read` function expects a
    // slice.
    let mut c = [0u8; 1];

    loop {
        // nix's `read` implementation reads as many bytes as the buffer passed
        // in.
        if let Ok(bytes_read) = read(stdin, &mut c) {
            println!("number of bytes read {:?}: {:?}", bytes_read, c);

            if 0 == bytes_read || b'q' == c[0] {
                println!("no more input, exiting");
                return;
            }
        } else {
            println!("Error while reading stdin");
        }
    }
}
