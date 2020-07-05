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
fn enable_raw_mode(fd: RawFd, mut termios: Termios) -> Result<(), std::io::Error> {
    tcgetattr(fd, &mut termios)?;
    termios.c_lflag &= !(ECHO);
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
        println!("Could not create termios instance");
        return;
    };

    if let Result::Err(e) = enable_raw_mode(stdin, original_termios) {
        println!("Could not activate raw mode: {:?}", e);
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
                // breaking out of the loop instead of returning to make sure we
                // run the clean up functions.
                break;
            }
        } else {
            println!("Error while reading stdin");
        }
    }

    if let Result::Err(e) = disable_raw_mode(stdin, original_termios) {
        println!("Could not deactivate raw mode: {:?}", e);
    }
}
