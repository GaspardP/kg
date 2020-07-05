extern crate nix;
use nix::unistd::read;
use std::os::unix::io::RawFd;

/// char c;
/// while (read(STDIN_FILENO, &c, 1) == 1);
fn main() {
    let stdin: RawFd = 0;
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
