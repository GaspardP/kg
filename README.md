# kg - A kilo inspired editor in Rust

The goal of this project is to:

- Follow the [kilo-tutorial][kilo-tutorial] as closely as possible. The commits
  will match the chapters / sub-chapters split.
- Rely on the same features as the C implementation. The project will make use
  of the C POSIX library and the VT100 escape sequences.
- Learn about the language constructs specific to Rust and use them when it
  simplifies the code (the code should be as idiomatic as possible).
- Focus on the implementation of the editor's feature by using crates that
  abstracts the Rust/C bindings (e.g. limit the use of `unsafe`).

The project depends on:

- [nix][nix] for abstracting and providing a safe interface to the C POSIX
  library.
- [unicode-segmentation][unicode]
- [termios-rs][termios-rs] to provide safe bindings the the C termios library.


## Usage

- `Ctrl+q` to quit
- `Ctrl+s` to save the file
- the arrow keys to move the cursor
- `PgUp` and `PgDn` to move the cursor to the top or bottom of the screen
- `Home` to move the cursor at the beginning of the line
- `End` to move the cursor at the end of the line

```bash
# Build the application
cargo build

# Start the application
cargo run
```


## License

This work is based on the [kilo-tutorial][kilo-tutorial] which content is
licensed under [CC BY 4.0][CCBY40].

The original C implementation comes from [kilo][kilo]. Kilo was written by
Salvatore Sanfilippo aka [antirez][antirez] and is released under the BSD 2
clause license. The complete license is available in the `kilo.LICENSE` file.

The Rust implementation available in this project is released under the BSD 2
clause license.


[CCBY40]: https://creativecommons.org/licenses/by/4.0/
[antirez]:  https://github.com/antirez
[kilo-tutorial]: https://viewsourcecode.org/snaptoken/kilo/
[kilo]: https://github.com/antirez/kilo
[nix]: https://github.com/nix-rust/nix
[termios-rs]: https://github.com/dcuddeback/termios-rs
[unicode]: https://docs.rs/unicode-segmentation
