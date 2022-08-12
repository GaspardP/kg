# Parsing utf8 / utf16 with tree-sitter

```rust
const HALÅ:u32 = 123; // 4 * 1 + 1 * 2 bytes, 4 * 1 spaces
const 안녕:f32 = 4.5; // 2 * 2 bytes, 2 * 2 spaces (depends on typeface)
const CCCC:u32 = 678; // 4 * 1 bytes, 4 * 1 spaces
```

## Position

Row / Column indices
Assuming tree-sitter represent wide-chars as 2 column wide (big assumption)

- utf8 matches byte representation, not character
- utf16
  - each utf16 is "two utf8" so the position is twice the expected utf8 position
  - support for narrow and wide character as expected
  - need to convert to/from utf16


```
expected utf8: [
  (Point { row: 1, column: 17 }, Point { row: 1, column: 20 })
  (Point { row: 2, column: 17 }, Point { row: 2, column: 20 })
  (Point { row: 3, column: 17 }, Point { row: 3, column: 20 })
]

tree-parser utf8: [
  (Point { row: 1, column: 18 }, Point { row: 1, column: 21 })
  (Point { row: 2, column: 19 }, Point { row: 2, column: 22 })
  (Point { row: 3, column: 17 }, Point { row: 3, column: 20 })
]

tree-parser utf16: [
  (Point { row: 1, column: 34 }, Point { row: 1, column: 40 })
  (Point { row: 2, column: 30 }, Point { row: 2, column: 36 })
  (Point { row: 3, column: 34 }, Point { row: 3, column: 40 })
]
```


## Reference code

### utf8

```rust
let source_utf8 = r#"const CCCC:u32 = 678;"#;

const HALÅ:u32 = 123; // 17..20
const 안녕:f32 = 4.5; // 18..21
const CCCC:u32 = 678; // 17..20

let mut parser = rust();
let tree = parser.parse(
  source_utf8.as_bytes(),
  None,
).expect("Invalid test code");
let nodes = find_numbers(
  source_utf8.as_bytes(),
  &tree,
);

assert_eq!(
    vec![
        16..19,
        16..19,
        16..19,
    ],
    nodes.iter().map(|n| (n.byte_range())).collect::<Vec<_>>()
);
```

### utf16

```rust
let source_utf8 = r#"
const HALÅ:u32 = 123;
const 안녕:f32 = 4.5;
const CCCC:u32 = 678;
"#;


let source: Vec<u16> = std::str::from_utf8(&source_utf8.as_bytes())
    .unwrap()
    .encode_utf16()
    .into_iter()
    .collect();

let mut parser = rust();
let tree = parser.parse_utf16(
    source.clone(),
    None,
).expect("Invalid test code");
let nodes = find_numbers(
    &source
        .iter()
        .copied()
        .map(u16::to_ne_bytes)
        .flat_map(std::array::IntoIter::new)
        .collect::<Vec<u8>>()
        ,
    &tree
);


assert_eq!(
    vec![
        (Point::new(1, 16), Point::new(1, 19)),
        (Point::new(2, 16), Point::new(2, 19)),
        (Point::new(3, 16), Point::new(3, 19)),
    ],
    nodes.iter().map(|n| (n.start_position(), n.end_position())).collect::<Vec<_>>()
);
```
