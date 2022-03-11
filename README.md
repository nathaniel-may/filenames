[![CircleCI](https://circleci.com/gh/nathaniel-may/filenames.svg?style=svg)](https://circleci.com/gh/nathaniel-may/filenames)

# filenames

filenames is a small DSL based on parser generators that lets you describe the format your filenames by composing simple parsers together. This tool then runs on your files to detect any files that do not conform to your format.

## Example

format.txt:

```
d := "-"

format := with_delim d [
    no_delim (==1) ["art", "photo"]
  , delim d (>=0) ["nature", "architecture", "people"]
  , id
  , no_delim (>=0) ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  ]

id := no_delim (==6) id_chars

// limited characters for example
id_chars := ["A", "B", "C", "D", "E", "F", "1", "2", "3"]
```

To run:
`filenames format.txt ./my-files/`

| Filenames                    | Match |
|------------------------------|-------|
| art-people-ABCDEF            | ✅    |
| art-nature-people-ABC123-001 | ✅    |
| photo-architecture-AAAAAA-99 | ✅    |
| art-person-ABCDEF            | ❌    |
| art-ABCDEF                   | ❌    |
| art-photo-ABCDEF             | ❌    |
| art-people-01                | ❌    |

## Bad Example
formats are not all equally valid. The following is an example of a bad format that would be difficult to parse:

```
format := with_no_delim [
    no_delim (>=1) ["A", "B", "C"]
  , no_delim (>=1) ["A", "B", "C"]
  , no_delim (>=1) ["A", "B", "C"]
  ]
```

This is a bad format because although `ABC` looks like it's intended to be a valid filename, the parser will greedily consume everything in the first parse group and complain that the following two had no matches.

## Development

run unit tests with `make test`

## Future work
- name the tool better
- add additional functions like histogram reporting
- decide on strictly greedy parsing, or ambiguity detection
- instead of interpreting, have it target Haskell and call ghc to compile an artifact
- add non-local constraints like the ability to enforce uniqueness and that incrementing counters are consecutive. 
- Have it work recursively across subdirectories. Would have to decide how non-local constraints like uniqueness and consequitive numbers interact with directory boundaries.
- Tests for local development
- Migrate a directory from one naming scheme to another
