[![CircleCI](https://circleci.com/gh/nathaniel-may/filenames.svg?style=svg)](https://circleci.com/gh/nathaniel-may/filenames)

# filenames

_work in progress_</br>
This readme represents what the goal of this project is, not the current state.

Filenames is a small, effectless DSL based on parser generators that lets you describe the format of your filenames by composing simple parsers together. This tool then compiles your source to a native app that you can run on your filenames to detect any that do not conform to your format.

## Example

format.txt:

```
let d := "-"

let format := with_delim d [
    no_delim {== 1} ["art", "photo"]
  , delim d {>= 0} ["nature", "architecture", "people"]
  , id
  , no_delim {>= 0} ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  ]

let id := no_delim {== 6} id_chars

// limited characters for example
let id_chars := ["A", "B", "C", "D", "E", "F", "1", "2", "3"]
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
formats are not all equally valid. Here is a a bad format: 

```
let format := with_no_delim [
    no_delim {>= 1} ["A", "B", "C"]
  , no_delim {>= 1} ["A", "B", "C"]
  , no_delim {>= 1} ["A", "B", "C"]
  ]
```

This is a bad format because although `ABC` looks like it's intended to be a valid filename, the parser will greedily consume everything in the first parse group and complain that the following two had no matches. This DSL does not detect nor prevent the creation of these bad parsers.

## Development

Compiled with ghc 8.8. You can install ghc from [ghcup](https://www.haskell.org/ghcup/).

run tests with `make test`

## Future work
- name the tool better
- add additional functions like histogram reporting
- decide on strictly greedy parsing, or ambiguity detection
- add non-local constraints like the ability to enforce uniqueness and that incrementing counters are consecutive. 
- Have it work recursively across subdirectories. Would have to decide how non-local constraints like uniqueness and consequitive numbers interact with directory boundaries. Alternatively, require users to pipe strings to avoid file system interactions.
- Generate local ui from source that allows for visually iterating through the files and constructing valid usernames with checkboxes. Boxes should be pre-checked if the filename is already valid for easy editing.
- Migrate a directory from one naming scheme to another
