[![CircleCI](https://circleci.com/gh/nathaniel-may/filenames.svg?style=svg)](https://circleci.com/gh/nathaniel-may/filenames)

# filenames

_work in progress_</br>
This readme represents what the goal of this project is, not the current state.

Filenames is a small, pure DSL that lets you describe the format of your filenames by composing simple parsers together. This tool then compiles your source to a native app that you can run on your filenames to detect any that do not conform to your format, and explore the ones that do.

## Example

schema.txt:

```
Schema
    "-"
    [ Part "Id" [] [unique]
    , Part "Medium" ["art", "photo"]
    , Part "Source" [] [optional]
    , Part "Tags" ["architecture"] [optional]
    , Part "Tags" ["nature"] [optional]
    , Part "Tags" ["person"] [optional]
    ]
```

To run:
```
> filenames schema.txt
> ./schema
```

| Filenames                    | Match |
|------------------------------|-------|
| ABC-art-people               | ✅    |
| ABC-art-someusername-people  | ✅    |
| ABC123001-art-nature-people  | ✅    |
| AAAAAA99-photo-architecture  | ✅    |
| art-person-ABCDEF            | ❌    |
| ABCDEF-art                   | ❌    |
| art-photo-ABCDEF             | ❌    |
| art-people-01                | ❌    |

## Development

Compiled with ghc 8.8. You can install ghc from [ghcup](https://www.haskell.org/ghcup/).

run tests with `make test`

## Future work
- name the tool better
- add additional app functions like histogram reporting
- Have it work recursively across subdirectories. Would have to decide how non-local constraints like uniqueness and consequitive numbers interact with directory boundaries. Alternatively, require users to pipe strings to avoid file system interactions.
- Generate local ui from source that allows for visually iterating through the files and constructing valid usernames with checkboxes. Boxes should be pre-checked if the filename is already valid for easy editing.
- Migrate a directory from one naming scheme to another
