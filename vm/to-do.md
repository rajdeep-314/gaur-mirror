# To do


## VM Emulator

- Maybe generalize all functions that pattern match on s.frames
- Pass along instructions (and some more data, maybe parsing-related) to
exceptions
- Add comments about exceptions in functions that might raise exceptions.


## Suggestions

- Maybe use polymorphic variant types for IR
- Improve error messages, for example
    - Duplicate label error for emulator should mention function name and
    (syntactic) line numbers


## Translations

- Implement setting up the local segment for the function in the translation
  of `function`
- Add translation for `function` and for entire VM programs

