# To do


## Bugs

- `reg r100`, for example, in the debug mode, causes the simulator to quit. Change the responsible `failwith`.


## Improvements

- Go through the entire codebase and clean things up. They're somewhat messy after the assembler-dependency-rework.
- Pretty printing
    - Use some pretty printing library to deal with the tab misalignment
      issues, etc.
    - Do this for all kinds of outputs (traces, RAM outputs, register file
      outputs, disassembly, etc.)
- Add tests for lexer and parser, maybe
- Input formats
    - Both assembly and binary inputs (integrate with disassembler).
- Breakpoints
    - Add breakpoint support, maybe for assembly inputs with labels
      as breakpoints.
- Better error messages in
    - Lexer
    - Parser
    - The simulator
- Documentation
    - Write a basic README file
    - Write documentation (odoc)

