# To do


## Top level

- Implement max_steps
- RAM file
    - Make the tool?
    - Use it as a library for the simulator?

## Traces

- Make r0 a zero-register in the traces

## Pretty printing

- Look at pretty printing the output (to deal with tab misalignment, etc.)
- Do this for all kinds of outputs (traces, RAM prints, register file prints,
  disassembly, etc.)


- Lexer/parser
    - Implement comments
    - Make a lexer/parser
        - Use Menhir, maybe

## Testing

- Test out VM programs
    - Write a few VM programs
    - Translate to our assembly
    - Translate to the base assembly
    - Run on the simulator and check


# Future

- A debug mode for running
    - Users can define breakpoints
    - They can enter debug mode, display the current state,
      step through instructions, observe the side effects, etc.
    - Look at how spike and gdb do things
- Think of a "standard output format" for the simulator, that is relatively easy to
  parse.
