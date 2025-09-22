# To do

- RAM (new branch)
    - Replace hash table with array
    - Take a look at alternate implementation of arrays as well
    - Don't bother with optimization if all is good
    - Maybe benchmark it with hash tables
- Traces
    - Make r0 a zero-register in the traces
- Pretty printing
    - Look at pretty printing the output (to deal with tab misalignment, etc.)
    - Do this for all kinds of outputs (traces, RAM prints, register file prints,
      disassembly, etc.)
- Make a lexer/parser
    - Use Menhir
- Test out VM programs
    - Write a few VM programs
    - Translate to our assembly
    - Translate to the base assembly
    - Run on the simulator and check
- Top-level executable
    - Have an argument for initial memory map (initial RAM state) as a file
        - This should probably be a relatively simple "binary" format
        - Maybe have tools to convert from a simpler, easier-to-use text format
          to this binary format
    - Have flags for trace and final state
    - Put the trace in one file, final state in another file, maybe


# Future

- A debug mode for running
    - Users can define breakpoints
    - They can enter debug mode, display the current state,
      step through instructions, observe the side effects, etc.
    - Look at how spike and gdb do things
- Think of a "standard output format" for the simulator, that is relatively easy to
  parse.
