# To do

- RAM
    - Replace hash table with array
    - Take a look at alternate implementation of arrays as well
    - Don't bother with optimization if all is good
    - Maybe benchmark it with hash tables

- Traces
    - Make r0 a zero-register in the traces
- Make a lexer/parser
- Test out VM programs
- Top-level executable
    - Have an argument for initial memory map (initial RAM state) as a file
    - Have flags for trace and final state
    - Put the trace in one file, final state in another file, maybe

- Maybe breakpointing in the future? maybe look at how spike does it.
- pretty pritning and dealing with tab misalignment
- maybe think of an output format that is easier to parse, in the future.
