# To do


## General

- Finish the script in demo/
- Sort out some tasks (like r0 being a zero-reg in trace)
- Make a proper top-level for ram-init
- Merge those changes from ram-init to cpu-simulator-top-level
- Merge changes from cpu-simulator-top-level to cpu-simulator
- Implement max-steps in CPU simulator
- Create branch cpu-simulator-debug-mode and start working on the debug mode
  in that branch


## Debug mode

- For step/steps, also print the instruction that was executed (regradless
  of trace)
- Print PC and instruction with the trace as well
- Maybe show current instruction with a marker while printing the program
- Even if the PC is out of bounds, keep the repl running


## Top level

- Clean up bin/
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
- Think of a "standard output format" for the simulator, that is relatively easy
  to parse.
- Documentation
