# Topics

## TODO

- [ ] OCaml has nat support, add it for TBits instead of int.

- [x] Add support for declared and bound variables in environment.
      An in/out/state variable can be declared before being bound.

## Dependency Tree

Inside a chip definition, we have

1. name: string
2. input: (string, typ)
3. output: (string, typ)
4. states: (string -> typ) hashtable
5. parts: list of assignments

Graph G = (V, E)
The vertex set is the set of all variables, in, out, state or temporary
variables.
Initialise an empty set, keep adding names of variables after checking they
are not repeated, if they are, raise approprate error

Add in the in, out and all the state variables with the primed copies of state
variables, make sure they are unique

there is a function to extract the name of all variables present in an
expression, the thing apearing on the rhs of an assignment

fold over all assignments, if
assignment is of type combinational, extract the variable names appearing
on the right, check if they exist
