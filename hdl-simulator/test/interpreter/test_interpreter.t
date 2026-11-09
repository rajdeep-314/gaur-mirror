Foundational logic gates test.

  $ GATES=files/gates.phdl
  $ sim() { name=$1; shift; hdl-simulator -name $name "$@" | tr '>' '\n' | sed -e 's/^ *//' -e '/^$/d'; }
  $ tt() { for a in 0 1; do for b in 0 1; do echo "INPUT {a=$a, b=$b}"; echo OUTPUT; done; done; }

Builtin Nand gate.

  $ tt | sim Nand $GATES
  t: 0	 | 1
  t: 0	 | 1
  t: 0	 | 1
  t: 0	 | 0

Not gate.

  $ printf 'INPUT 0\nOUTPUT\nINPUT 1\nOUTPUT\n' | sim Not $GATES
  t: 0	 | 1
  t: 0	 | 0

And gate.

  $ tt | sim And $GATES
  t: 0	 | 0
  t: 0	 | 0
  t: 0	 | 0
  t: 0	 | 1

Or gate.

  $ tt | sim Or $GATES
  t: 0	 | 0
  t: 0	 | 1
  t: 0	 | 1
  t: 0	 | 1

Xor gate.

  $ tt | sim Xor $GATES
  t: 0	 | 0
  t: 0	 | 1
  t: 0	 | 1
  t: 0	 | 0

Mux gate.

  $ for sel in 0 1; do for a in 0 1; do for b in 0 1; do echo "INPUT {a=$a, b=$b, sel=$sel}"; echo OUTPUT; done; done; done | sim Mux $GATES
  t: 0	 | 0
  t: 0	 | 0
  t: 0	 | 1
  t: 0	 | 1
  t: 0	 | 0
  t: 0	 | 1
  t: 0	 | 0
  t: 0	 | 1

An ill-typed input is reported instead of crashing.

  $ printf 'INPUT {a=1}\nOUTPUT\n' | sim And $GATES
  File "stdin", characters 7-12:
  The input expression has type {a: bits 1} but an expression was expected of type 
  {a: bits 1, b: bits 1}
  t: 0	 | 0

Unknown chip.

  $ echo QUIT | hdl-simulator -name Foo $GATES
  Error: can not simulate chip Foo: no chip with this name
  [1]
