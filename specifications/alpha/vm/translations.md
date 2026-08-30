# VM Translation

This file contains the translation VM instruction to pasm assembly .

## The Memory Layout

```text

|------------------|
|    0  -  15      | // temp
|------------------|
|    16 - 255      | // static
|------------------|
|                  |
|   255 - 65535    | // stack
|------------------|
```

### The registers are the following

| register |     utility      |
|----------|------------------|
|   zero   |   zero register  |
|    sp    |  stack pointer   |
|    lp    |  local pointer   |
|    ap    | argument pointer |
| w1,w2,w3 | scratch regiters |


## Instruction translations


### Push

`push const i`

```pasm
li   w1 i
st   w1 sp 0
addi sp sp 1
```

`push temp i`

```pasm
li   w1 i
ld   w1 w1 0
st   w1 sp 0
addi sp sp 1
```

`push static i`

```pasm
li   w1 16+i
ld   w1 w1 0
st   w1 sp 0
addi sp sp 1
```

`push local i`

```pasm
li   w2 i
add  w1 lp w2
ld   w1 w1 0
st   w1 sp 0
addi sp sp 1
```

`push argument i`

```pasm
li   w2 i
add  w1 lp w2
ld   w1 w1 0
st   w1 sp 0
addi sp sp 1
```

### Pop

> note : pop const i , is not a valid vm instruction

`pop temp i`

```pasm
li   w1 i
addi sp sp -1
ld   w2 sp 0
st   w2 w1 0
```

`pop static i`

```pasm
li   w1 16+i
addi sp sp -1
ld   w2 sp 0
st   w2 w1 0
```

`pop local i`

```pasm
li   w1 i
add  w1 lp w1
addi sp sp -1
ld   w2 sp 0
st   w2 w1 0
```


`pop argument i`

```pasm
li   w1 i
add  w1 ap w1
addi sp sp -1
ld   w2 sp 0
st   w2 w1 0
```

### Arithemetic and Boolean

`add`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp sp -1
ld   w2 sp 0

add  w1 w2 w1

st   w1 sp 0
addi sp sp 1
```

`sub`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp sp -1
ld   w2 sp 0

sub  w1 w2 w1

st   w1 sp 0
addi sp sp 1
```

`neg`

```pasm
addi sp sp -1
ld   w1 sp 0

sub  w1 zero w1

st   w1 sp 0
addi sp sp 1
```

`and`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp sp -1
ld   w2 sp 0

and  w1 w2 w1

st   w1 sp 0
addi sp sp 1
```

`not`

```pasm
addi sp sp -1
ld   w1 sp 0

not  w1 w1

st   w1 sp 0
addi sp sp 1
```

`or`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp sp -1
ld   w2 sp 0

not  w1 w1
not  w2 w2
and  w1 w1 w2
not  w1 w1

st   w1 sp 0
addi sp sp 1
```

### Branch

`eq`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp sp -1
ld   w2 sp 0

beq  w1 w2 EQ_TRUE
j    EQ_FALSE

label EQ_TRUE
addi w1 zero -1
j    EQ_DONE

label EQ_FALSE
addi w1 zero 0

label EQ_DONE
st   w1 sp 0
addi sp sp 1
```

`lt`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp sp -1
ld   w2 sp 0

blt  w2 w1 LT_TRUE  // w2 < w1
j    LT_FALSE

label LT_TRUE
addi w1 zero -1
j    LT_DONE

label LT_FALSE
addi w1 zero 0

label LT_DONE
st   w1 sp 0
addi sp sp 1
```

`gt`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp sp -1
ld   w2 sp 0

blt  w2 w1 LT_TRUE   // w2 < w1
j    LT_FALSE

label LT_TRUE
addi w1 zero -1
j    LT_DONE

label LT_FALSE
addi w1 zero 0

label LT_DONE
st   w1 sp 0
addi sp sp 1
```

### Jumps and Labels

`Label l`

```pasm
label l
```

`goto l`

```pasm
j  l
```


`if-goto l`

```pasm
addi sp sp -1
ld   w1 sp 0

not  w1 w1

beq  w1 zero IF_TRUE
j    IF_FALSE

label IF_TRUE
j    l

label IF_FALSE
```


### Function call and return

`call f args`

```pasm
st   lp sp 1
st   ap sp 2

addi lp sp 3
li   w1 n_args
sub  ap sp w1

addi w2 zero 0
apc  w2 6

st   w2 sp 0
addi sp sp 3

j    f_label
```

`ret`

```pasm
addi sp sp -1
ld   w1 sp 0

addi sp lp -3

ld   w2 sp 2
ld   lp sp 1
addi ap w2 0
ld   w3 sp 0

addi sp w2 0

st   w1 sp 0
addi sp sp 1

jalr r0 w3 0
```
