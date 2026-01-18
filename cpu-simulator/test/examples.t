Tests for the output of the CPU simulator on some sample programs and RAMs,
with various configurations.


*** fib ***

  $ cpusim snippets/fib.pasm.inter -r ram-imgs/fib.bram -ms=69 -fs -nt
  
  
  Final state
  ----------------
  
  PC:	000c
  
  Register file:
  r0:			0000		0
  r1:			0005		5
  r2:			002a		42
  r3:			0005		5
  r4:			002e		46
  r5:			0005		5
  r6:			0008		8
  r7:			0000		0
  r8:			0000		0
  r9:			0000		0
  r10:		0000		0
  r11:		0000		0
  r12:		0000		0
  r13:		0000		0
  r14:		0000		0
  r15:		0000		0
  
  RAM (non-zero values):
  0000:	0005
  0001:	002a
  002b:	0001
  002c:	0001
  002d:	0002
  002e:	0003
  $ cpusim snippets/fib.pasm.inter -r ram-imgs/fib.bram -ms=69 -nfs -t
  (0000)	ld r1 r0 0
  	r1	0005
  (0001)	ld r2 r0 1
  	r2	002a
  (0002)	addi r3 r0 0
  	r3	0000
  (0003)	addi r5 r0 0
  	r5	0000
  (0004)	addi r6 r0 1
  	r6	0001
  (0005)	beq r3 r1 7
  	-
  (0006)	add r4 r2 r3
  	r4	002a
  (0007)	st r5 r4 0
  	002a	0000
  (0008)	add r6 r6 r5
  	r6	0001
  (0009)	sub r5 r6 r5
  	r5	0001
  (000a)	addi r3 r3 1
  	r3	0001
  (000b)	j -6
  	-
  (0005)	beq r3 r1 7
  	-
  (0006)	add r4 r2 r3
  	r4	002b
  (0007)	st r5 r4 0
  	002b	0001
  (0008)	add r6 r6 r5
  	r6	0002
  (0009)	sub r5 r6 r5
  	r5	0001
  (000a)	addi r3 r3 1
  	r3	0002
  (000b)	j -6
  	-
  (0005)	beq r3 r1 7
  	-
  (0006)	add r4 r2 r3
  	r4	002c
  (0007)	st r5 r4 0
  	002c	0001
  (0008)	add r6 r6 r5
  	r6	0003
  (0009)	sub r5 r6 r5
  	r5	0002
  (000a)	addi r3 r3 1
  	r3	0003
  (000b)	j -6
  	-
  (0005)	beq r3 r1 7
  	-
  (0006)	add r4 r2 r3
  	r4	002d
  (0007)	st r5 r4 0
  	002d	0002
  (0008)	add r6 r6 r5
  	r6	0005
  (0009)	sub r5 r6 r5
  	r5	0003
  (000a)	addi r3 r3 1
  	r3	0004
  (000b)	j -6
  	-
  (0005)	beq r3 r1 7
  	-
  (0006)	add r4 r2 r3
  	r4	002e
  (0007)	st r5 r4 0
  	002e	0003
  (0008)	add r6 r6 r5
  	r6	0008
  (0009)	sub r5 r6 r5
  	r5	0005
  (000a)	addi r3 r3 1
  	r3	0005
  (000b)	j -6
  	-
  (0005)	beq r3 r1 7
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-
  (000c)	j 0
  	-


