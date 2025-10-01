# 2. RAM sum.
# RAM[3] <- RAM[1] + RAM[2]


ld r1 r0 1
ld r2 r0 2
add r1 r1 r2
st r1 r0 3

