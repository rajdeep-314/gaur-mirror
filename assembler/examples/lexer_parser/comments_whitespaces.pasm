# comments_whitespaces.pasm

# To test whitespace and comments.

# Multiple spaces
li    r1    10

# Leading whitespace
    addi r3 r1 5

# Inline comment
add r4 r1 r2 # everything after '#' is a comment

# Blank lines and comments between instructions

sub r5 r4 r1
