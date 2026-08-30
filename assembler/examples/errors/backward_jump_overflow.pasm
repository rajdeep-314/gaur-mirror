# backward_jump_overflow.pasm

# Test backward branch overflow.
# beq minimum negative offset is -32.
# Target is placed at offset -33.

target:
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
spin
beq r0 r0 target
