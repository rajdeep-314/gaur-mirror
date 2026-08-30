# forward_max.pasm

# Test forward branch overflow.
# beq maximum positive offset is 31.
# Target is placed at offset 31 as well.

beq r0 r0 target
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

target:
    spin
