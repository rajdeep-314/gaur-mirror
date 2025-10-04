# NOTE:     Should be executed from inside this (demo) directory.


import sys


asm_names = [
    '1-load.pasm',
    '2-ram-sum.pasm',
    '3-initialize-array.pasm',
    '4-even-non-neg.pasm',
    '5-sorting.pasm',
    '6-fibonacci.pasm'
]

init_rams = [
    '',
    '2-ram-sum.bram',
    '',
    '4-even-non-neg.bram',
    '5-sorting.bram',
    '6-fibonacci.bram'
]

asm_base_path = './snippets/'
init_ram_base_path = './init-rams/'


pnums = sys.argv[1:]

for pnum in pnums:
    pnum = int(pnum)
    asm_path = asm_base_path + asm_names[pnum - 1]
    if init_rams[pnum - 1] != '':
        init_ram_path = init_ram_base_path + init_rams[pnum - 1]
    else:
        init_ram_path = None

    # TODO:     Work on this!

