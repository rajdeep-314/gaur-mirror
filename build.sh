#!/usr/bin/env bash

exec_dir=./executables
build_dir=./_build/default

assembler_path=${build_dir}/assembler/bin/main.exe
disassembler_path=${build_dir}/disassembler/bin/main.exe
cpusim_path=${build_dir}/cpu-simulator/bin/main.exe
raminit_path=${build_dir}/ram-init/bin/main.exe
hdlsim_path=${build_dir}/hdl-simulator/bin/main.exe

mkdir -p ${exec_dir}

echo "Buliding executables..."
dune clean &> /dev/null
dune build &> /dev/null

if [ -f ${assembler_path} ]; then
    echo "Assembler        :    Build successful."
else
    echo "Assembler        :    Build failed."
    exit 1
fi
if [ -f ${disassembler_path} ]; then
    echo "Disassembler     :    Build successful."
else
    echo "Disassembler     :    Build failed."
    exit 1
fi
if [ -f ${cpusim_path} ]; then
    echo "CPU simulator    :    Build successful."
else
    echo "CPU simulator    :    Build failed."
    exit 2
fi
if [ -f ${raminit_path} ]; then
    echo "RAM Init         :    Build successful."
else
    echo "RAM Init         :    Build failed."
    exit 3
fi
if [ -f ${hdlsim_path} ]; then
    echo "HDL simulator    :    Build successful."
else
    echo "HDL simulator    :    Build failed."
    exit 4
fi
echo "Building successful. Placing executables in ${exec_dir}"

mv ${assembler_path} ${exec_dir}/assembler
mv ${disassembler_path} ${exec_dir}/dis
mv ${cpusim_path}    ${exec_dir}/cpusim
mv ${raminit_path}   ${exec_dir}/ram-init
mv ${hdlsim_path}    ${exec_dir}/hdl-simulator

echo "Script executed."

