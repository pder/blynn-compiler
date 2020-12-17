#!/usr/bin/env bash

set -eux

M2-Planet --architecture x86 \
	-f functions/file.c \
	-f functions/exit.c \
	-f functions/malloc.c \
	-f functions/calloc.c \
	-f functions/file_print.c \
	-f functions/in_set.c \
	-f functions/numerate_number.c \
	-f functions/match.c \
	-f functions/require.c \
	-f generated/marginally.c \
	--debug \
	-o bin/marginally.M1

blood-elf -f bin/marginally.M1 --entry _start -o bin/marginally-footer.M1

M1 -f test/common_x86/x86_defs.M1 \
	-f test/common_x86/libc-core.M1 \
	-f bin/marginally.M1 \
	-f bin/marginally-footer.M1 \
	--LittleEndian \
	--architecture x86 \
	-o bin/marginally.hex2

hex2 -f test/common_x86/ELF-i386-debug.hex2 \
	-f bin/marginally.hex2 \
	--LittleEndian \
	--architecture x86 \
	--BaseAddress 0x8048000 \
	-o bin/marginally --exec_enable
