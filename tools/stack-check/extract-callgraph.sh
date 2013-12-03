#!/bin/bash
# $1: object file
# extracts the calls between functions in a simple format:
# caller
# .. callee
if [ -z "$1" ]; then
	echo "usage. $0 ELF-file"
	exit 1
fi
objdump -d $1 | \
egrep "(^[0-9a-f]|callq  |jmp  )" | \
sed -e "s,^[0-9a-f]* <\(.*\)>:$,\1," -e "s,^  .*\(callq\|jmp\)  .*<\(.*\)>$,.. \2," | \
grep -v "+0x[0-9a-f]*$"
