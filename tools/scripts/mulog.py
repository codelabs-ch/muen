#!/usr/bin/env python3

import os
import re
import sys

formatted = []


# append line to parent with given log prefix
def append(prefix, line):
    for idx, l in reversed(list(enumerate(formatted))):
        if l[0:9] == prefix:
            formatted[idx] = formatted[idx] + line
            return
    raise Exception("Parent line '" + prefix + "' not found")


filename = ""

if len(sys.argv) != 2:
    print(sys.argv[0] + " <filename>")
    exit(-1)
else:
    filename = sys.argv[1]
    if not os.path.exists(filename):
        print("Specified file '" + filename + "' does not exist")
        exit(-1)

f = open(filename, 'r', encoding='utf-8', errors='replace')
lines = [line.rstrip('\r\n') for line in f]
f.close()

p = re.compile('16#[0-9a-fA-F]{4}#>')

for line in lines:
    m = p.match(line)
    if m:
        parentPrefix = m.group().replace('>', '|')
        append(parentPrefix, line[9:])
    elif line[0:3] != "---":
        formatted.append(line)

for line in formatted:
    print(line)
