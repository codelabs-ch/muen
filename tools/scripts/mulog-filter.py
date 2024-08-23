#!/usr/bin/env python3

from __future__ import print_function

import sys
import re
import os.path


if len(sys.argv) == 1:
    print(sys.argv[0] + " <channel id> [filename]")
    exit(-1)
elif len(sys.argv) == 2:
    f = sys.stdin
else:
    filename = sys.argv[2]
    if not os.path.exists(filename):
        print("Specified file '" + filename + "' does not exist")
        exit(-1)

    f = open(filename, "r", encoding="utf-8", errors="replace")

id = int(sys.argv[1], 0)

if id > 0xFFFF:
    print("Invalid ID %x: must be in range 0 .. 0xffff" % id)
    exit(-1)

p = re.compile("16#" + format(id, "x").zfill(4) + "#[%#>|]")

is_first_line = True
line = f.readline()

while line:
    m = p.match(line)
    if m:
        if line[8] != ">" and is_first_line is False:
            print("\n", end="")

        print(line.rstrip("\n")[9:].translate("\r\01"), end="")

        sys.stdout.flush()
        if is_first_line is True:
            is_first_line = False

    line = f.readline()

print("\n", end="")
f.close()
exit(0)
