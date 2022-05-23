#!/usr/bin/env python3

from __future__ import print_function

import sys
import re


if len(sys.argv) == 1:
    print(sys.argv[0] + " <subject_id> [filename]")
    exit(-1)
elif len(sys.argv) == 2:
    f = sys.stdin
else:
    filename = sys.argv[2]
    f = open(filename, 'r', encoding='utf-8', errors='replace')

subject_id = int(sys.argv[1], 0)

if subject_id > 0xffff:
    print("Invalid subject ID %x: must be in range 0 .. 0xffff" % subject_id)
    exit(-1)

p = re.compile('16#' + format(subject_id, 'x').zfill(4) + '#[%#>|]')

is_first_line = True
line = f.readline()

while line:
    m = p.match(line)
    if m:
        if line[8] != '>' and is_first_line is False:
            print('\n', end='')

        print(line.rstrip('\n')[9:].translate('\r\01'), end='')

        sys.stdout.flush()
        if is_first_line is True:
            is_first_line = False

    line = f.readline()

print('\n', end='')
f.close()
exit(0)
