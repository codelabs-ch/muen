#!/usr/bin/python

from __future__ import print_function

import sys
import re

if len(sys.argv) == 1:
    print (sys.argv[0] + " <subject_id> [filename]")
    exit(-1)
elif len(sys.argv) == 2:
    f = sys.stdin
else:
    filename = sys.argv[2]
    f = open(filename)

subject_id = int(sys.argv[1], 0)

if subject_id > 0xffff:
    print ("Invalid subject ID %x: must be in range 0 .. 0xffff" % subject_id)
    exit(-1)

p = re.compile(format(subject_id, 'x').zfill(4) + '[>|]')

add_newline = False
line = f.readline()

while line:
    line = line.rstrip('\r\n')
    m = p.match(line)
    if m:
        if line[4] == '|' and add_newline is True:
            print ('\n', end='')

        print (line[5:], end='')
        add_newline = True
    sys.stdout.flush()
    line = f.readline()

print ('\n', end='')
f.close()
exit(0)
