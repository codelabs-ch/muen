#!/usr/bin/python

from __future__ import print_function

import sys
import re

# Length of lines that are potentially split by dbgserver
SPLIT_LINE_LENGTH = 67

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

p = re.compile('16#' + format(subject_id, 'x').zfill(4) + '#[%#>|]')

add_newline = False
line = f.readline()

while line:
    m = p.match(line)
    if m:
        if line[8] != '>' and add_newline is True:
            print ('\n', end='')

        print (line.rstrip('\n')[9:].translate(None,'\r\01'), end='')

        if len(line) != SPLIT_LINE_LENGTH:
            print ('\n', end='')
            add_newline = False
        else:
            add_newline = True

        sys.stdout.flush()
    line = f.readline()

if add_newline:
    print ('\n', end='')

f.close()
exit(0)
