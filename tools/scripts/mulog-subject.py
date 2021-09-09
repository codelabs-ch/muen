#!/usr/bin/env python3

from __future__ import print_function

import sys
import re


def not_split(line_length):
    """
    Returns True if the line with given length was not split into multiple
    lines by the dbgserver.
    """
    # Length of lines that are potentially split by dbgserver
    SPLIT_LINE_LENGTH = 67
    SPLIT_BUFFER_LENGTH = 56

    if line_length > SPLIT_LINE_LENGTH:
        return (line_length - SPLIT_LINE_LENGTH) % 56 != 0
    else:
        return line_length != SPLIT_LINE_LENGTH


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

add_newline = False
line = f.readline()

while line:
    m = p.match(line)
    if m:
        if line[8] != '>' and add_newline is True:
            print('\n', end='')

        print(line.rstrip('\n')[9:].translate('\r\01'), end='')

        if not_split(len(line)):
            print('\n', end='')
            add_newline = False
        else:
            add_newline = True

        sys.stdout.flush()
    line = f.readline()

if add_newline:
    print('\n', end='')

f.close()
exit(0)
