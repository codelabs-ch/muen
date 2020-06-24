#!/usr/bin/env python
#
# Encode arbitrary file in CSL image.

import argparse
import os
import sys
import struct

CSL_VERMAGIC = struct.pack('Q', 0x8adc5fa2448cb65e)


def parse_args():
    """
    Returned parsed command line arguments
    """
    arg_parser = argparse.ArgumentParser(description='CSL file prepender')
    arg_parser.add_argument('img_src', type=str,
                            help='CSL image source')
    arg_parser.add_argument('img_dst', type=str,
                            help='CSL image destination')
    arg_parser.add_argument('cmd_id', type=int, help='CSL cmd ID to use')
    arg_parser.add_argument('file', type=str, help='File to encode')

    return arg_parser.parse_args()


args = parse_args()
img_src_fname = args.img_src
img_dst_fname = args.img_dst
cmd_id = args.cmd_id
file_to_encode = args.file

if not os.path.isfile(img_src_fname):
    sys.exit("Error: CSL source '" + img_src_fname + "' not found")
if not os.path.isfile(file_to_encode):
    sys.exit("Error: file '" + file_to_encode + "' not found")
if not 60000 <= cmd_id <= 65535:
    sys.exit("Error: CMD ID " + str(cmd_id) + " not in vendor specific range")

with open(img_src_fname, 'rb') as img_src:
    magic = img_src.read(8)
    if magic != CSL_VERMAGIC:
        img_src.close()
        sys.exit("Error: source '" + img_src_fname + "' is not a CSL image")

    img_src.seek(8)
    with open(img_dst_fname, 'wb') as img_dst:

        fsize = os.stat(file_to_encode).st_size
        print("Encoding file '" + file_to_encode + "' of size " + str(fsize)
              + " byte(s) as CSL CMD ID " + str(cmd_id))

        img_dst.write(CSL_VERMAGIC)
        img_dst.write(struct.pack('Q', cmd_id))
        img_dst.write(struct.pack('Q', fsize))

        with open(file_to_encode, 'rb') as in_file:
            img_dst.write(in_file.read())
            in_file.close()
            img_dst.write(img_src.read())
            img_src.close()
            img_dst.close()
