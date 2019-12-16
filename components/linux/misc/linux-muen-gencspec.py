#!/usr/bin/env python

import argparse
from lxml import etree
import os
import sys

import _paths
import muutils

DESCRIPTION = 'Linux XML component spec generator'
LINUX_VIRTUAL_ADDRESS = "16#0040_0000#"


def add_provides_memory(xml_spec, name, region_type, address, filename, size,
                        executable, writable):
    """
    Add file-backed memory region with given name and size to <provides>
    section of XML spec. The size is rounded up to the next 4K.
    """
    provides = xml_spec.xpath("/component/provides")[0]
    mem_size = size
    mem_size -= mem_size % -4096
    print("* Adding '" + name + "' region with size " + str(mem_size)
          + " bytes, address " + address + ", type " + region_type)
    mem = etree.Element("memory",
                        logical=name,
                        virtualAddress=address,
                        size=muutils.int_to_ada_hex(mem_size),
                        executable=executable,
                        writable=writable,
                        type=region_type)
    etree.SubElement(mem,
                     "file",
                     filename=filename,
                     offset="none")
    provides.append(mem)


def parse_args():
    """
    Returned parsed command line arguments
    """
    arg_parser = argparse.ArgumentParser(description=DESCRIPTION)
    arg_parser.add_argument('kernel_binary', type=str,
                            help='Linux kernel binary')
    arg_parser.add_argument('src_xml_spec', type=str,
                            help='Muen component source XML specification')
    arg_parser.add_argument('--out_spec', type=str,
                            help=('Filename of generated Muen component XML'))

    return arg_parser.parse_args()


args = parse_args()
src_bin_path = args.kernel_binary
src_spec_path = args.src_xml_spec
out_spec_path = args.out_spec

if not os.path.isfile(src_bin_path):
    sys.exit("Error: Linux kernel binary not found '" + src_bin_path + "'")

if not os.path.isfile(src_spec_path):
    sys.exit("Error: Source component XML specification not found")

if out_spec_path is None:
    sys.exit(("Error: Muen output component XML specification not specified"))

out_spec_dir = os.path.dirname(out_spec_path)
if len(out_spec_dir) > 0 and not os.path.isdir(out_spec_dir):
    sys.exit(("Error: Output directory for component specification does not "
              + "exist ('" + os.path.dirname(out_spec_dir) + "')"))

print("Reading source component specification from '" + src_spec_path + "'")
src_spec_name = os.path.basename(src_spec_path)
xml_parser = etree.XMLParser(remove_blank_text=True)
src_spec = etree.parse(src_spec_path, xml_parser).getroot()

print("Processing Linux binary '" + src_bin_path + "'")
binary_name = os.path.basename(src_bin_path)
binary_size = os.path.getsize(src_bin_path)
add_provides_memory(src_spec,
                    "binary",
                    "subject_binary",
                    LINUX_VIRTUAL_ADDRESS,
                    binary_name,
                    binary_size,
                    "true",
                    "true")

with open(out_spec_path, 'wb') as out_spec:
    print("Writing component specification to '" + out_spec_path + "'")
    out_spec.write(etree.tostring(src_spec, pretty_print=True))
