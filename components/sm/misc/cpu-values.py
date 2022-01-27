#!/usr/bin/env python3

import argparse
from collections import namedtuple
from lxml import etree
import os
import sys


DESCRIPTION = 'SM CPU config generator'
DEFAULT_PACKAGE_NAME = 'CPU_Values.Target'


def write_spec(policy, package_name, f):
    """
    Write CPU config to given file.
    """
    cpuid = policy.xpath("/system/hardware/processor/cpuid")
    count = len(cpuid)

    f.write("private package " + package_name + "\n\n")
    f.write("is\n\n")
    f.write("   CPUID : constant array (Positive range <>) of CPUID_Entry_Type\n")
    f.write("     := (\n");
    for idx, c in enumerate(cpuid, start=1):
        leaf = c.get("leaf")
        subleaf_count = len(c.xpath("/system/hardware/processor/cpuid[@leaf='" + leaf + "']"))
        has_subleaf = "True" if subleaf_count > 1 else "False"
        f.write("         " + str(idx) + " => ("
                + leaf + ", " + has_subleaf + ", "
                + c.get("subleaf") + ", "
                + c.get("eax") + ", " + c.get("ebx") + ", "
                + c.get("ecx") + ", " + c.get("edx") + ")")
        if idx < count:
            f.write(",\n")
        else:
            f.write("\n")
    f.write("        );\n");
    f.write("end " + package_name + ";\n")


def parse_args():
    """
    Returns parsed command line arguments
    """
    arg_parser = argparse.ArgumentParser(description=DESCRIPTION)
    arg_parser.add_argument('--out', type=str,
                            help=('Filename of source file to be generated'))
    arg_parser.add_argument('--src_policy', type=str,
                            help=('Muen XML system policy'))
    arg_parser.add_argument('--package', type=str,
                            default=DEFAULT_PACKAGE_NAME,
                            help=('Name of Ada package'))

    return arg_parser.parse_args()


args = parse_args()
src_policy_path = args.src_policy
out_path = args.out
pkg_name = args.package

if out_path is None:
    sys.exit(("Error: Output file not specified"))

if src_policy_path is None:
    sys.exit("Error: Muen source system policy XML not specified")

    if not os.path.isfile(src_policy_path):
        sys.exit("Error: Muen source system policy XML not found '"
                 + src_policy_path + "'")

out_dir = os.path.dirname(out_path)
if len(out_dir) > 0 and not os.path.isdir(out_dir):
    sys.exit(("Error: Output directory for source file does not "
              + "exist ('" + os.path.dirname(out_dir) + "')"))

print("Reading source system policy from '" + src_policy_path + "'")
xml_parser = etree.XMLParser(remove_blank_text=True)
src_policy = etree.parse(src_policy_path, xml_parser).getroot()

with open(out_path, 'w') as out_file:
    print("Writing Ada package '" + pkg_name + "' file to '" + out_path + "'")
    write_spec(src_policy, pkg_name, out_file)
