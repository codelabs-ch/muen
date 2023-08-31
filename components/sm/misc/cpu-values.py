#!/usr/bin/env python3

import argparse
from lxml import etree
import os
import sys


DESCRIPTION = 'SM CPU config generator'
DEFAULT_PACKAGE_NAME = 'CPU_Values.Target'


def write_spec(policy, package_name, f):
    """
    Write CPU config to given file.
    """
    f.write("package " + package_name + "\n")
    f.write("is\n\n")

    cpuid = policy.xpath("/system/hardware/processor/cpuid")

    # CPUID leafs
    for c in cpuid:
        leaf = c.get("leaf")
        subleaf = c.get("subleaf")
        f.write("   CPUID_" + leaf[3:-1] + "_" + subleaf[3:-1]
                + " : constant CPUID_Values_Type\n     := ("
                + c.get("eax") + ", " + c.get("ebx") + ", "
                + c.get("ecx") + ", " + c.get("edx")
                + ");\n")
    f.write("\n")

    # XSAVE feature bit to CPUID value entries map
    d_leafs = policy.xpath("/system/hardware/processor/cpuid[@leaf='16#0000_000d#']")
    f.write("   XSAVE_Feature_Values : constant array (XSAVE_Feature_Pos)"
            " of CPUID_Values_Type\n     := (\n")
    for d in d_leafs:
        subleaf = d.get("subleaf")
        if int(subleaf[3:-1], 16) < 2:
            continue
        f.write("         " + subleaf + " => ("
                + d.get("eax") + ", " + d.get("ebx") + ", "
                + d.get("ecx") + ", " + d.get("edx")
                + "),\n")
    f.write("         others => Null_CPUID_Values\n        );\n\n")

    # MSRs
    msr = policy.xpath("/system/hardware/processor/msr")

    for m in msr:
        f.write("   MSR_" + m.get("name") + " : constant MSR_Entry_Type\n"
                "      := ("
                + m.get("address") + ", " + m.get("regval") + ");\n")

    f.write("\nend " + package_name + ";\n")
    f.close()


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
