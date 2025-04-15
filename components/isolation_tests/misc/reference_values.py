#!/usr/bin/env python3

import argparse
import sys
from pathlib import Path

import _paths
from muutils import ada_hex_to_int

from lxml import etree


def write_spec(policy, package_name, f):
    """
    Write reference values to given file.
    """

    # See SK.Constants.XCR0_Supported_Features_Mask
    xcr0_supported_features_mask = 0xE7

    cpuid = policy.xpath("/system/hardware/processor/cpuid[@leaf='16#0000_000d#']")[0]
    f.write(f"private package {package_name}\n")
    f.write("is\n")
    f.write(
        f"   XSAVE_Supported : constant := {ada_hex_to_int(cpuid.get('eax')) & xcr0_supported_features_mask};\n"
    )
    f.write(f"end {package_name};\n")
    f.close()


def parse_args():
    """
    Returns parsed command line arguments
    """
    arg_parser = argparse.ArgumentParser(
        description="Generate ITS reference values from policy"
    )
    arg_parser.add_argument(
        "--out", type=str, help=("Filename of source file to be generated")
    )
    arg_parser.add_argument("--src_policy", type=str, help=("Muen XML system policy"))
    arg_parser.add_argument(
        "--package",
        type=str,
        default="Reference_Values",
        help=("Name of Ada package"),
    )

    return arg_parser.parse_args()


args = parse_args()
pkg_name = args.package

if args.out is None:
    sys.exit("Error: Output file not specified")
out_path = Path(args.out)

if args.src_policy is None:
    sys.exit("Error: Muen source system policy XML not specified")
src_policy_path = Path(args.src_policy)

if not src_policy_path.is_file():
    sys.exit(f"Error: Muen source system policy XML not found '{src_policy_path}'")

out_dir = out_path.parent
out_dir.mkdir(parents=True, exist_ok=True)

print(f"Reading source system policy from '{src_policy_path}'")
xml_parser = etree.XMLParser(remove_blank_text=True)
src_policy = etree.parse(src_policy_path, xml_parser).getroot()

with open(out_path, "w") as out_file:
    print(f"Writing Ada package '{pkg_name}' file to '{out_path}'")
    write_spec(src_policy, pkg_name, out_file)
