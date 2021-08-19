#!/usr/bin/env python3

import argparse
from lxml import etree
import os
import sys


DESCRIPTION = 'Dbgserver subject name mapping generator'
COMPONENT_NAME = 'dbgserver'
DEFAULT_PACKAGE_NAME = 'Dbg.Subject_List'


def extract_subject_names(xml_spec):
    """
    Return names of subjects mapping log channels from given XML system policy.
    """
    subject_names = []
    log_channels = xml_spec.xpath("/system/subjects/subject/component[@ref='"
                                  + COMPONENT_NAME
                                  + "']/map[starts-with(@logical,'log_')]")

    for channel in log_channels:
        phys_name = channel.get("physical")
        subjs = xml_spec.xpath("/system/subjects/subject/component[not(@ref='"
                               + COMPONENT_NAME + "') and map/@physical='"
                               + phys_name + "']/..")
        if len(subjs) == 0:
            subjs = xml_spec.xpath("/system/subjects/subject[channels/writer/"
                                   "@physical='" + phys_name + "']")

        try:
            subject_names.append(subjs[0].get("name"))
        except IndexError:
            sys.exit("Error: Unable to determine subject for channel '"
                     + phys_name + "'")

    return subject_names


def write_spec(subject_names, package_name, f):
    """
    Write mapping of specified subject names to given file.
    """
    subjcount = len(subject_names)
    maxlen = max(len(name) for name in subject_names)
    f.write("private package " + package_name + "\n")
    f.write("is\n\n")
    f.write("   subtype Name_Type is String (1 .. " + str(maxlen) + ");\n\n")
    f.write("   type Names_Array is array (Subject_Buffer_Range) of "
            + "Name_Type;\n\n")
    f.write("   Subject_Names : constant Names_Array :=\n")
    for i, name in enumerate(subject_names):
        if i:
            f.write(",\n      ")
        else:
            f.write("     (",)

        f.write(str(i + 1) + ' => "' + name.ljust(maxlen) + '"')

    f.write(");\n\n")
    f.write("end " + package_name + ";\n")


def parse_args():
    """
    Returned parsed command line arguments
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
    sys.exit(("Error: Muen output file not specified"))

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

names = extract_subject_names(src_policy)

with open(out_path, 'w') as out_file:
    print("Writing Ada package '" + pkg_name + "' file to '" + out_path + "'")
    write_spec(names, pkg_name, out_file)
