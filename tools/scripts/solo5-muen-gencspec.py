#!/usr/bin/env python

import argparse
import json
import lief
from lief import ELF
from lxml import etree
import math
import os
import shutil
import subprocess
import sys

import _paths
import muutils

MFT_CMD = "solo5-elftool query-manifest"
DEFAULT_RAM_SIZE = "512"
MAX_NAME_LEN = 63

chanSize = 0x100000
chanAddr = 0xe0000000
comp_name = ""


def add_bootparams(xml_spec, bootparams):
    """
    Add bootparams config value to XML spec.
    """
    section = src_spec.xpath("/component/config")
    if len(section) is 0:
        comp = src_spec.xpath("/component")[0]
        config = etree.Element("config")
        comp.insert(0, config)
    else:
        config = section[0]

    if len(bootparams) > 0:
        print("* Setting bootparams to '" + bootparams + "'")

    etree.SubElement(config, "string", name="bootparams", value=bootparams)


def set_rip(xml_spec, binary):
    """
    Set RIP in XML spec to entry point of given ELF binary.
    """
    rip = src_spec.xpath("/component/requires/vcpu/registers/gpr/rip")[0]
    rip.text = muutils.int_to_ada_hex(binary.header.entrypoint)
    print("* Setting RIP to " + rip.text)


def add_elf_memory(xml_spec, binary, filename):
    """
    Add memory regions for given ELF binary with specified name to <requires>
    section of XML spec. Each ELF segment of type LOAD is processed with the
    region name being a concatenation of contained ELF section names, truncated
    to fit the maximum name length after expansion to physical memory regions.
    Returns the virtual end address of the highest binary memory region.
    """
    global comp_name
    # Component name+'|' is prepended to physical names of required resources
    max_len = MAX_NAME_LEN - (len(comp_name) + 1)
    end_addr = 0
    provides = xml_spec.xpath("/component/provides")[0]
    for segment in binary.segments:
        if segment.type == ELF.SEGMENT_TYPES.LOAD:
            sections = segment.sections
            if len(sections) == 0:
                continue

            n = "+".join([section.name for section in sections])
            mem_name = (n[:max_len - 2] + '..') if len(n) > max_len else n
            print("* Adding memory region '" + mem_name + "'")

            w = ELF.SEGMENT_FLAGS.W in segment
            x = ELF.SEGMENT_FLAGS.X in segment

            virtual_addr = segment.virtual_address
            phy_size = segment.physical_size
            # Round up to page size
            phy_size -= phy_size % -4096
            vaddr_str = muutils.int_to_ada_hex(virtual_addr)
            offset_str = muutils.int_to_ada_hex(segment.file_offset)

            mem = etree.Element("memory",
                                logical=mem_name,
                                virtualAddress=vaddr_str,
                                size=muutils.int_to_ada_hex(phy_size),
                                executable=muutils.bool_to_str(x),
                                writable=muutils.bool_to_str(w),
                                type="subject_binary")
            etree.SubElement(mem,
                             "file",
                             filename=filename,
                             offset=offset_str)
            provides.append(mem)

            # Add fill region if virtual_size is larger than physical_size
            mem_size = segment.virtual_size
            mem_size -= mem_size % -4096
            if phy_size < mem_size:
                vaddr = virtual_addr + phy_size
                vaddr_str = muutils.int_to_ada_hex(vaddr)
                size_str = muutils.int_to_ada_hex(mem_size - phy_size)
                print("* Adding memory region '" + mem_name + "|fill'")
                mem = etree.Element("memory",
                                    logical=mem_name + "|fill",
                                    virtualAddress=vaddr_str,
                                    size=size_str,
                                    executable=muutils.bool_to_str(x),
                                    writable=muutils.bool_to_str(w),
                                    type="subject_binary")
                etree.SubElement(mem, "fill", pattern="16#00#")
                provides.append(mem)

            end_addr = segment.virtual_address + mem_size

    return end_addr


def add_ram_memory(xml_spec, binary, binary_end, ram_size_mb):
    """
    Add RAM memory region of given size and set RSP to top of RAM in XML spec.
    """
    print("* Adding RAM region with size " + str(ram_size_mb) + " MB")
    ram_size = ram_size_mb * 2 ** 20
    provides = src_spec.xpath("/component/provides")[0]
    etree.SubElement(provides, "memory",
                     logical="ram",
                     virtualAddress=muutils.int_to_ada_hex(binary_end),
                     size=muutils.int_to_ada_hex(ram_size),
                     executable="false",
                     writable="true",
                     type="subject")

    rsp = src_spec.xpath("/component/requires/vcpu/registers/gpr/rsp")[0]
    rsp.text = muutils.int_to_ada_hex(binary_end + ram_size - 8)
    print("* Setting RSP to " + rsp.text)


def add_channel(name, channels):
    """
    Add channel reader/writer XML elements for Solo5 device with given name.
    """
    global chanAddr, chanSize
    print("* Adding channels for device '" + name + "'")
    channels.append(etree.Element("reader", logical=name + "|in",
                    virtualAddress=muutils.int_to_ada_hex(chanAddr),
                    size=muutils.int_to_ada_hex(chanSize)))
    chanAddr += chanSize
    channels.append(etree.Element("writer", logical=name + "|out",
                    virtualAddress=muutils.int_to_ada_hex(chanAddr),
                    size=muutils.int_to_ada_hex(chanSize)))
    chanAddr += chanSize


def parse_args():
    """
    Returned parsed command line arguments
    """
    arg_parser = argparse.ArgumentParser(description='Solo5 to Muen converter')
    arg_parser.add_argument('elf_binary', type=str,
                            help='Solo5 unikernel ELF binary')
    arg_parser.add_argument('src_xml_spec', type=str,
                            help='Muen component source XML specification')
    arg_parser.add_argument('out_dir', type=str, help='Output directory')
    arg_parser.add_argument('--bootparams', type=str, default="",
                            help=('Solo5 unikernel boot parameters'))
    arg_parser.add_argument('--out_spec', type=str,
                            help=('Filename of processed Muen component XML '
                                  '(default: <out_dir>/cspecs/'
                                  '$component_name.xml)'))
    arg_parser.add_argument('--ram', type=int, default=DEFAULT_RAM_SIZE,
                            help=('Allocate unikernel memory in MB (default: '
                                  + DEFAULT_RAM_SIZE + ' MB'))
    arg_parser.add_argument('--nocopy', dest='copy_bin',
                            action="store_false", default=True,
                            help=('Do not copy unikernel binary to output '
                                  'directory'))

    return arg_parser.parse_args()


args = parse_args()
src_bin_path = args.elf_binary
src_spec_path = args.src_xml_spec
out_dir = args.out_dir
boot_params = args.bootparams
out_spec_path = args.out_spec
copy_binary = args.copy_bin
ram_size_mb = args.ram

if not os.path.isfile(src_bin_path):
    sys.exit("Error: ELF binary not found")

if not os.path.isfile(src_spec_path):
    sys.exit("Error: Source component XML specification not found")

print("Processing unikernel binary '" + src_bin_path + "'")
binary_name = os.path.basename(src_bin_path)
binary = lief.parse(src_bin_path)

print("Reading source component specification from '" + src_spec_path + "'")
src_spec_name = os.path.basename(src_spec_path)
xml_parser = etree.XMLParser(remove_blank_text=True)
src_spec = etree.parse(src_spec_path, xml_parser).getroot()
comp_name = src_spec.attrib['name'].lower()

add_bootparams(src_spec, boot_params)
set_rip(src_spec, binary)
end_address = add_elf_memory(src_spec, binary, binary_name)
add_ram_memory(src_spec, binary, end_address, ram_size_mb)

print("Extracting Solo5 manifest from unikernel binary")
try:
    manifest = subprocess.check_output(MFT_CMD + " " + src_bin_path,
                                       shell=True)
except subprocess.CalledProcessError:
    sys.exit("Error: Unable to extract manifest from unikernel binary")

data = json.loads(manifest)
if not data['type'] == "solo5.manifest":
    sys.exit("Error: JSON file does not contain Solo5 Manifest")

channels = src_spec.xpath("/component/requires/channels")

for dev in data['devices']:
    if dev['type'] == "NET_BASIC":
        if len(channels) == 0:
            req = src_spec.xpath("/component/requires")[0]
            channels = etree.SubElement(req, "channels")
            # Move <devices> to have schema conformity
            devs = src_spec.xpath("/component/requires/devices")
            if len(devs) > 0:
                req.remove(devs[0])
                req.append(devs[0])
        add_channel(dev['name'], channels)

if out_spec_path is None:
    out_spec_path = out_dir + "/"
    if os.path.isdir(out_spec_path + "cspecs"):
        out_spec_path += "cspecs/"

    out_spec_path += comp_name + ".xml"

if not os.path.isdir(os.path.dirname(out_spec_path)):
    sys.exit(("Error: Output directory for component specification does not "
              + "exist ('" + os.path.dirname(out_spec_path) + "')"))
with open(out_spec_path, 'wb') as out_spec:
    print("Writing component specification to '" + out_spec_path + "'")
    out_spec.write(etree.tostring(src_spec, pretty_print=True))

if copy_binary:
    if not os.path.exists(out_dir):
        print("Creating binary output directory '" + out_dir + "'")
        os.makedirs(out_dir)

    out_bin_path = out_dir + "/" + binary_name
    print("Copying unikernel binary to '" + out_bin_path + "'")
    shutil.copy(src_bin_path, out_bin_path)
