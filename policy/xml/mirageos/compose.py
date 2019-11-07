#!/usr/bin/env python

import sys
from copy import deepcopy
from lxml import etree
import math

POLICY_TEMPLATE = "mirage-solo5-net.xml"
COMPONENT_SPEC = "component_unikernel.xml"
SUBJECT_NAME = "unikernel"
LINUX_NAME = "nic_linux"
LINUX_BASE_ADDR = 0xe03000000
LINUX_BOOTPARAM = "unikernel_iface"
OUT_SPEC_PATH = "../" + POLICY_TEMPLATE


def hex_ada(value):
    """
    Convert given Ada hex string to numeric value:
    16#0123_0000# -> 0x1230000
    """
    if value == "":
        return ""
    else:
        val = value.replace('_', '').rstrip("#").lstrip("16#")
        return int(val, 16)


def ada_hex(value):
    """
    Convert given numeric value to Ada hex string:
    0x1230000 -> 16#0123_0000#
    """
    if value == "":
        return ""
    else:
        val = hex(value).rstrip("L").lstrip("0x")
        val = val.rjust(int(math.ceil(len(val) / 4.0) * 4), '0')
        hexvalue = "16#"
        for index in range(0, len(val)):
            if ((index % 4) is 0) and (index is not 0):
                hexvalue += "_"

            hexvalue += val[index]

        return hexvalue + "#"


def add_physical_channels(doc, comp_channels, subject_name):
    """
    Add physical channels for given component channels using specified subject
    name to construct the physical name.
    """
    phys_channels = doc.xpath("/system/channels")[0]
    for comp_chan in comp_channels:
        name = comp_chan.get("logical")
        size = comp_chan.get("size")
        phys_name = subject_name + "_" + name.replace('|', '_')
        print("* Adding physical channel '" + phys_name + "' with size "
              + size)
        etree.SubElement(phys_channels, "channel", name=phys_name, size=size)


def add_subject_channel_mappings(doc, comp_channels, subject_name):
    """
    Add component channel mappings for given component channels to subject with
    specified name.
    """
    subj_comp = doc.xpath("/system/subjects/subject[@name='" + subject_name
                          + "']/component")[0]
    for comp_chan in comp_channels:
        log_name = comp_chan.get("logical")
        phys_name = subject_name + "_" + log_name.replace('|', '_')
        print("* Adding subject channel mapping for '" + log_name + "'")
        etree.SubElement(subj_comp, "map", logical=log_name,
                         physical=phys_name)


def add_linux_channels(doc, comp_channels, subject_name):
    """
    Add logcal channels for given component channels to Linux subject.
    """
    subj_channels = doc.xpath("/system/subjects/subject[@name='" + LINUX_NAME
                              + "']/channels")[0]
    address = LINUX_BASE_ADDR
    for comp_chan in comp_channels:
        # Flip channel role
        chan_type = "reader" if comp_chan.tag == "writer" else "writer"
        log_name = comp_chan.get("logical")
        size = hex_ada(comp_chan.get("size"))
        phys_name = subject_name + "_" + log_name.replace('|', '_')
        print("* Adding NIC Linux channel " + chan_type + " '" + phys_name
              + "' @ " + ada_hex(address))
        etree.SubElement(subj_channels, chan_type, logical=log_name,
                         physical=phys_name, virtualAddress=ada_hex(address))
        address += size


def extend_subject_bootparams(doc, subject_name, bootparams):
    """
    Append given boot parameters to bootparams of subject with specified name.
    """
    print("* Extending boot parameters of subject '" + subject_name
          + "' with '" + bootparams + "'")
    subj_params = doc.xpath("/system/subjects/subject[@name='"
                            + subject_name + "']/bootparams")[0]

    if subj_params.text is not None and len(subj_params.text) > 0:
        params = subj_params.text + " " + bootparams
    else:
        params = bootparams

    subj_params.text = params


def set_subject_bootparams(doc, comp_spec, subject_name):
    """
    Set subject boot parameters to those of the component config variable named
    'bootparams'. This can be dropped once component specifications can
    directly set bootparams.
    """
    comp_params = comp_spec.xpath("/component/config/string"
                                  + "[@name='bootparams']")
    if len(comp_params) > 0:
        extend_subject_bootparams(doc, subject_name,
                                  comp_params[0].get("value"))


def add_linux_bootparams(doc, comp_channels):
    """
    Add 'unikernel_iface' bootparameter to Linux subject.
    """
    ifaces = ""
    for comp_chan in comp_channels:
        if comp_chan.tag == "reader":
            log_name = comp_chan.get("logical").rstrip("|in")
            if len(ifaces) > 0:
                ifaces += ","
            ifaces += log_name

    extend_subject_bootparams(doc, LINUX_NAME,
                              LINUX_BOOTPARAM + "=" + ifaces)


def add_component(doc, comp_doc):
    """
    Add copy of given component specification to components section of
    specified system policy document.
    """
    components = doc.xpath("/system/components")[0]
    print("* Adding component '" + comp_doc.get("name") + "'")
    components.append(deepcopy(comp_doc))


parser = etree.XMLParser(remove_blank_text=True)
doc = etree.parse(POLICY_TEMPLATE, parser).getroot()
cspec_doc = etree.parse(COMPONENT_SPEC, parser).getroot()
comp_channels = cspec_doc.xpath("/component/requires/channels/*")

print("Creating system policy '" + OUT_SPEC_PATH + "'")

add_component(doc, cspec_doc)
add_physical_channels(doc, comp_channels, SUBJECT_NAME)
add_subject_channel_mappings(doc, comp_channels, SUBJECT_NAME)
add_linux_channels(doc, comp_channels, SUBJECT_NAME)
add_linux_bootparams(doc, comp_channels)
set_subject_bootparams(doc, cspec_doc, SUBJECT_NAME)

with open(OUT_SPEC_PATH, 'wb') as f:
    print("Writing system policy to '" + OUT_SPEC_PATH + "'")
    f.write(etree.tostring(doc, pretty_print=True))
