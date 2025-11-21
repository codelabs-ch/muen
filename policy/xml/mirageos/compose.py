#!/usr/bin/env python3

import argparse

from copy import deepcopy
from lxml import etree

import muutils

SUBJECT_NAME = "unikernel"
LINUX_NAME = "nic_linux"
LINUX_BASE_ADDR = 0xE03000000
LINUX_BOOTPARAM = "unikernel_iface"
RESET_VAR_NAME = "resettable"


def add_physical_channels(doc, comp_channels, subject_name):
    """
    Add physical channels for given component channels using specified subject
    name to construct the physical name.
    """
    phys_channels = doc.xpath("/system/channels")[0]
    for comp_chan in comp_channels:
        name = comp_chan.get("logical")
        size = comp_chan.get("size")
        phys_name = subject_name + "_" + name.replace("|", "_")
        print("* Adding physical channel '" + phys_name + "' with size " + size)
        etree.SubElement(phys_channels, "channel", name=phys_name, size=size)


def add_subject_channel_mappings(doc, comp_channels, subject_name):
    """
    Add component channel mappings for given component channels to subject with
    specified name.
    """
    subj_comp = doc.xpath(
        "/system/subjects/subject[@name='" + subject_name + "']/component"
    )[0]
    for comp_chan in comp_channels:
        log_name = comp_chan.get("logical")
        phys_name = subject_name + "_" + log_name.replace("|", "_")
        print("* Adding subject channel mapping for '" + log_name + "'")
        etree.SubElement(subj_comp, "map", logical=log_name, physical=phys_name)


def add_linux_channels(doc, comp_channels, subject_name):
    """
    Add logical channels for given component channels to Linux subject.
    """
    subj_channels = doc.xpath(
        "/system/subjects/subject[@name='" + LINUX_NAME + "']/channels"
    )[0]
    address = LINUX_BASE_ADDR
    for comp_chan in comp_channels:
        # Flip channel role
        chan_type = "reader" if comp_chan.tag == "writer" else "writer"
        log_name = comp_chan.get("logical")
        size = muutils.ada_hex_to_int(comp_chan.get("size"))
        phys_name = subject_name + "_" + log_name.replace("|", "_")
        addr_str = muutils.int_to_ada_hex(address)
        print(
            "* Adding NIC Linux channel "
            + chan_type
            + " '"
            + phys_name
            + "' @ "
            + addr_str
        )
        etree.SubElement(
            subj_channels,
            chan_type,
            logical=log_name,
            physical=phys_name,
            virtualAddress=addr_str,
        )
        address += size


def copy_reset_variable(doc, subject_name):
    """
    Copy component-local "resettable" variable to system config section.
    """
    comp_rst = doc.xpath(
        "/system/components/component[@name='"
        + subject_name
        + "']/config/boolean[@name='"
        + RESET_VAR_NAME
        + "']"
    )[0]
    sys_config = doc.xpath("/system/config")[0]
    etree.SubElement(
        sys_config,
        "boolean",
        name=subject_name + "_" + RESET_VAR_NAME,
        value=comp_rst.get("value"),
    )


def extend_subject_bootparams(doc, subject_name, bootparams):
    """
    Append given boot parameters to bootparams of subject with specified name.
    """
    print(
        "* Extending boot parameters of subject '"
        + subject_name
        + "' with '"
        + bootparams
        + "'"
    )
    subj_params = doc.xpath(
        "/system/subjects/subject[@name='" + subject_name + "']/bootparams"
    )[0]

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
    comp_params = comp_spec.xpath("/component/config/string" + "[@name='bootparams']")
    if len(comp_params) > 0:
        extend_subject_bootparams(doc, subject_name, comp_params[0].get("value"))


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

    extend_subject_bootparams(doc, LINUX_NAME, LINUX_BOOTPARAM + "=" + ifaces)


def add_component(doc, comp_doc):
    """
    Add copy of given component specification to components section of
    specified system policy document.
    """
    components = doc.xpath("/system/components")[0]
    print("* Adding component '" + comp_doc.get("name") + "'")
    components.append(deepcopy(comp_doc))


def parse_args():
    """
    Returned parsed command line arguments
    """
    arg_parser = argparse.ArgumentParser(description="MirageOS/Solo5 system composer")
    arg_parser.add_argument(
        "policy_path", type=str, help="Path of to be created system policy"
    )
    arg_parser.add_argument(
        "--template", type=str, help=("System policy template path")
    )
    arg_parser.add_argument("--unikernel-spec", type=str, help=("Unikernel spec path"))
    return arg_parser.parse_args()


args = parse_args()
parser = etree.XMLParser(remove_blank_text=True)
doc = etree.parse(args.template, parser).getroot()
cspec_doc = etree.parse(args.unikernel_spec, parser).getroot()
comp_channels = cspec_doc.xpath("/component/requires/channels/*")

add_component(doc, cspec_doc)
add_physical_channels(doc, comp_channels, SUBJECT_NAME)
add_subject_channel_mappings(doc, comp_channels, SUBJECT_NAME)
add_linux_channels(doc, comp_channels, SUBJECT_NAME)
add_linux_bootparams(doc, comp_channels)
set_subject_bootparams(doc, cspec_doc, SUBJECT_NAME)
copy_reset_variable(doc, SUBJECT_NAME)

with open(args.policy_path, "wb") as f:
    print("Writing system policy to '" + args.policy_path + "'")
    f.write(etree.tostring(doc, pretty_print=True))
