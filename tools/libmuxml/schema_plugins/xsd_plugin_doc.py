#!/usr/bin/env python3
"""
 This script extends the given schema definition (xsd-file)
 such that "doc" elements can occur as the first child of every node,
 provided that the node has complex content.

 Copyright (C) 2023 secunet Security Networks AG

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

from lxml import etree
import libmuxsdplugin as libp
import argparse

DESCRIPTION = "Script to extend the given xsd-schema with 'doc'-elements."

# The definition how doc-elements look like
documentationType_definition = """
<dummy_root xmlns:xs="http://www.w3.org/2001/XMLSchema">
 <xs:complexType name="documentationType">
  <xs:annotation>
   <xs:documentation>
    A \\texttt{doc} node contains documentation about its parent node.
    The type of the documentation specifies its intended recipients.
   </xs:documentation>
  </xs:annotation>
  <xs:simpleContent>
   <xs:extension base="xs:string">
    <xs:attribute name="type" type="docTypeType" use="optional"/>
    <xs:attribute name="chapter" type="xs:string" use="optional"/>
    <xs:attribute name="priority" type="xs:nonNegativeInteger" use="optional"/>
   </xs:extension>
  </xs:simpleContent>
 </xs:complexType>
 <xs:simpleType name="docTypeType">
  <xs:annotation>
   <xs:documentation>
    Supported types of documentation.
   </xs:documentation>
  </xs:annotation>
  <xs:restriction base="xs:string">
   <xs:enumeration value="muen-tools"/>
   <xs:enumeration value="dev"/>
  </xs:restriction>
 </xs:simpleType>
</dummy_root>
"""


def documentationType_defined(root: etree._Element):
    nsmap = libp.Ns_Info(root).nsmap
    docT_nodes = root.xpath(
        "./*[local-name()='complexType' and @name='documentationType']",
        namespaces=nsmap,
    )
    docTT_nodes = root.xpath(
        "./*[local-name()='simpleType' and @name='docTypeType']", namespaces=nsmap
    )
    return len(docT_nodes) >= 1 and len(docTT_nodes) >= 1


def doc_in_universal_starting_group(root: etree._Element) -> bool:
    """
    Determine if the universal_starting_group contains a doc-element.
    """
    nsmap = libp.Ns_Info(root).nsmap
    doc_nodes = root.xpath(
        "./*[local-name()='group' "
        + "and @name='universal_starting_group']/"
        + "*[local-name()='sequence']/"
        + "*[local-name()='element' and @name='doc']",
        namespaces=nsmap,
    )
    return len(doc_nodes) >= 1


def insert_doc_in_universal_starting_group(doc_root: etree._Element):
    """
    Inserts an element of type documentationType into the universal
    starting group.
    Assumes that universal_starting_group exists.
    """
    ns = libp.Ns_Info(doc_root)
    seq_node = doc_root.xpath(
        "./*[local-name()='group' "
        + "and @name='universal_starting_group']/"
        + "*[local-name()='sequence']",
        namespaces=ns.nsmap,
    )[0]
    attribs = {
        "qname": ns.ns_prefix + "element",
        "name": "doc",
        "type": "documentationType",
        "minOccurs": "0",
        "maxOccurs": "unbounded",
    }
    new_node = libp.create_element_from_dict(attribs)
    # we always insert at index 0 - documentation should always be first
    seq_node.insert(0, new_node)


def arg_parse():
    arg_parser = argparse.ArgumentParser(description=DESCRIPTION)
    arg_parser.add_argument("input_file", type=str, help="path of input xsd-file")
    arg_parser.add_argument("output_file", type=str, help="path of output xsd-file")
    return arg_parser.parse_args()


def add_documentation():
    """
    This procedure will add the possibility to use <doc> nodes on
    toplevel and as child of any element-node with complex type
    (this includes elements of type "string" or "word64Type").

    Prerequisite: all complex types are defined in the root-node
    ('Garden of Eden'-Style).
    """

    parser = etree.XMLParser(remove_blank_text=True)
    args = arg_parse()
    tree = etree.parse(args.input_file, parser)
    doc_root = tree.getroot()

    # Insert universal_starting_group if it does not exists
    # (wrapper for plugins that are allowed in every complexType).
    if not libp.universal_starting_group_defined(doc_root):
        libp.insert_universal_starting_group(doc_root)

    # Append definition of documentationType
    if not documentationType_defined(doc_root):
        libp.insert_children_from_string(
            root=doc_root, xml_string=documentationType_definition
        )

    # Insert doc in universal_starting_group
    if not doc_in_universal_starting_group(doc_root):
        insert_doc_in_universal_starting_group(doc_root)

    tree.write(
        args.output_file, pretty_print=True, encoding="utf-8", xml_declaration=True
    )


if __name__ == "__main__":
    add_documentation()
