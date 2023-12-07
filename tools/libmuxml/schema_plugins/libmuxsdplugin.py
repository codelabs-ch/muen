"""
 This is libmuxsdplugin, a library to insert small changes into xml-schema
 definitions (xsd-files).

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

# definition of the group that is inserted everywhere
# This wrapper can be used to add more elements "everywhere".
from lxml import etree
universal_starting_group_definition = """
<dummy_root xmlns:xs="http://www.w3.org/2001/XMLSchema">
 <xs:group name="universal_starting_group">
   <xs:annotation>
     <xs:documentation>
       This group holds content that can appear at the beginning
       of every complex type.
     </xs:documentation>
   </xs:annotation>
   <xs:sequence/>
 </xs:group>
</dummy_root>
"""


class Ns_Info:
    def __init__(self, node: etree._Element):
        if node.prefix is None:
            self.ns_short = ""
            self.ns_url = ""
            self.nsmap = {}
            self.ns_prefix = ""
        else:
            self.ns_short = node.prefix
            self.ns_url = node.nsmap[node.prefix]
            self.nsmap = node.nsmap
            self.ns_prefix = f"{{{self.ns_url}}}"


def localname(node: etree._Element) -> str:
    """
    Return the localname, i.e., if tag is "xs:foo" with xml:xs="my_url"
    it returns "foo"  (node.tag will be "{my_url}foo").
    """
    if node.prefix is None:
        return node.tag
    else:
        return "}".join(node.tag.split("}")[1:])


def node_to_string(node: etree._Element) -> str:
    """
    Returns a string representation of node
    """
    attrib_list = [key + '="' + node.attrib[key] + '"' for key in node.attrib]
    # sorting is essential because this function is used to compare nodes
    attrib_list.sort()
    out = "<"
    if node.prefix is not None:
        out += node.prefix + ":"
    out += localname(node)
    if len(attrib_list) > 0:
        out += " "
    out += " ".join(attrib_list)
    out += ">"
    return out


def node_ancestors_string(node: etree._Element) -> str:
    """
    Returns the logical location of node as string, including attributes.
    Example with node <a/>:
    <foo name="1"><bar1/><bar2 id="1", ix="2"><a/></bar></foo>
    returns
     <foo name="1"><bar2 id="1", ix="2"><a>
    """
    out = node_to_string(node)
    for curr_node in node.iterancestors():
        out = node_to_string(curr_node) + out
    return out


def get_defining_node(node: etree._Element, def_name: str) -> etree._Element:
    """
    Given a name of a complexType, simpleType or group definition (as string),
    return the node which defines the type/group with this name.
    The search is limited to children of node
    """
    root = node.getroottree().getroot()
    target_prefix = Ns_Info(node).ns_short
    target_tags = ['complexType', 'simpleType', 'group']

    # Build xpath
    target_xpath = ""
    for t in target_tags:
        target_xpath += f"./{target_prefix}:{t}[@name='{def_name}']"
        if t != target_tags[-1]:
            target_xpath += " | "

    results = root.xpath(target_xpath)
    if len(results) == 1:
        return results[0]
    else:
        raise Error("Could not find unique defining node for type/group "
                    + f"with name '{def_name}'")


def create_element_from_dict(attribs: dict[str, str]) -> etree._Element:
    """
    Returns an element node created from "qname" entry of attribs
    and with all other key-value pairs of attribs as attributes.
    """
    new_elem = etree.Element(attribs["qname"])
    for k in attribs:
        if k != "qname":
            new_elem.set(k, attribs[k])
    return new_elem


def insert_universal_starting_group(doc_root: etree._Element):
    """
    Function to insert a reference to a group with name
    "universal_starting_group" as the first element in all complex types.
    In places where a reference exists already, no new entry is created.

    The strategy of the insertion is "as early as possible", i.e.,
    if type B is derived from type A, then type A is changed but not type B
    (simply by not changing types which are derived).
    The function does not support all possible xsd-constructions.
    In particular a complexTypes derived via 'restriction' are not allowed.
    """

    for child_node in doc_root.iterchildren():
        if localname(child_node) == "complexType":
            insert_usg_in_complexType(node=child_node)

    insert_children_from_string(root=doc_root,
                                xml_string=universal_starting_group_definition)


def insert_usg_in_complexType(node: etree._Element):
    """
    Function to insert a reference to a group with name
    "universal_starting_group" as the first element of the given complexType
    node.
    """

    for child in node.iterchildren():
        name = localname(child)
        prefix = Ns_Info(child).ns_prefix
        # If a complexType contains simpleContent the (only) child is
        # always a text-node. Hence, no other elements can show up.
        if name == "simpleContent":
            return
        # complexContent may only contain restrictions or extensions.
        #  In case of extensions the base class will be modified.
        #  In case of restrictions elements may be taken away.
        #    This case is not supported!
        elif name == "complexContent":
            if len(child.xpath("./*[local-name()='restriction']")) > 0:
                raise Error("Found unsupported element in complexType: "
                            + "'restriction'")
            return
        elif name in ["annotation"]:
            continue
        elif name in ["choice", "group"]:
            # Within 'choice' we lack the freedom to add additional elements.
            # Hence we have to wrap it into a sequence.
            # The same may or may not hold in a 'group'.
            child.addprevious(etree.Element(f"{prefix}sequence"))
            sequence_sibling = child.getprevious()
            # 'append' in lxml moves the child
            sequence_sibling.append(child)
            insert_usg_in_sequence(sequence_sibling)
            return
        elif name == "sequence":
            insert_usg_in_sequence(child)
            return
        elif name in ["attribute", "attributeGroup"]:
            # Declarations regarding attributes are only allowed at the end.
            # Hence, we have to insert before the current node.
            child.addprevious(etree.Element(f"{prefix}sequence"))
            sequence_sibling = child.getprevious()
            insert_usg_in_sequence(sequence_sibling)
            return
        else:
            raise (f"Found unsupported tag in complexType: {name}\n"
                   + "Location of finding: " + node_ancestors_string(child))

    # if the type did not contain any children we insert a sequence
    node.append(etree.Element(f"{prefix}sequence"))
    insert_usg_in_sequence(node[-1])


def insert_usg_in_sequence(node: etree._Element):
    """
    Function to insert a reference to a group with name
    "universal_starting_group" as the first element of the give
    """
    prefix = Ns_Info(node).ns_prefix
    # Check if the requested node has been inserted already.
    node_to_insert_info = {"qname": f"{prefix}group",
                           "ref": "universal_starting_group"}
    new_element = create_element_from_dict(node_to_insert_info)
    if len(node) == 0:
        node.append(new_element)
    elif node_to_string(node[0]) != node_to_string(new_element):
        node.insert(0, new_element)
    else:
        return


def universal_starting_group_defined(root: etree._Element) -> bool:
    """
    Return True if and only if the universal_starting_group is defined as a
    child of root.
    """
    matches = root.xpath(
        "./*[local-name()='group' and @name='universal_starting_group']",
        namespaces=Ns_Info(root).nsmap)
    return len(matches) >= 1


def insert_children_from_string(root: etree._Element,
                                xml_string: str,
                                position: str = None):
    """
    Interprets xml_string as XML tree T (must be valid XML),
    and inserts all children of the root of T as children of 'root'.
    The insertion position can be configured via 'position'.
    Valid values are "top" and "bottom"
    """
    parser = etree.XMLParser(remove_blank_text=True)
    new_doc = etree.fromstring(xml_string, parser)
    if position is None or position == 'top':
        index = 0
    elif position == 'bottom':
        index = len(root)
    else:
        raise Error(f"Unknown value for parameter 'position': {positiion}")

    for child in new_doc.iterchildren():
        root.insert(index, child)
        index += 1
