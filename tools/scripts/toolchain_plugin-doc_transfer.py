#!/usr/bin/env python3
"""
Documentation-transfer plugin

This script can be used to extend the muen toolchain.
It transfers <doc ...> nodes (for documentation) from the joined policy
to their respective places in the policy-b and saves the new policy-b as
output.

At the moment the script only looks at some paths within the joined policy.
The functionality can be extended by adding new rules. However, the parsers
capabilities are limited to the cases present in the current rules
(e.g. at most one parametrized attribute per xml-node).
"""

import sys
import copy
import argparse
from lxml import etree


class Graph:
    """A simple class representing typed nodes and edges between them"""

    def __init__(self):
        # nodes:
        # set of tuples: (name, {l,c,s}) ("Library","Component","Subject")
        self.nodes = set()
        # edges: if A depends/instantiates B make an edge from A to B
        self.edges = set()  # tuples of nodes: (from,to)

    def get_peers(self, node):
        """return set of all nodes reachable via one edge"""
        return set([x for x in self.nodes if (node, x) in self.edges])

    def get_reachable_nodes(self, node):
        """return list of all nodes that are reachable from node"""
        reachable = reachable_new = self.get_peers(node)
        while reachable_new != set():
            working_list = set()
            for x in reachable_new:
                working_list = working_list | self.get_peers(x)
            if working_list.issubset(reachable):
                break
            else:
                reachable = reachable | working_list
                reachable_new = set() | working_list  # make sure its a copy

        return reachable


def compile_component_to_subjects_map(root):
    """Create a mapping from component/library names to
    a list of all subject names instantiating or depending
    on that component or library.
    """
    c2s_map = dict()
    # build a dependency graph between subjects, components and libraries
    dep_graph = Graph()
    for comp in root.xpath("components/library"):
        c2s_map[comp.get('name')] = set()
        dep_graph.nodes.add((comp.get('name'), 'l'))
        depends = comp.find("depends")
        if depends is not None:
            for dep in depends.findall("library"):
                dep_graph.edges.add(
                    ((comp.get('name'), 'l'), (dep.get('ref'), 'l')))

    for comp in root.xpath("components/component"):
        c2s_map[comp.get('name')] = set()
        dep_graph.nodes.add((comp.get('name'), 'c'))
        depends = comp.find("depends")
        if depends is not None:
            for dep in depends.findall("library"):
                dep_graph.edges.add(
                    ((comp.get('name'), 'c'), (dep.get('ref'), 'l')))

    for subj in root.xpath("subjects/subject"):
        dep_graph.nodes.add((subj.get('name'), 's'))
        comp = subj.find("component")
        if comp is not None and comp.get("ref"):
            dep_graph.edges.add(
                ((subj.get('name'), 's'), (comp.get('ref'), 'c')))

    # resolve dependencies of libraries
    for subj_node in [x for x in dep_graph.nodes if x[1] == 's']:
        for comp_node in dep_graph.get_reachable_nodes(subj_node):
            c2s_map[comp_node[0]].add(subj_node[0])

    return c2s_map


def do_traversal(current_map):
    """Lambda-function wrapper for do_traversal_func"""
    return lambda node, param_bind, c2s_map, expansion: do_traversal_func(
        current_map=current_map,
        node=node,
        param_bind=param_bind,
        c2s_map=c2s_map,
        expansion=expansion)


def do_traversal_func(current_map, node, param_bind, c2s_map, expansion):
    """Recursively applies the rule specified in current map to node"""
    for child in node.findall("*"):
        for rule in current_map:
            (rule_matches, fresh_param_bindings, action) = apply_rule(
                child, rule, current_map[rule], param_bind)
            if rule_matches:
                action(child, param_bind | fresh_param_bindings,
                       c2s_map, expansion)


def apply_rule(node, key, value, param_bind):
    """returns tuple: (rule_matches, fresh_param_bindings, action)
    rule_matches ... indicates if the first level (until first '/')
                     of the given key matches node
    fresh_param_bindings ... mapping which var-name has been bound to which
                             value
    action ... next action to take

    Note: This parser only implements features needed at the time of writing.
    """

    local_rule = key.split('/')[0]
    remaining_rule = "/".join(key.split('/')[1:])
    # a new, shortened rule is produced from the old rule in case the given
    # rule involed children of node
    new_action = (do_traversal({remaining_rule: value})
                  if remaining_rule != '' else value)

    node_name = local_rule.split("[")[0]
    if node.tag != node_name:
        return (False, dict(), None)

    if "[" not in local_rule:
        return (True, dict(), new_action)

    # get string inside of [@...]
    # e.g. name="$component_name"
    attribs_string = local_rule.split("[@")[1][:-1]

    # get attrib from @attrib="..."
    # e.g. name
    attr_name = attribs_string.split("=")[0]
    if attr_name not in node.attrib:
        return (False, dict(), None)

    # get value of attrib without "
    attr_value = attribs_string.split('="')[1][:-1]
    if attr_value[0] == "$":
        return (True, {attr_value[1:]: node.get(attr_name)}, new_action)
    elif node.get(attr_name) == attr_value:
        return (True, dict(), new_action)
    else:
        return (False, dict(), None)


def add_to_output(target_path):
    """Lambda-function wrapper for add_to_output_func"""
    if target_path[0] != '/':
        print("ERROR: target path must be absolute and start with '/'")
        exit(1)
    return lambda node, param_bind, c2s_map, expansion: \
        add_to_output_func(
            target_path=target_path[1:],
            node=node,
            param_bind=param_bind,
            c2s_map=c2s_map,
            exp_subdict=expansion)


def add_new_leaf(exp_subdict, node):
    """Add a new reference to node in exp_subject"""
    if "__new_leaves" not in exp_subdict:
        exp_subdict["__new_leaves"] = []
    exp_subdict["__new_leaves"].append(node)


def include_new_child(exp_subdict, child_str):
    """Add a new child (non-recursive) to exp_subject if not present"""
    if "__children" not in exp_subdict:
        exp_subdict["__children"] = dict()
    if child_str in exp_subdict["__children"]:
        return
    else:
        exp_subdict["__children"][child_str] = dict()


def get_list_of_substitutions(parametrized_substring, param_bind, c2s_map):
    """The function returns a list of subject names either referencing the
    given component (resolved via param_bind) or simply the resolution
    of $subject_name.
    parametrized_substring must be either
    $Component_To_Subjects($component_name)
    or of the from
    $subject_name
    """

    if parametrized_substring.startswith("$Component_To_Subjects"):
        param_name = parametrized_substring.split(
            '$Component_To_Subjects($')[1].split(')')[0]
        resolve_to_subject = True
    else:
        param_name = parametrized_substring.split('$')[1]
        resolve_to_subject = False

    if param_name not in param_bind:
        print(f"ERROR: Could not resolve parameter {param_name}")
        exit(1)
    else:
        param_value = param_bind[param_name]

    output = []
    if resolve_to_subject:
        if param_value not in c2s_map:
            print(f"ERROR: Could not find mapping for component {param_value}")
            exit(1)
        for value in c2s_map[param_value]:
            output.append(value)
    else:
        output.append(param_value)

    return output


def add_to_output_func(target_path, node, param_bind, c2s_map, exp_subdict):
    """Insert the given node at target_path into exp_subject"""

    # check if we are the end of the recursion
    if target_path == '':
        add_new_leaf(exp_subdict, node)
        return

    local_path = target_path.split('/')[0]
    remaining_path = "/".join(target_path.split('/')[1:])
    if "$" not in local_path:
        include_new_child(exp_subdict=exp_subdict, child_str=local_path)
        add_to_output_func(
            target_path=remaining_path,
            node=node,
            param_bind=param_bind,
            c2s_map=c2s_map,
            exp_subdict=exp_subdict['__children'][local_path])
        return

    parametrized_substring = local_path.split('"')[1]
    substitutions = get_list_of_substitutions(
        parametrized_substring, param_bind, c2s_map)

    for sub in substitutions:
        local_path_subst = local_path.replace(parametrized_substring, sub)
        include_new_child(exp_subdict=exp_subdict, child_str=local_path_subst)
        add_to_output_func(
            target_path=remaining_path,
            node=node,
            param_bind=param_bind,
            c2s_map=c2s_map,
            exp_subdict=exp_subdict['__children'][local_path_subst])


def compile_target_expansion(source_root):
    """Creates a data structure which determines
    what to insert where in the target tree.
    The data stucture is has layout like this:
    {'__children' :
      {'system' :
       {'__new_leaves' : [<Element doc at 0x7fc793f93b80>,
                          <Element doc at 0x7fc793f93500>]
        '__children' :
        {'subjects' :
          {'__children' :
            {'subject[@name="time"]' : ...}
    }}}}}
    """

    # resolve which component is used by which subjects
    c2s_map = compile_component_to_subjects_map(source_root)

    expansion = dict()
    parameter_binding = dict()
    do_traversal(system_map)(
        node=source_root,
        param_bind=parameter_binding,
        c2s_map=c2s_map,
        expansion=expansion)

    return expansion


def simple_xpath_match(node, local_xpath):
    """Determines for a 'local rule' such as 'subject[@name="foo"]'
    if node matches it. It only looks at the node-tag and compares
    at most one attribute-value pair."""
    return apply_rule(node, local_xpath, None, dict())[0]


def apply_target_expansion(subtree_root, sub_expansion):
    """Applies the modifications given by sub_expansion
    to the xml-subtree with rooted at subtree_root
    """

    if '__children' in sub_expansion:
        for child in list(subtree_root):
            for path in sub_expansion['__children']:
                if simple_xpath_match(child, path):
                    apply_target_expansion(
                        child, sub_expansion['__children'][path])

    if '__new_leaves' in sub_expansion:
        for leave in reversed(sub_expansion['__new_leaves']):
            subtree_root.insert(0, copy.deepcopy(leave))
    return


def print_expansion(exp, indent):
    """ DEBUG only: print given expansion structure in a human readable way"""
    if not isinstance(exp, dict):
        print(indent * ' ', exp)
        return

    print(indent * ' ' + '{')
    for key in exp:
        print(indent * ' ' + key + ' :')
        print_expansion(exp[key], indent + 1)
    print(indent * ' ' + '}')

###################################################################
# These dictionaries determine what the program does
# and can easily be modified.
#
# The dictionaries contain rules which nodes from the source tree
# are inserted at which place in the target tree.
# A single rule says: "if <key> matches at the current node, execute <value>".
# For each node, all rules in the current dict are applied one after another.
# Variables are marked with $. They are regarded as wildcards in <key>
# and their value bound in a matching is used in <value>.
#
# do_traversal starts evaluation relative to the matched node using
# the specified dict (and all bound variables).
#
# add_to_output inserts the matched node in the target document.
# (More precisely: it inserts the node in an intermediate data structure
# called expansion, which is later used to make all insertions in the
# target document in one run).
#
# "$Component_To_Subjects($component_name)" is special. It triggers that
# $component_name is replaced by all names of subjects referencing this
# component (the node in question is inserted in all these subjects).
# This replacement includes recursive resolution of dependencies
# between components/libraries.


subj_mem_map = {
    'doc': add_to_output(
        '/system/subjects/subject[@name="$subject_name"]'
        + '/memory/memory[@logical="$subj_mem_logical"]')}

subject_map = {
    'doc': add_to_output('/system/subjects/subject[@name="$subject_name"]'),
    'memory/memory[@logical="$subj_mem_logical"]': do_traversal(subj_mem_map),
    'component/map[@logical="$subj_mem_logical"]': do_traversal(subj_mem_map),
    'channels/reader'
    + '[@logical="$subj_mem_logical"]': do_traversal(subj_mem_map),
    'channels/writer'
    + '[@logical="$subj_mem_logical"]': do_traversal(subj_mem_map)}

comp_mem_map = {
    'doc': add_to_output(
        '/system/subjects/subject'
        + '[@name="$Component_To_Subjects($component_name)"]'
        + '/memory/memory[@logical="$comp_mem_logical"]')}

comp_map = {
    'doc': add_to_output(
        '/system/subjects/subject'
        + '[@name="$Component_To_Subjects($component_name)"]'),
    'requires/memory/memory[@logical="$comp_mem_logical"]':
    do_traversal(comp_mem_map),
    'requires/memory/array/memory[@logical="$comp_mem_logical"]':
    do_traversal(comp_mem_map),
    'requires/channels/reader[@logical="$comp_mem_logical"]':
    do_traversal(comp_mem_map),
    'requires/channels/writer[@logical="$comp_mem_logical"]':
    do_traversal(comp_mem_map),
    'requires/channels/array/reader[@logical="$comp_mem_logical"]':
    do_traversal(comp_mem_map),
    'requires/channels/array/writer[@logical="$comp_mem_logical"]':
    do_traversal(comp_mem_map)}

components_map = {'component[@name="$component_name"]': do_traversal(comp_map),
                  'library[@name="$component_name"]': do_traversal(comp_map)}

subjects_map = {'doc': add_to_output("/system/subjects"),
                'subject[@name="$subject_name"]': do_traversal(subject_map)}

system_map = {'doc': add_to_output('/system'),
              'memory/memory[@name="$mem_name"]/doc':
              add_to_output('/system/memory/memory[@name="$mem_name"]'),
              'components': do_traversal(components_map),
              'subjects': do_traversal(subjects_map)}

###################################################################


def main():
    parser = argparse.ArgumentParser(
        description='Transfer <doc> nodes from policy-joined to policy-b')
    parser.add_argument('--pj', type=str, required=True,
                        help='path to policy_joined.xml')
    parser.add_argument('--pb', type=str, required=True,
                        help='path to policy_b.xml')
    parser.add_argument('--o', type=str, required=True,
                        help='path to output file '
                        + '(policy_b with documentation)')
    args = parser.parse_args()

    policy_joined = etree.ElementTree()
    pj_root = policy_joined.parse(args.pj)

    policy_b = etree.ElementTree()
    pb_root = policy_b.parse(args.pb)

    expansion = compile_target_expansion(source_root=pj_root)

    if '__children' in expansion and 'system' in expansion['__children']:
        apply_target_expansion(subtree_root=pb_root,
                               sub_expansion=expansion['__children']['system'])

    policy_b.write(args.o, xml_declaration=True, pretty_print=True)


if __name__ == '__main__':
    main()
