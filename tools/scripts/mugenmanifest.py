#!/usr/bin/env python3

import sys
from lxml import etree

import _paths
import muutils

if len(sys.argv) != 3:
    print(sys.argv[0] + ' <policy_b> <manifest>')
    sys.exit(1)

parser = etree.XMLParser(remove_blank_text=True)
doc = etree.parse(sys.argv[1], parser).getroot()
nodes = doc.xpath('/system/memory/memory[file|fill]')
nodes = sorted(nodes, key=lambda c:
               muutils.ada_hex_to_int(c.get('physicalAddress')))


with open(sys.argv[2], 'w') as f:
    f.write(
        '[Name;PhysicalAddress;MemorySize;Offset;ContentSize;Usage;Type;'
        'Content]\n')

    for node in nodes:
        name = node.get('name')
        addr = node.get('physicalAddress')
        size = node.get('size')
        offset = '16#0000#'
        usage = '100%'

        content_node = node.xpath('file|fill')[0]
        is_file = content_node.tag == 'file'

        if is_file:
            f_offset = content_node.get('offset')
            if f_offset != 'none':
                offset = f_offset

            t = node.get('type')

            content_size = content_node.get('size')
            if not content_size:
                sys.exit('Error: No size attribute in file node, '
                         'use mucfgmemhashes to add one')

            offset_nr = muutils.ada_hex_to_int(offset)
            size_nr = muutils.ada_hex_to_int(size)
            content_size_nr = muutils.ada_hex_to_int(content_size) - offset_nr
            usage = str(((content_size_nr * 100) +
                         (size_nr - 1)) / size_nr) + '%'

            content_size = muutils.int_to_ada_hex(content_size_nr)

            content = content_node.get('filename')
        else:
            t = 'fill_pattern'
            content = content_node.get('pattern')
            content_size = size

        f.write(name + ';' + addr + ';' + size + ';' + offset + ';' +
                content_size + ';' + usage + ';' + t + ';' + content + '\n')
