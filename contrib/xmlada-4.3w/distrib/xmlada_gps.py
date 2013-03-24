## This file provides the integration of XML/Ada into GPS

import GPS

GPS.parse_xml ("""
   <doc_path>share/doc/xmlada</doc_path>
   <submenu before="About">
      <title>/Help/XML Ada</title>
   </submenu>
   <documentation_file>
      <name>index.html</name>
      <descr>XML/Ada User's Guide</descr>
      <category>XMLAda</category>
      <menu>/Help/XML Ada/XML Ada User's Guide</menu>
   </documentation_file>
""")
