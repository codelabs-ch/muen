.. _The_Schema_module:

*****************
The Schema module
*****************

XML Grammars
============

There are several steps that applications must go through when they have to use
XML files:

* Make sure the XML file is well-formed.

  This is a basic step where we ensure that XML tags are correctly nested, that
  closing tags have the same names as the matching opening tags, that attribute
  values are quoted,.... This corresponds to a syntactic parser in a compiler.

  This step does not depend on the application domain. One file that is
  well-formed will always be so, no matter in what context you use it.

* Make sure the contents of the XML file is semantically valid.

  Depending on the application domain, we must ensure that the content of the
  file makes sense. This step is highly application dependent, and a file that
  is usable in one application might not be usable in another one.

  This is the phase in which the application needs to check whether a given XML
  file has all its required attributes, whether the children of an XML tag are
  the expected ones, whether the type of the attributes is valid,....

* Use the XML file in the application.

  This is done through the already-described SAX or DOM parsers

The first phase is mandatory, and necessarily enforced by XML/Ada. You will not
be able to access the contents of the XML file if it isn't well-formed.

The second phase is provided by the Schema module in XML/Ada. Although such
constraints can be checked at the application level, with ad hoc code, it is
generally easier to maintain a separate file that describes the valid semantic
contents of the file, that maintain specific code when the semantic changes. It
is also difficult not to forget special cases when doing the validating through
a set of `if` statements in the Ada core.

XML provides two ways to describe additional constraints that a file must
satisfy in order to be considered as valid.

* DTD

  The Document Type Description is the original way to do this. They come
  directly from the ancestor of XML, SGML. All XML parsers must parse the DTD,
  and report events if the user is using SAX. However, not all parsers are able
  to validate the document against a DTD (XML/Ada doesn't).

  Their use tend to greatly diminish. Among their limitation are a limit
  capability to express constraints on the order of tag children, the fact they
  the DTD themselves are written in a separate language totally different from
  XML, and that users must learn as a result.

* XML Schema

  The XML schemas are replacing the DTDs. They are written in XML, and provide
  an extensive capability to describe what the XML document should look like.
  In fact, almost all Ada types can be described in an XML schema, including
  range constraints, arrays, records, type inheritance, abstract types,....

  It is for instance possible to indicate that the value of a preference, in
  our example, must be a string of length 6. Any other length will result in a
  validation error.

XML Schema Syntax
=================

The Schema modules provides subprograms and types to parse an XML schema and
validate an XML document with this schema.

This document does not provide a full documentation on the format of XML
Schemas. This is extensive, has several obscure features, which, although
supported by XML/Ada, are of little use in most pratical uses. We refer the
reader to the first part of the XML Schema specification, which is designed as
a tutorial (`http://www.w3.org/TR/xmlschema-0/
<http://www.w3.org/TR/xmlschema-0/>`_).

The typical extension for a schema file is :file:`.xsd`.

A schema file must be a valid XML file, and thus start with the usual `<?xml
version="1.0" ?>` line. The root node must be named `schema`, and belong to the
namespace (`http://www.w3.org/2001/XMLSchema/
<http://www.w3.org/2001/XMLSchema/>`_). The handling of namespaces is fairly
powerful, but also complex. A given XML document might have nodes belonging to
several namespaces, and thus several schema files might have to be loaded, each
defining one of the namespaces.

.. highlight:: xml

In the following simple example, we will not define our schema for a specific
namespace, and thus no special attribute is needed for the root node.  Thus,
our document will be organized as::

  <?xml version="1.0" ?>
  <xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">
      ... rest of the description goes here ...
  </xsd:schema>

An XML schema does not enforce a specific root node in the XML documents it
validates. However, it must define all the valid elements that can be used in
the XML file. This is done through the `<element>` tag, which takes
one mandatory attribute, the name of the element we are defining.

The contents of the element is then defined in one of two ways:

* Through a `type` attribute.

  Schemas come with a number of predefined simple types. A simple type is
  such that an element of that type accepts no child node, and that its
  contents must satisfy additional constraints (be an integer, a date,
  ...).

  Among the predefined simple type (which are all defined in the namespace
  `http://www.w3.org/2001/XMLSchema/ <http://www.w3.org/2001/XMLSchema/>`_),
  one can find: `string`, `integer`, `byte`, `date`, `time`, `dateTime`,
  `boolean`,...

  If no additional constraint should be enforced on this simple type when
  applied to the element, the type of the element is given through a `type`
  attribute, as in::

     <xsd:element name="tag1" type="xsd:string"  />
     <xsd:element name="tag2" type="xsd:boolean" />
    
  which would accept the following XML files::

     <tag1>Any string is valid here</tag1>

  and::

     <tag2>true</tag2>
    
  but not::

     <tag2>String</tag2>
    
  As will be described later, it is possible to create new types in XML schema,
  which are created with a name. Such new types can also be associated with the
  element through the `type` attribute.

* Through an inline type definition

  If the element must accept child elements, or if a further constraint needs
  to be enforced on the list of valid values, one must create the type. As
  mentioned above, this can be done by creating a type separately and
  referencing it by name, or through an inline type definition.

  The syntax is mostly the same in both cases. Schemas distinguish between the
  notion of simple types (that accept no child element) and complex types (that
  accept child elements, and possibly text value).

  To define a simple type, based on string, but that only allows a limited set
  of value (similar to an Ada enumeration), one would create a restriction of
  the standard string type, as in::

     <xsd:element name="tag3">
      <xsd:simpleType>
        <xsd:restriction base="xsd:string">
          <xsd:enumeration value="value1" />
          <xsd:enumeration value="value2" />
        </xsd:restriction>
      </xsd:simpleType>
     </xsd:element>

  Similarly, we could create an integer type whose valid range of values
  is between 10 and 20, as in::

     <xsd:element name="tag4">
      <xsd:simpleType>
        <xsd:restriction base="xsd:byte">
          <xsd:minInclusive value="10" />
          <xsd:maxInclusive value="20" />
        </xsd:restriction>
      </xsd:simpleType>
     </xsd:element>
    
  Complex types allow elements to have child nodes, as well as attributes. The
  list of valid attributes is created by a set of `<xsd:attribute>` tags, and
  the list of valid child nodes is generally defined either through a
  `<xsd:choice>` or a `<xsd:sequence>` node (although it is possible to
  indicate that any child node is authorized, among other things).

  `<xsd:choice>` indicate the children can appear in any order, whereas
  `<xsd:sequence>` enforces a specific order on children.

  In both cases, extra attributes can be specified to indicate the number of
  times the sequence or choice itself can be repeated, or that each child node
  can appear.

  For instance, we can indicate that `tag5` accepts between 1 and 4 child
  nodes, chosen among `tag6` and `tag7`, but that the latter, if
  present, can only appear once. In addition, tag5 accepts one optional
  attribute. Note that the type of tag6 and tag7 is here specified through a
  `type` attribute, although it could in turn be defined inline::

     <xsd:element name="tag5">
       <xsd:complexType>
         <xsd:choice>
           <xsd:element name="tag6" type="xsd:string"
                        minOccurs="1" maxOccurs="3/>
           <xsd:element name="tag7" type="xsd:string" maxOccurs="1" />
         </xsd:choice>
         <xsd:attribute name="attr" type="xsd:boolean" use="optional" />
       </xsd:complexType>
     </xsd:element>
    
  In the example above, if `tag6` was defined elsewhere in the
  schema, we could use a reference to it, instead of duplicating its
  type definition, as in::

      <xsd:element ref="tag6" />

  If you need an element with no child element (just a string value),
  but that accepts attributes, this also must be defined through a
  complex type, as in::

     <xsd:element name="tag8" />
       <xsd:complexType>
         <xsd:simpleContent>
           <xsd:extension base="xsd:string">
              <xsd:attribute name="attr" type="xsd:boolean" />
           </xsd:extension>
         </xsd:simpleContent>
       </xsd:complexType>
     </xsd:element>

As mentioned before, instead of defining inline types, we could explicitly
declare them, and reference them in the element declaration later on::

   <xsd:simpleType name="string_of_length_10">
     <xsd:restriction base="xsd:string" />
       <xsd:length value="10"/>
     </xsd:restriction>
   </xsd:simpleType>
   <xsd:element name="tag9" type="string_of_length_10" />
  
Connecting XML documents and schemas
====================================

There are several ways that XML/Ada uses to find what schema to use when
validating a file.

* Manually creating the grammar.

  The schema module contains the package `Schema.Validators` which
  allows you to create a grammar by hand. It is very low-level, and it is
  likely that you will never need to use it. It is used internally mostly,
  and when creating the schema which is used to validate schema files
  themselves.

* Explicitly parsing a schema file

  Parsing a schema file can be done through a call to parse for a reader
  derived from `Schema.Schema_Readers.Schema_reader`.  As usual, you
  call `Parse`, and pass it an input source. As output, you get
  access to a grammar, that can then be given to another instance of a
  `Schema.Readers.Validating_Reader`.

  .. highlight:: ada
     :linenothreshold: 3

  This technique will generally be used when you need to validate several
  XML files with the same grammar: you parse the grammar only once, and
  then reuse its instance, instead of reparsing the :file:`.xsd` file every
  time::

    with Schema.Schema_Readers, Schema.Validators, Input_Sources.File;
    use  Schema.Schema_Readers, Schema.Validators, Input_Sources.File;
    with Schema.Schema_Grammar;  use  Schema.Schema_Grammar;

    procedure SchemaExample2 is
       Grammar : XML_Grammar;
       Schema  : Schema_Reader;
       Read    : File_Input;
    begin
         Open ("file.xsd", Read);
         Parse (Schema, Read);
         Close (Read);

         Grammar := Get_Grammar (Schema);
    end SchemaExample2;

  In the example above, the schema file itself is validated against the
  official schema for schema files.

  The resulting grammar object is in fact a collection of parsed schema
  files, each associated with its own namespace. It can be kept as long as
  you need it in your application. Memory will automatically be reclaimed
  when no longer needed.

  Every time you parse an XML file later on, you must associated the
  Grammar with the parser::

     declare
        Read      : File_Input;
        My_Reader : Validating_Reader;
     begin
        Set_Grammar (My_Reader, Grammar);
        Set_Feature (My_Reader, Schema_Validation_Feature, True);
        Open (Xml_File.all, Read);
        Parse (My_Reader, Read);
        Close (Read);
     end;

* Implicitly parsing the schema

  Two special attributes, defined in the Schema standard, can be used to
  indicate, in an XML document itself, that it should be validated with
  a specific schema.

  These attributes are both defined in a special namespace,
  `http://www.w3.org/2001/XMLSchema-instance
  <http://www.w3.org/2001/XMLSchema-instance>`_.

  * `xsi:noNamespaceSchemaLocation`

    The value of this attribute is the name of a file that contains
    the schema to use for elements that are not associated with a
    specific namespace.

  * `xsi:schemaLocation`

    This attribute is a list of strings, alternatively the prefix of
    a namespace and the name of an xsd file to use for that
    namespace. For instance, `"ns1 file1.xsd ns2 file2.xsd"`.

  When it encounters any of these two attributes, XML/Ada will
  automatically parse the corresponding schema files, and use the result
  to validate the file.

Validating documents with SAX
=============================

XML/Ada is quite unique in the category of XML parsers, since it allows the
validation of XML files when you are using an event-based parser with SAX.
Most other XML parsers only work on DOM trees.

Basing the validation on SAX is more efficient, since there is no need to read
the whole XML stream (or even the grammar) in memory before starting the
validation, and errors can be reported immediatly.

It also requires less memory to run, and thus can validate large XML
documents.

It also means that even if you are using SAX, and not DOM, you still have
access to the validation features.

Validating a XML document while parsing it is basically done the same as when
using SAX itself. Instead of inheriting from `Sax.Readers.Reader`, your tagged
type must inherit from `Schema.Readers.Validating_Reader`.

As usual, you can still override the predefined primitive operations like
`Start_Element`, `End_Element`, ...

Note the activation of the `Schema_Validation_Feature` feature, without which
no validation takes place:

.. literalinclude:: schema/schemaexample.adb
   :linenos:

Validating documents with DOM
=============================

This is very similar to using DOM itself, except the base class of your
reader should be `Schema.Dom_Readers.Tree_Reader`. Going back to the
example described in :ref:`Using_DOM`, you would use the following to
validate XML streams before generating the DOM tree.

.. literalinclude:: dom/domschemaexample.adb
   :language: ada
   :linenos:

Unsupported schema elements
===========================

Not all aspects of XML schemas are supported by XML/Ada.
In particular, it does not currently support XPath, so any part of the
schema that is related to XPath expressions (for instance `<xsd:key>`
and `<xsd:unique>`) are not supported currently.

