# Technical information about automatic schema extensions

Automatically modifying a schema definition is in general highly non-trivial.
The current implementation does not support the extension of arbitrary schemata.
The document is about the assumptions and considerations made for the current implementation.

## How extended schemata are built

1. Create the XSD-definitions for the core-schemata (from the base-files as usual)
2. Patch the finished definitions with the new scripts, applying the extensions one
after another. Use the naming scheme "coreName" > "coreName_ext" > "coreName_ext" > ...



## Properties extensions must have (to keep it simple and compatible)
Let f be the xsd-patching function.
* f must adhere to https://www.w3.org/TR/2004/REC-xmlschema-1-20041028
* The base-definitions are written in the definition-style "Garden of Eden",
  i.e., all type definitions are children of the "schema"-node and there are  no inline or anonymous definitions
  (see https://www.oracle.com/technical-resources/articles/java/design-patterns.html).
  The result of f must be in the same style.
* Additionally, the base-definitions only use the following constructions.
The following list is to be read as  '<xsd_element_name>' : <list of xsd_elements used as children>:

'schema': ['complexType', 'simpleType', 'group', 'attributeGroup', 'element'],

'complexType': ['annotation', 'attribute', 'sequence', 'complexContent', 'simpleContent', 'attributeGroup', 'choice'],
'sequence': ['element', 'choice'],
'simpleType': ['annotation', 'restriction', 'union'],
'restriction': ['maxInclusive', 'enumeration', 'pattern', 'maxLength', 'length', 'minLength', 'minInclusive'],
'complexContent': ['extension']
'extension': ['group', 'attribute', 'attributeGroup', 'sequence'],
'group': ['choice'],
'choice': ['element'],
'simpleContent': ['extension'],
'attributeGroup': ['attribute', 'attributeGroup'],
'annotation': ['documentation'],
'attribute': ['annotation'],

'maxLength': [],
'length': [],
'minLength': [],
'pattern': [],
'minInclusive': [],
'union': [],
'enumeration': [],
'maxInclusive': [],
'element': [],
'documentation': [],

Key takeaways from the above list:
* no "restriction" inside of complexContent
* "all" is not used
* "any" is not used

 If f uses XSD-elements/constructions not in the above list, all other schema-extension scripts must be checked (they may break, not all what XSD supports is implemented).

* f must be **idempotent**, i.e., for all definitions X: f(f(X)) = f(X)
* f should be **monotone** in the sense that valid XML-data can contain additional tags and attributes but previously legal elements stay legal.
In particular the sets of legal values of attributes should only be expanded.
* f must not introduce complex types with mixed content.
* f must not violate the restrictions that 'amend' needs to work:
 ** There is only one namespace (namespaces are ignored when processing).
 ** There are no nodes with name "import", "include", " redefine", "example", or "redefine".
 ** The attribute "substitutionGroup" is not used.
 ** Within one type, there is no cyclic reference between <group>-elements.
 ** <element>-nodes have a named type attribute inside of their tag (i.e., no inline type-definition or type reference via separate tag happens).
 ** Elements never have the types "any" or "anyType".
 ** Within one type, there must not be two child elements with the same name and different types.

* f should not modify the children of elements which have a text-node child (because that would become very error-prone)
* The semantics of f should avoid dependencies on the order of elements when possible.


## Further considerations for present and future implementation
This section is meant to show a few problems that arise when extending XSD-schemata,
so that future schema-extensions can be designed accordingly.


### Root-only insertion:
Problem:
complexType A has some or no elements (maybe only attributes)
complexType B, extends A with group C
group C has a sequence of elements
complexType D extends B

Where to insert the doc-element if e.g. D is examined first?

Solution: change only A.
>> Hence the rule: If a type is an extension or restriction of another type, do not change it.


### Restructuring
In general the insertion of the universal starting group may need restructuring as the following
example shows:

complexType
  group
    choice
      elm A
      elm B

>> here we need to restructure to add the universal starting group:
complexType
  sequence
    universal_group
    group
      choice
        elm A
        elm B

>> another problematic case:
complexType
    choice
      elm A
      elm B


complexType
  sequence
    universal_g
    choice
      elm A
      elm B


>> hence the rule: never change a group. It might extend an already extended complexType.
>> Guideline: Stick changes as close to the complexType as possible!


### General
Note: What is allowed in complexTypes is rather complicated in general
(see https://www.w3.org/TR/2004/REC-xmlschema-1-20041028/structures.html#element-complexContent).
We do not intend to cover all cases.
