.. _The_DOM_module:

**************
The DOM module
**************

DOM is another standard associated with XML, in which the XML stream is
represented as a tree in memory. This tree can be manipulated at will, to add
new nodes, remove existing nodes, change attributes,...

Since it contains the whole XML information, it can then in turn be dump to a
stream.

As an example, most modern web browsers provide a DOM interface to the document
currently loaded in the browser. Using javascript, one can thus modify
dynamically the document. The calls to do so are similar to the ones provided
by XML/Ada for manipulating a DOM tree, and all are defined in the DOM
standard.

The W3C commitee (`http://www.w3c.org <http://www.w3c.org>`_) has defined
several version of the DOM, each building on the previous one and adding
several enhancements.

XML/Ada currently supports the second revision of DOM (DOM 2.0), which mostly
adds namespaces over the first revision. The third revision is not supported at
this point, and it adds support for loading and saving XML streams in a
standardized fashion.

Although it doesn't support DOM 3.0, XML/Ada provides subprograms for doing
similar things.

Only the Core module of the DOM standard is currently implemented, other
modules will follow.

Note that the :file:`encodings.ads` file specifies the encoding to use to store
the tree in memory. Full compatibility with the XML standard requires that this
be UTF16, however, it is generally much more memory-efficient for European
languages to use UTF8. You can freely change this and recompile.

.. _Using_DOM:

Using DOM
=========

In XML/Ada, the DOM tree is build through a special implementation of a
SAX parser, provided in the `DOM.Readers` package.

Using DOM to read an XML document is similar to using SAX: one must setup an
input stream, then parse the document and get the tree. This is done with a
code similar to the following:

.. literalinclude:: dom/domexample.adb
   :language: ada
   :linenos:

This code is almost exactly the same as the code that was used when
demonstrating the use of SAX (:ref:`Using_SAX`).

The main two differences are:

* We no longer need to define our own XML reader, and we simply use the
  one provided in `DOM.Readers`.
* We therefore do not add our own callbacks to react to the XML events.
  Instead, the last instruction of the program gets a handle on the tree that
  was created in memory.

The tree can now be manipulated to get access to the value stored.
If we want to implement the same thing we did for SAX, the code would look
like:

.. literalinclude:: dom/domexample2.adb
   :language: ada
   :linenos:

The code is much simpler than with SAX, since most of the work is done
internally by XML/Ada. In particular, for SAX we had to take into account the
fact that the textual contents of a node could be reported in several events.
For DOM, the tree is initially normalized, ie all text nodes are collapsed
together when possible.

This added simplicity has one drawback, which is the amount of memory required
to represent even a simple tree.

XML/Ada optimizes the memory necessary to represent a tree by sharing the
strings as much as possible (this is under control of constants at the
beginning of :file:`dom-core.ads`). Still, DOM requires a significant amount of
information to be kept for each node.

For really big XML streams, it might prove impossible to keep the whole tree in
memory, in which case ad hoc storage might be implemented through the use of a
SAX parser. The implementation of `dom-readers.adb` will prove helpful in
creating such a parser.

Editing DOM trees
=================

Once in memory, DOM trees can be manipulated through subprograms provides by
the DOM API.

Each of these subprograms is fully documented both in the Ada specs (the
:file:`*.ads` files) and in the DOM standard itself, which XML/Ada follows
fully.

One important note however is related to the use of strings. Various
subprograms allows you to set the textual content of a node, modify its
attributes,.... Such subprograms take a Byte_Sequence as a parameter.

This Byte_Sequence must always be encoded in the encoding defined in the
package `Sax.Encoding` (as described earlier, changing this package requires
recompiling XML/Ada). By default, this is UTF-8.

.. highlight:: ada

Therefore, if you need to set an attribute to a string encoded for
instance in iso-8859-15, you should use the subprogram
`Unicode.Encodings.Convert` to convert it appropriately.
The code would thus look as follows::

    Set_Attribute (N, Convert ("å", From => Get_By_Name ("iso-8859-15")));
  
Printing DOM tress
==================

The standard DOM 2.0 does not define a common way to read DOM trees from
input sources, nor how to write them back to output sources. This was
added in later revision of the standard (DOM 3.0), which is not yet
supported by XML/Ada.

However, the package :file:`DOM.Core.Nodes` provides a `Write`
procedure that can be used for that purpose. It outputs a given DOM tree
to an Ada stream. This stream can then be connected to a standard file
on the disk, to a socket, or be used to transform the tree into a string
in memory.

An example is provided in the XML/Ada distribution, called
:file:`dom/test/tostring.adb` which shows how you can create a stream to
convert the tree in memory, without going through a file on the disk

