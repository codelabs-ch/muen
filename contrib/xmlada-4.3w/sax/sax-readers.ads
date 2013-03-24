------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2005;

with Ada.Unchecked_Deallocation;
with Input_Sources;
with Interfaces;
with Sax.Locators;
with Sax.Exceptions;
with Sax.Attributes;
with Sax.Models;
with Sax.Symbols;
with Sax.Utils;            use Sax.Utils;
with Unicode;
with Unicode.CES;
with Sax.HTable;
pragma Elaborate_All (Sax.HTable);

package Sax.Readers is

   type Sax_Reader is tagged private;
   type Sax_Reader_Access is access all Sax_Reader'Class;
   --  This package defines two types of XML readers: Reader is the historic
   --  type; Sax_Reader was added later on.
   --  These two readers differ by the type of parameters to their callbacks.
   --  The callbacks of Sax_Reader require less string copying and memory
   --  allocations, so are therefore more efficient. On the other hand, they
   --  do not pass strings directly (for the name of the elements for instance)
   --  but symbols (basically, naturals that can be converted to a string
   --  through calls to Get_Symbol below).
   --  New code is encouraged to extend Sax_Reader rather than Reader.

   procedure Parse
     (Parser  : in out Sax_Reader;
      Input   : in out Input_Sources.Input_Source'Class);
   --  Parse an XML stream, and calls the appropriate SAX callbacks for each
   --  event.
   --  To parse a stream, you must therefore extend the Reader or Sax_Reader
   --  class, and override any of the callbacks (see "Content Handlers" below).
   --  You then call Parse.
   --  This is not re-entrant: you can not call Parse with the same Parser
   --  argument in one of the SAX callbacks. This has undefined behavior.

   procedure Set_Symbol_Table
     (Parser  : in out Sax_Reader;
      Symbols : Symbol_Table);
   --  Symbols is the symbol table to use. Most of the time, it should be left
   --  to null, but you might want to share it with other parsers for
   --  efficiency (in which case you will need to provide a task-safe version
   --  of the symbol table).
   --  If Symbols is null (or this subprogram is not called) a symbol table
   --  will be created just for that parser and discarded along with the parser
   --
   --  This subprogram must be called before calling Parse.

   procedure Set_XML_Version
     (Parser : in out Sax_Reader; XML : XML_Versions := XML_1_0_Fifth_Edition);
   function Get_XML_Version (Parser : Sax_Reader) return XML_Versions;
   --  Set the XML version to accept.

   procedure Set_Feature
     (Parser : in out Sax_Reader; Name : String; Value : Boolean);
   function Get_Feature (Parser : Sax_Reader; Name : String) return Boolean;
   --  Set or lookup the value of a feature
   --  Name is a fully qualified URI, see below in "Recognized features" for
   --  more information.

   procedure Use_Basename_In_Error_Messages
     (Parser       : in out Sax_Reader;
      Use_Basename : Boolean := True);
   function Use_Basename_In_Error_Messages
     (Parser       : Sax_Reader) return Boolean;
   --  Indicates whether error messages will include only the base name of
   --  files, or the full file names. In the latter case, the error message
   --  itself might be incomplete, since the message attached to an Ada
   --  exception is limited to 200 characters.
   --  For backward compatibility, the default is to show full file names.

   -------------------------
   -- Recognized features --
   -------------------------
   --  The two strings below reference the two default features that are
   --  recognized by all parsers.

   Namespace_Feature : constant String :=
     "http://www.xml.org/sax/features/namespace";
   --  Controls general namespace processing. If it is true (the default),
   --  namespace URIs will be used in events.
   --  If False, colons (':') are allowed in tag names, and not considered
   --  as namespace identifiers.
   --  In fact, this is only given for full compatibility with the SAX
   --  standard. As authorized in the standard, this parser will always
   --  report URIs to the Start_Element and End_Element callbacks.
   --
   --  Default is True.

   Namespace_Prefixes_Feature : constant String :=
     "http://www.xml.org/sax/features/namespace-prefixes";
   --  Controls the reporting of qNames and namespace attributes (xmlns*) to
   --  the application.
   --  When this is False (the default), qNames may optionaly be reported,
   --  and namespace attributes must not be reported.

   --  Summary of the above two features:
   --  1: Namespace names
   --  2: Start/endPrefixMapping
   --  3: qNames
   --  4: xmlns* attributes
   --  namespaces namespace-prefixes   1        2       3      4
   --     true          false         YES      YES   unknown   NO
   --     true          true          YES      YES     YES    YES
   --     false         false         (ILLEGAL COMBINATION)
   --     false         true         unknown unknown   YES    YES
   --
   --  Default is False.

   Validation_Feature : constant String :=
     "http://www.xml.org/sax/features/validation";
   --  If True (not the default), a number of additional tests are performed
   --  while parsing the document, most notably that the document matches
   --  the DTD (internal and external subset).
   --  In such a case, the DTD must be present.
   --
   --  XML/Ada doesn't currently support validating against a DTD.

   Schema_Validation_Feature : constant String :=
     "http://www.adacore.com/sax/features/schema_validation";
   --  If True (not the default), XML/Ada will attempt to validate the XML
   --  document against an XML schema. However, your reader must also extend
   --  the Schema.Readers.Validating_Reader class (see comments in that
   --  package).

   External_General_Entities_Feature : constant String :=
     "http://xml.org/sax/features/external-general-entities";
   --  If True, include all external general text entities.
   --  If False, these are not included, and will be reported with
   --  Content_Handlers.Skipped_Entity.
   --
   --  Default is True

   External_Parameter_Entities_Feature : constant String :=
     "http://xml.org/sax/features/external-parameter-entities";
   --  If True, include all external parameter entities, including the
   --  external DTD subset. Parameter entities are the ones defined in DTDs
   --  and whose name starts with '%'

   Parameter_Entities_Feature : constant String :=
     "http://xml.org/sax/features/lexical-handler/parameter-entities";
   --  True if the SAX parser will reports parameter entities through its
   --  Lexical_Handler.

   Test_Valid_Chars_Feature : constant String :=
     "http://www.adacore.com/sax/features/test_valid_chars";
   --  True if the SAX parser will check for each character read from the
   --  input streams whether it is valid. This might slow done the parser,
   --  but will provide better validation.
   --  This is False by default.

   -------------------
   -- Error handler --
   -------------------
   --  The following functions are defined in the Error_Handler interface
   --  in the SAX standard.

   procedure Warning
     (Handler : in out Sax_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is null;
   --  Receive notification of a warning.
   --  This method is used to report conditions that are not errors or fatal
   --  errors.
   --  The SAX parser must continue to provide normal parsing events after
   --  invoking this method.
   --  Default action is to do nothing.

   procedure Error
     (Handler : in out Sax_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class) is null;
   --  Receive notification of a recoverable error.
   --  For example, a validating parser would use this callback to report the
   --  violation of a validity constraint. The default behaviour is to take no
   --  Action.
   --  The SAX parser must continue to provide normal parsing events after
   --  invoking this method. If the application cannot do so, then the parser
   --  should report a fatal error.
   --  Default action is to do nothing.

   procedure Fatal_Error
     (Handler : in out Sax_Reader;
      Except  : Sax.Exceptions.Sax_Parse_Exception'Class);
   --  Receive notification of a non-recoverable error.
   --  For example, a parser would use this callback to report the violation
   --  of a well-Formedness constraint.
   --  The application must assume that the document is unusable after the
   --  parser has invoked this method. Thus, a Program_Error will be raised
   --  if your callback returns. You should always raise an exception.
   --  Default action is to raise an exception Fatal_Error;

   ----------------
   -- Attributes --
   ----------------
   --  Although there is a more complete attributes API in the package
   --  Sax.Attributes, the following types are used for those more efficient
   --  callbacks. The following attributes do not require any memory allocation
   --  however they are only valid while the parser has not been destroyed.

   type Sax_Attribute is private;
   type Sax_Attribute_List is private;
   --  A lighter weight version of attributes than Attributes,
   --  based on symbols.

   function Get_Index
     (List       : Sax_Attribute_List;
      URI        : Sax.Symbols.Symbol;
      Local_Name : Sax.Symbols.Symbol) return Integer;
   function Get_Index
     (Handler    : Sax_Reader'Class;
      List       : Sax_Attribute_List;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence) return Integer;
   --  Return the index of the attribute within the list, or -1 if not found.
   --  The first version is more efficient. The idea is that the symbols can be
   --  computed once when the parsing starts, and then reused. They are much
   --  faster to compare than strings.
   --  The second version is provided to help transitions.
   --
   --  A more efficient approach is to traverse the list of attributes only
   --  once and store the values in your own record, rather than traverse the
   --  list of attributes every time you need to access a value:
   --
   --     Name : Qualified_Name;
   --     for J in 1 .. Get_Length (List) loop
   --        Name := Get_Qualified_Name (List, J);
   --        if Name.NS = Empty_String and then Name.Local = ... then
   --           ...;
   --        elsif ... then
   --           ...
   --        end if;
   --     end loop;

   procedure Set_Value
     (List  : Sax_Attribute_List;
      Index : Integer;
      Val   : Sax.Symbols.Symbol);
   function Get_Value
     (List : Sax_Attribute_List; Index : Integer) return Sax.Symbols.Symbol;
   --  Returns No_Symbol if Index is negative.
   --  Use  Get (Get_Value (List, Index)).all  to retrive the strings value
   --  (or in Ada05 dotted notation:  Get_Value (List, Index).Get.all

   function Get_Location
     (List : Sax_Attribute_List; Index : Integer) return Sax.Locators.Location;
   --  Return the start location for this attribute

   function Get_Non_Normalized_Value
     (List : Sax_Attribute_List; Index : Integer) return Sax.Symbols.Symbol;
   function Get_Value_As_Boolean
     (List : Sax_Attribute_List; Index : Integer; Default : Boolean := False)
      return Boolean;
   pragma Inline (Get_Value, Get_Non_Normalized_Value, Get_Value_As_Boolean);
   --  Return the value of the corresponding attribute.
   --  [Default] is returned if the attribute does not exist

   procedure Set_Normalized_Value
     (List : Sax_Attribute_List; Index : Integer; Value : Sax.Symbols.Symbol);
   pragma Inline (Set_Normalized_Value);
   --  Set the normalized value of the attribute

   function Get_Type
     (List : Sax_Attribute_List; Index : Integer)
      return Sax.Attributes.Attribute_Type;
   procedure Set_Type
     (List : Sax_Attribute_List; Index : Integer;
      Typ  : Sax.Attributes.Attribute_Type);
   pragma Inline (Get_Type, Set_Type);
   --  Return the type of the attribute

   function Get_Length (List : Sax_Attribute_List) return Natural;
   pragma Inline (Get_Length);
   --  Return the number of attributes in the list

   type Qualified_Name is record
      NS    : Sax.Symbols.Symbol;
      Local : Sax.Symbols.Symbol;
   end record;
   No_Qualified_Name : constant Qualified_Name :=
     (Sax.Symbols.No_Symbol, Sax.Symbols.No_Symbol);

   function Get_Prefix
     (List : Sax_Attribute_List; Index : Integer) return Sax.Symbols.Symbol;
   function Get_Name
     (List : Sax_Attribute_List; Index : Integer) return Qualified_Name;
   function Get_Qname
     (List : Sax_Attribute_List; Index : Integer)
      return Unicode.CES.Byte_Sequence;  --  Using the prefix
   pragma Inline (Get_Prefix, Get_Name, Get_QName);
   --  Return the various name components of the attribute

   ----------------------
   -- Content Handlers --
   ----------------------
   --  The following functions are defined in the Content_Handler interface
   --  in the SAX standard.
   --  The default for all the subprograms below is to do nothing, unless
   --  otherwise specified.

   procedure Set_Document_Locator
     (Handler : in out Sax_Reader; Loc : in out Sax.Locators.Locator) is null;
   --  Receive an object for locating the origin of SAX document events.
   --  SAX parsers are strongly encouraged but not required to give this
   --  information. This callback will always be called before any other.
   --  Note that [Loc] is only valid within the call to [Parse], and will be
   --  free on exit, so should no longer be referenced.
   --  In practice, this callback (mandated by the standard), is not so very
   --  useful and direct calls to [Locator] below should be preferred.

   function Current_Location
     (Handler : Sax_Reader) return Sax.Locators.Location;
   pragma Inline (Current_Location);
   --  Return the current location in the stream (or [No_Location] if parsing
   --  has finished or not started).

   procedure Start_Document (Handler : in out Sax_Reader) is null;
   --  Receive notification of the beginning of a document.
   --  This callback is called only once by the parser, before any other
   --  function in this interface except Set_Document_Locator.

   procedure End_Document (Handler : in out Sax_Reader) is null;
   --  Receive notification of the end of a document.
   --  This callback will be called only once once it has reached the end of
   --  the input stream. It won't be called if a Fatal_Error is raised, it is
   --  your responsability to call the callback yourself in this case.

   procedure Start_Prefix_Mapping
     (Handler : in out Sax_Reader;
      Prefix  : Sax.Symbols.Symbol;
      URI     : Sax.Symbols.Symbol) is null;
   --  Begin the scope of a prefix-URI mapping.
   --  This callback is not necessarily for normal namespace processing, since
   --  the SAX parser will automatically substitute prefixes for elements and
   --  attributes if XML_Readers.Namespace_Feature is set to True.
   --  However, there are cases where the automatic replacement can not be
   --  safely done, and in this case this callback is invoked.
   --  It is not garanteed that calls to End_Prefix_Mapping will occur in the
   --  same order (or the reverse one) as Start_Prefix_Mapping.

   procedure End_Prefix_Mapping
     (Handler : in out Sax_Reader;
      Prefix  : Sax.Symbols.Symbol) is null;
   --  End the scope of a prefix-URI mapping.
   --  This will always occur after the corresponding End_Element event.

   procedure Start_Element
     (Handler    : in out Sax_Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax_Attribute_List) is null;
   --  Receive notification of the beginning of an element.
   --  There will always be a matching call to End_Element, even for empty
   --  elements.
   --  Up to three name components can be given for each element, depending
   --  on the value of the XML_Reader features.
   --  - Namespace_URI and Local_Name are required when Namespace_Feature is
   --    True, but are optional if False. If one is specified, both must be.
   --  - Qname (qualified name) is required if Namespace_Prefixes_Feature is
   --    True, and optional if False. This is basically of the form "Ns:Name"
   --  The attribute list will only contain attributes with explicit values. It
   --  will contain attributes used for namespace declaration (xmlns*) only if
   --  Namespace_Prefixes_Feature is True.
   --
   --  For users of older versions of XML/Ada, the old profile of Start_Element
   --  is still available if you derive from the "Reader" type (below) instead
   --  of "Sax_Reader". We do encourage you to transition to the new profiles
   --  at your convenience, though, because they provide greater efficiency,
   --  mostly by limiting the number of string comparison and allocations.

   procedure End_Element
     (Handler    : in out Sax_Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol) is null;
   --  Receive notification of the end of an element.

   procedure Characters
     (Handler : in out Sax_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is null;
   --  Receives notification of character data.
   --  XML parsers may return all contiguous character data in a single chunk,
   --  or they may split them into several chunks. However, all of the
   --  characters in any single event must come from the same external entity
   --  so that the Locator provides useful information
   --
   --  Note that some parsers will report (and validating parsers must) report
   --  whitespace between elements using the Ignorable_Whitespace event.

   procedure Ignorable_Whitespace
     (Handler : in out Sax_Reader;
      Ch      : Unicode.CES.Byte_Sequence) is null;
   --  Receive notification of ignorable whitespace in element content (ie
   --  for elements whose xml:space attribute is not set to 'preserve', see
   --  XML specifications 2.10)
   --  If there is only white spaces between two tags, they are reported via
   --  this callback.
   --  SAX parsers may return all contiguous whitespace in a single chunk, or
   --  they may split it into several chunks.

   procedure Processing_Instruction
     (Handler : in out Sax_Reader;
      Target  : Unicode.CES.Byte_Sequence;
      Data    : Unicode.CES.Byte_Sequence) is null;
   --  Receive notification of a processing instruction.
   --  A SAX parser must never report an XML declaration (<?xml..?>, 2.8 in
   --  XML specifications) or a text declaration (<?xml?>, 4.3.1 in XML
   --  specifications) using this method.

   procedure Skipped_Entity
     (Handler : in out Sax_Reader;
      Name    : Sax.Symbols.Symbol) is null;
   --  Receive notification of a skipped entity.
   --  The Parser will invoke this method once for each entity
   --  skipped. Non-validating processors may skip entities if they have not
   --  seen the declarations (because, for example, the entity was declared in
   --  an external DTD subset). All processors may skip external Entities,
   --  depending on the value of External_General_Entities_Feature and
   --  External_Parameter_Entities_Feature.
   --
   --  Name is the name of the skipped entity. If it is a parameter entity,
   --  the name will begin with '%', and if it is the external DTD subset,
   --  it will be the string "[dtd]".

   ------------------
   -- DTD Handlers --
   ------------------
   --  The following functions are defined in the DTD_Handler interface
   --  in the SAX standard.

   procedure Unparsed_Entity_Decl
     (Handler       : in out Sax_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence;
      Notation_Name : Unicode.CES.Byte_Sequence) is null;
   --  Receive notification of an unparsed entity declaration event.
   --  This is for entities like  "<!ENTITY foo SYSTEM ".." NDATA gif>"

   procedure Notation_Decl
     (Handler       : in out Sax_Reader;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence) is null;
   --  Receive notification of a notation declaration event.
   --  At least one of publicId and systemId must be non-null. If a system
   --  identifier is present, and it is a URL, the SAX parser must resolve it
   --  fully before passing it to the application through this event.
   --  There is no guarantee that the notation declaration will be reported
   --  before any unparsed entities that use it.

   ---------------------
   -- Entity Resolver --
   ---------------------
   --  The following functions are defined in the Entity_Resolver interface
   --  in the SAX standard.

   function Resolve_Entity
     (Handler   : Sax_Reader;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence)
      return Input_Sources.Input_Source_Access;
   --  Allow the application to resolve external entities.
   --  The parser will call this method before opening any external entity
   --  except the top-level document entity. Such entities include the external
   --  DTD subset and external parameter entities referenced within the DTD (in
   --  either case, only if the parser reads external parameter entities), and
   --  external general entities referenced within the document element (if the
   --  parser reads external general entities). The application may request
   --  that the parser locate the entity itself, that it use an alternative
   --  URI, or that it use data provided by the application (as a character or
   --  byte input stream).
   --  Application writers can use this method to redirect external system
   --  identifiers to secure and/or local URIs, to look up public identifiers
   --  in a catalogue, or to read an entity from a database or other input
   --  source (including, for example, a dialog box). Neither XML nor SAX
   --  specifies a preferred policy for using public or system IDs to resolve
   --  resources. However, SAX specifies how to interpret any InputSource
   --  returned by this method, and that if none is returned, then the system
   --  ID will be dereferenced as a URL.
   --
   --  If the returned value is null, the standard algorithm is used. Otherwise
   --  the returend value is automatically freed by the parser when no longer
   --  needed.
   --
   --  Calls to this subprogram are nested within Start_Entity/End_Entity.

   ---------------------
   -- Lexical Handler --
   ---------------------
   --  The following functions are defined in the Lexical_Handler interface
   --  in the extended SAX standard. This is not part of the standard itself,
   --  but rather part of the extension for it.
   --  Note that the SAX standard indicates that such extended handlers should
   --  be set through properties, but this is not necessary in this
   --  implementation where you simply have to override the following
   --  subprograms.

   procedure Comment
     (Handler : in out Sax_Reader; Ch : Unicode.CES.Byte_Sequence) is null;
   --  Report an XML comment anywhere in the document.
   --  Default behavior is to do nothing.

   procedure Start_Cdata (Handler : in out Sax_Reader) is null;
   --  Report the start of a CData section.
   --  The content of the section is reported through the usual Characters
   --  event, this only acts as the boundary.

   procedure End_Cdata (Handler : in out Sax_Reader) is null;
   --  Report the end of a CData section

   procedure Start_Entity
     (Handler : in out Sax_Reader;
      Name    : Sax.Symbols.Symbol) is null;
   --  Report the beginning of some internal and external XML entities.
   --  Check the feature Parameter_Entities_Feature to know if the handler
   --  will report these events.

   procedure End_Entity
     (Handler : in out Sax_Reader;
      Name    : Sax.Symbols.Symbol) is null;
   --  Report the end of an entity

   procedure Start_DTD
     (Handler   : in out Sax_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence := "";
      System_Id : Unicode.CES.Byte_Sequence := "") is null;
   --  Report the start of DTD declarations, if any.
   --  All events reported to a Decl_Handler are reported between a Start_DTD
   --  and an End_DTD event.
   --  Public_Id and System_Id might be the empty string if none was declared.
   --  The events following Start_DTD (and before the matching End_DTD) are
   --  assumed to be part of the internal subset of the DTD, unless they
   --  appear between a Start_Entity and End_Entity events (with "[dtd]" for
   --  the name).

   procedure End_DTD (Handler : in out Sax_Reader) is null;
   --  Report the end of a DTD section

   ------------------
   -- Decl Handler --
   ------------------
   --  The following functions are defined in the Decl_Handler interface
   --  in the extended SAX standard. This is not part of the standard itself,
   --  but rather part of the extension for it.

   procedure Internal_Entity_Decl
     (Handler : in out Sax_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Value   : Unicode.CES.Byte_Sequence) is null;
   --  Report an internal entity declaration.
   --  This is for <!ENTITY...> notations in the DTD, where the value is
   --  specified directly as a string.
   --  Only the effective (first) declaration for each entity will be reported.
   --  All parameter entities in the value will be expanded, but general
   --  entities will not.
   --  For Parameter entities, Name will start with '%'

   procedure External_Entity_Decl
     (Handler   : in out Sax_Reader;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence) is null;
   --  Report a parsed external entity declaration, ie when their value is
   --  not defined as a string.

   procedure Element_Decl
     (Handler : in out Sax_Reader;
      Name    : Unicode.CES.Byte_Sequence;
      Model   : Sax.Models.Content_Model) is null;
   --  Report an element type declaration.
   --  Model represents the content model for this element. If you need to keep
   --  a copy of it, you must Ref it, and Unref it when you no longer need the
   --  copy, for proper memory management.
   --  The model is normalized so that all parameter entities are fully
   --  resolved and all whitespace is removed,and includes the enclosing
   --  parentheses.

   procedure Attribute_Decl
     (Handler       : in out Sax_Reader;
      Ename         : Unicode.CES.Byte_Sequence;
      Aname         : Unicode.CES.Byte_Sequence;
      Typ           : Sax.Attributes.Attribute_Type;
      Content       : Sax.Models.Content_Model;
      Value_Default : Sax.Attributes.Default_Declaration;
      Value         : Unicode.CES.Byte_Sequence) is null;
   --  Report an attribute type declaration.
   --  Only the first declaration for an attribute will be reported.
   --  If Typ is Notation or Enumeration, then Content will contain the
   --  description model for the attribute. Otherwise Content is null.
   --  If you need to keep a copy of Content, you must Ref it, and Unref it
   --  when you are done using it.
   --  Value_Default represents the attribute default requirements
   --  ("#IMPLIED", "#REQUIRED", or "#FIXED").
   --  Value is a string representing the attribute's default value, or ""
   --  if there is none

   XML_Fatal_Error : exception;

   -------------------
   -- Misc services --
   -------------------

   function Prefix_From_Qname (Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;
   --  Return the prefix part of Qname, or the empty string if no explicit
   --  prefix is defined.

   -----------
   -- Hooks --
   -----------
   --  A parser will call some hooks before it calls the primitive operations
   --  like Start_Element,...
   --  These hooks are meant for internal use only at this point, since it is
   --  cleaner for the user to simply extend the primitive operation.
   --  These are currently used to plug in an XML validator while limiting the
   --  dependencies between the SAX and Schema modules.

   type Hook_Data is abstract tagged null record;
   type Hook_Data_Access is access all Hook_Data'Class;

   procedure Free (Data : in out Hook_Data) is abstract;
   --  Free the memory associated with the data

   type Element is private;
   type Element_Access is access Element;

   function To_QName
     (Namespace_URI, Local_Name : Sax.Symbols.Symbol)
      return Unicode.CES.Byte_Sequence;
   function To_QName
     (Elem   : Element_Access) return Unicode.CES.Byte_Sequence;
   --  Return the qualified name "{namespace_uri}local_name"

   function Start_Tag_Location
     (Elem : Element_Access) return Sax.Locators.Location;
   function Start_Tag_End_Location
     (Elem : Element_Access) return Sax.Locators.Location;
   --  The location for the start of the element (start tag and end tag).

   function Get_NS (Elem : Element_Access) return XML_NS;
   function Get_Local_Name (Elem : Element_Access) return Sax.Symbols.Symbol;
   pragma Inline (Get_NS, Get_Local_Name);
   --  Return the name and local name of the element

   procedure Initialize_Symbols (Parser : in out Sax_Reader);
   --  Initialize the symbol table with some predefined symbols

   function Find_Symbol
     (Parser : Sax_Reader'Class;
      Str    : Unicode.CES.Byte_Sequence) return Sax.Symbols.Symbol;
   function Get_Symbol_Table (Parser : Sax_Reader'Class) return Symbol_Table;
   --  Manipulation of symbols

   procedure Find_NS
     (Parser             : Sax_Reader'Class;
      Prefix             : Sax.Symbols.Symbol;
      NS                 : out XML_NS;
      Include_Default_NS : Boolean := True);
   --  Search the namespace associated with a given prefix in the scope of
   --  Elem or its parents. Use the empty string to get the default namespace.
   --  Fatal_Error is raised if no such namespace was found (and null is
   --  returned, in case Fatal_Error didn't raise an exception)
   --  The default namespace is not resolved if Include_Default_NS is False.
   --  Returns No_XML_NS if the namespace is not defined

   procedure Find_NS_From_URI
     (Parser             : in out Sax_Reader'Class;
      URI                : Sax.Symbols.Symbol;
      NS                 : out XML_NS);
   --  Return the XML_NS for URI. There could be several, and the most recent
   --  one is returned (that is with the prefix that was defined last in the
   --  current context.
   --  Returns No_XML_NS if the namespace is not defined

   type Start_Element_Hook is access procedure
     (Handler : access Sax_Reader'Class;
      Element : Element_Access;
      Atts    : in out Sax_Attribute_List);
   --  This hook should take the opportunity of normalizing attribute values
   --  if necessary (basic normalization is already done by the SAX parser,
   --  but based on information extracted from schemas, further normalization
   --  might be needed).
   --  The list of attributes Atts has not been checked, and thus some of the
   --  attributes might have wrong values, or some attributes might be missing.
   --  This hook is really intended for validating parsers to do their own
   --  checks in any case. Standard applications should override Start_Element.

   type End_Element_Hook is access procedure
     (Handler : access Sax_Reader'Class; Elem : Element_Access);
   type Characters_Hook is access procedure
     (Handler : access Sax_Reader'Class; Ch : Unicode.CES.Byte_Sequence);
   type Whitespace_Hook is access procedure
     (Handler : access Sax_Reader'Class; Ch : Unicode.CES.Byte_Sequence);
   type Set_Doc_Locator_Hook is access procedure
     (Handler : in out Sax_Reader'Class;
      Loc     : in out Sax.Locators.Locator);
   type Notation_Decl_Hook is access procedure
     (Handler   : access Sax_Reader'Class;
      Name      : Unicode.CES.Byte_Sequence;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence);

   function Get_Hooks_Data (Handler : Sax_Reader) return Hook_Data_Access;
   --  Return the hook data that was set through Set_Hooks. This could be null

   procedure Set_Hooks
     (Handler       : in out Sax_Reader;
      Data          : Hook_Data_Access     := null;
      Start_Element : Start_Element_Hook   := null;
      End_Element   : End_Element_Hook     := null;
      Characters    : Characters_Hook      := null;
      Whitespace    : Whitespace_Hook      := null;
      Doc_Locator   : Set_Doc_Locator_Hook := null;
      Notation_Decl : Notation_Decl_Hook   := null);
   --  Set a list of hooks to be called before calling the usual primitive
   --  operations. These override hooks that were defined previously.
   --  Data will be passed to each of the hook. It is automatically
   --  deallocated when no longer needed by the parser (ie the next call to
   --  Set_Hooks or when the parser itself is freed).

   procedure Error (Parser : in out Sax_Reader'Class; Msg : String);
   --  Raises an error

   ------------
   -- Reader --
   ------------

   type Reader is new Sax_Reader with private;
   type Reader_Access is access all Reader'Class;
   --  This is the old type that was provided by this package

   procedure Start_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Unicode.CES.Byte_Sequence;
      URI     : Unicode.CES.Byte_Sequence) is null;
   procedure End_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Unicode.CES.Byte_Sequence) is null;
   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class) is null;
   procedure End_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "") is null;
   procedure Skipped_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence) is null;
   procedure Start_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence) is null;
   procedure End_Entity
     (Handler : in out Reader;
      Name    : Unicode.CES.Byte_Sequence) is null;
   --  See documentation for similarly named callbacks for Sax_Reader.
   --  These subprograms require extra processing and are less efficient than
   --  the above subprograms

   overriding procedure Start_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Sax.Symbols.Symbol;
      URI     : Sax.Symbols.Symbol);
   overriding procedure End_Prefix_Mapping
     (Handler : in out Reader; Prefix : Sax.Symbols.Symbol);
   overriding procedure Start_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol;
      Atts       : Sax_Attribute_List);
   overriding procedure End_Element
     (Handler    : in out Reader;
      NS         : Sax.Utils.XML_NS;
      Local_Name : Sax.Symbols.Symbol);
   overriding procedure Skipped_Entity
     (Handler : in out Reader;
      Name    : Sax.Symbols.Symbol);
   overriding procedure Start_Entity
     (Handler : in out Reader;
      Name    : Sax.Symbols.Symbol);
   overriding procedure End_Entity
     (Handler : in out Reader;
      Name    : Sax.Symbols.Symbol);
   --  See inherited documentation

private

   type Parser_Hooks is record
      Data          : Hook_Data_Access     := null;
      Start_Element : Start_Element_Hook   := null;
      End_Element   : End_Element_Hook     := null;
      Characters    : Characters_Hook      := null;
      Whitespace    : Whitespace_Hook      := null;
      Doc_Locator   : Set_Doc_Locator_Hook := null;
      Notation_Decl : Notation_Decl_Hook   := null;
   end record;

   Entities_Table_Size : constant := 50;
   --  Size of the hash-table used to store entities.
   --  This is not a hard limit on the number of entities that can be defined.
   --  However, if this number is too small with regards to the number of
   --  entities, there will be conflicts in the hash-table that will slow
   --  down the lookup.

   Default_Atts_Table_Size : constant := 50;
   --  Size of the hash-table used to store the default attributes

   function Hash (Str : String) return Interfaces.Unsigned_32;
   --  Compute hash function for given String

   --------------
   -- Entities --
   --------------
   --  We need to memorize all the declared entities, so as to do the
   --  substitution ourselves.

   type Entity_Entry is record
      Name         : Sax.Symbols.Symbol;
      Value        : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Public       : Sax.Symbols.Symbol;

      External     : Boolean;
      --  Whether the entity references an external document

      Unparsed     : Boolean;
      --  Whether we have an unparsed entity (ie using a NOTATION)

      External_Declaration : Boolean;
      --  Whether the entity was defined in the external subset

      Already_Read : Boolean := False;
      --  True if the value of the entity was already read. This is used to
      --  detect entities referencing themselves.
   end record;
   type Entity_Entry_Access is access Entity_Entry;

   procedure Free is new Ada.Unchecked_Deallocation
     (Entity_Entry, Entity_Entry_Access);
   function Get_Key (Entity : Entity_Entry_Access) return Sax.Symbols.Symbol;

   package Entity_Table is new Sax.HTable
     (Element       => Entity_Entry_Access,
      Empty_Element => null,
      Free          => Free,
      Key           => Sax.Symbols.Symbol,
      Get_Key       => Get_Key,
      Hash          => Sax.Symbols.Hash,
      Equal         => Sax.Symbols."=");

   type Entity_Input_Source;
   type Entity_Input_Source_Access is access Entity_Input_Source;
   type Entity_Input_Source is record
      External : Boolean;
      Next  : Entity_Input_Source_Access;
      Name  : Sax.Symbols.Symbol;
      --  Name of the entity

      Handle_Strings : Boolean := True;
      --  True if " and ' should be recognized as special characters.
      --  This is used so that a string started in one stream isn't terminated
      --  in another entity or stream.

      System_Id : Sax.Symbols.Symbol;
      Public_Id : Sax.Symbols.Symbol;
      --  Uniq System_Id for each input source

      Input    : Input_Sources.Input_Source_Access;
      Save_Loc : Sax.Locators.Location;
   end record;

   type Parser_State is record
      Name : String (1 .. 3);
      --  Name of the state (debugging purposes)

      Ignore_Special : Boolean := False;
      --  True if special characters should be ignored (as is the case in
      --  strings).  ??? Could be ignored, duplicates Greater_Special,
      --  Less_Special, ..

      Detect_End_Of_PI : Boolean := False;
      --  Whether ?> should be reported as end of PI

      Greater_Special : Boolean := False;
      --  Whether > is considered a special character

      Less_Special : Boolean := False;
      --  Should be true if < should be reported separately. Note that in that
      --  case it won't even be associated with the following character if
      --  it is '!', '?',...

      Expand_Param_Entities : Boolean := False;
      --  True if %...; param entities should be recognized, as is the case in
      --  the DTD

      Expand_Entities : Boolean := True;
      --  True if &...; should be recognized

      Report_Character_Ref : Boolean := False;
      --  True if character references &#...; should be reported as a single
      --  token, with their replacement character stored in the buffer.
      --  Ignored if Expand_Character_Ref is True.

      Expand_Character_Ref : Boolean := True;
      --  True if character references &#...; should be recognized and
      --  expanded

      In_DTD : Boolean := False;
      --  True if we are parsing the DTD, and '['. ']' and '<!' should be
      --  recognized as special tags

      Recognize_External : Boolean := False;
      --  True if PUBLIC, SYSTEM and NDATA should be recognized as special
      --  tokens

      In_Attlist : Boolean := False;
      --  True if we are in an <!ATTLIST, and we should recognize special
      --  keywords like ID, NMTOKEN,...

      Handle_Strings : Boolean := False;
      --  True if " and ' should be recognized as special characters
      --  ??? Should be merged with a In_String field, that would also replace
      --  Ignore_Special.

      In_Tag : Boolean := False;
      --  True if = and : should be recognized as special characters

      Report_Parenthesis : Boolean := False;
      --  True if Opening_Parenthesis should be reported separately
   end record;

   type Element is record
      NS             : XML_NS;
      Name           : Sax.Symbols.Symbol;
      Parent         : Element_Access;
      Start          : Sax.Locators.Location;  --  Start tag location
      Start_Tag_End  : Sax.Locators.Location := Sax.Locators.No_Location;
      --  Character after start tag (ie first char of content)
      Namespaces     : XML_NS;
      --  Namespaces defined for that element and its children
   end record;

   type Sax_Attribute is record
      Prefix       : Sax.Symbols.Symbol;
      Local_Name   : Sax.Symbols.Symbol;
      Value        : Sax.Symbols.Symbol;
      Non_Normalized_Value : Sax.Symbols.Symbol;
      NS           : Sax.Utils.XML_NS := Sax.Utils.No_XML_NS;
      Att_Type     : Sax.Attributes.Attribute_Type := Sax.Attributes.Cdata;
      Default_Decl : Sax.Attributes.Default_Declaration :=
        Sax.Attributes.Default;
      Location     : Sax.Locators.Location;  --  Where the declaration occurred
   end record;
   --  An attribute as read in the XML stream. This is used to temporarily
   --  store the list of attributes until we have parsed all the namespace
   --  declarations, after which a regular list of attributes is created.

   type Sax_Attribute_Array is array (Natural range <>) of Sax_Attribute;
   type Sax_Attribute_Array_Access is access all Sax_Attribute_Array;
   --  A list of attributes.

   type Sax_Attribute_List is record
      Count : Natural := 0;
      List  : Sax_Attribute_Array_Access;
   end record;

   type Attributes_Entry is record
      Element_Name : Sax.Symbols.Symbol;
      Attributes   : Sax_Attribute_Array_Access;
   end record;
   Null_Attribute : constant Attributes_Entry := (Sax.Symbols.No_Symbol, null);

   procedure Free (Att : in out Attributes_Entry);
   function Get_Key (Att : Attributes_Entry) return Sax.Symbols.Symbol;

   package Attributes_Table is new Sax.HTable
     (Element       => Attributes_Entry,
      Empty_Element => Null_Attribute,
      Free          => Free,
      Key           => Sax.Symbols.Symbol,
      Get_Key       => Get_Key,
      Hash          => Sax.Symbols.Hash,
      Equal         => Sax.Symbols."=");

   type Notation_Entry is record
      Name             : Sax.Symbols.Symbol;
      Declaration_Seen : Boolean;
   end record;
   Null_Notation : constant Notation_Entry := (Sax.Symbols.No_Symbol, False);

   procedure Free (Notation : in out Notation_Entry);
   function Get_Key (Notation : Notation_Entry) return Sax.Symbols.Symbol;

   package Notations_Table is new Sax.HTable
     (Element       => Notation_Entry,
      Empty_Element => Null_Notation,
      Free          => Free,
      Key           => Sax.Symbols.Symbol,
      Get_Key       => Get_Key,
      Hash          => Sax.Symbols.Hash,
      Equal         => Sax.Symbols."=");
   --  For notations, we simply store whether they have been defined or not,
   --  and then only for validating parsers

   type Sax_Reader is tagged record
      Buffer_Length : Natural := 0;
      Buffer        : Unicode.CES.Byte_Sequence_Access;

      Attributes       : Sax_Attribute_List;
      --  List of attributes for the current element. This array is to limit
      --  the number of memory allocations, by reusing it for each element.

      Locator       : Sax.Locators.Locator;
      Current_Node  : Element_Access;

      Public_Id     : Sax.Symbols.Symbol;
      System_Id     : Sax.Symbols.Symbol;
      --  The initial file we were parsing.

      Symbols        : Symbol_Table;
      Lt_Sequence    : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Gt_Sequence    : Sax.Symbols.Symbol;
      Amp_Sequence   : Sax.Symbols.Symbol;
      Apos_Sequence  : Sax.Symbols.Symbol;
      Quot_Sequence  : Sax.Symbols.Symbol;
      Xmlns_Sequence : Sax.Symbols.Symbol;
      Namespaces_URI_Sequence : Sax.Symbols.Symbol;
      Xml_Sequence            : Sax.Symbols.Symbol;
      Symbol_Percent   : Sax.Symbols.Symbol;
      Symbol_Ampersand : Sax.Symbols.Symbol;
      --  The symbol table, and a few predefined symbols

      Inputs        : Entity_Input_Source_Access;
      --  Entities and parameter entities are processed inline (if we
      --  temporarily substitute the input stream with the replacement text
      --  for the entity).
      --  When Inputs is null, the characters are read from the input stream
      --  given in the call to Parser.

      Close_Inputs  : Entity_Input_Source_Access;
      --  List of entities to be closed at the next call to Next_Token

      Default_Atts : Attributes_Table.HTable (Default_Atts_Table_Size);
      --  This table contains the list of default attributes defined for
      --  each element in the DTD. Index is the name of the elements.
      --  Note that the namespaces haven't been resolved for these default
      --  attributes, since in some cases the namespace itself could be defined
      --  as a default attribute.

      Notations : Notations_Table.HTable (Default_Atts_Table_Size);
      --  List of notations defined in the XML document. This is left empty
      --  if the parser isn't configured to do validation.

      Entities : Entity_Table.HTable (Entities_Table_Size);

      DTD_Name : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      --  Name of the DTD, and also name of the root element (in case we have
      --  a validating parser). This is left to null for non-validating
      --  parsers.

      Default_Namespaces : XML_NS;
      --  All the namespaces defined by default

      Num_Toplevel_Elements : Natural;
      --  Number of elements at the toplevel

      Element_Id : Natural := 0;
      --  Id of the current element. All elements created will have a
      --  different Id

      Hooks  : Parser_Hooks;
      --  Hooks to be called before the primitive operations

      XML_Version   : XML_Versions := XML_1_0_Fifth_Edition;

      Standalone_Document : Boolean := False;
      --  Whether the document is specified as "standalone" in the XML
      --  prolog

      Lookup_Char  : Unicode.Unicode_Char := Unicode.Unicode_Char'Last;
      --  We can have a single forward lookup character, which is used to speed
      --  the parsing.

      Last_Read     : Unicode.Unicode_Char;
      Last_Read_Is_Valid : Boolean := True;
      --  Whether Last_Read is was actualy read, or whether it was set to null
      --  because we encountered the end of an input stream.
      --  (For instance, when an entity is parsed, its contents always ends
      --  with ASCII.NUL and Last_Read_Is_Valid is set to False.

      State         : Parser_State;

      In_External_Entity : Boolean;
      --  Whether we are parsing an external entity

      Previous_Char_Was_CR : Boolean;
      --  True if the previous character read from the stream was a
      --  Carriage_Return (needed since XML parsers must convert these to
      --  one single Line_Feed).

      Ignore_State_Special : Boolean;
      --  If True, ignore the State.Ignore_Special flag in the next call
      --  to Next_Token. This is used for handling of special characters
      --  withing strings.

      Basename_In_Messages : Boolean := False;
      --  If True, error messages are output with simple basenames for the
      --  files. This is required in a lot of cases because the message
      --  attached to an Ada exception is limited to 200 characters.

      Feature_Namespace                   : Boolean := True;
      Feature_Namespace_Prefixes          : Boolean := False;
      Feature_External_General_Entities   : Boolean := True;
      Feature_External_Parameter_Entities : Boolean := True;
      Feature_Validation                  : Boolean := False;
      Feature_Test_Valid_Chars            : Boolean := False;
      Feature_Schema_Validation           : Boolean := True;
   end record;

   type Reader is new Sax_Reader with null record;

end Sax.Readers;
