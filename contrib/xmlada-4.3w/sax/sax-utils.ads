------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package contains various subprograms not described in the SAX
--  standard, but which are used by the various components of XML/Ada

with Unicode.CES;
with Interfaces;
with Sax.Pointers;
with Sax.Symbols;

package Sax.Utils is

   type XML_Versions is
     (XML_1_0_Third_Edition,
      XML_1_0_Fourth_Edition,
      XML_1_0_Fifth_Edition,
      XML_1_0,   --  Alias for the latest version
      XML_1_1
     );

   function Is_Valid_Language_Name
     (Lang : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Lang is a valid language, as per 2.12 in the XML specifications.
   --  Lang is encoded with Sax.Encodings.Encoding

   function Is_Valid_Name_Char
     (Char    : Unicode.Unicode_Char;
      Version : XML_Versions := XML_1_1) return Boolean;
   function Is_Valid_Name_Startchar
     (Char    : Unicode.Unicode_Char;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Char is a valid NameChar, as per 2.3 in the XML specifications

   function Is_Valid_NCname_Char
     (Char    : Unicode.Unicode_Char;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Char is a valid NCnameChar, as per 2 in the XML specifications

   function Is_Valid_Nmtoken
     (Nmtoken : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Nmtoken is valid NMTOKEN as per 2.3 in the XML specifications

   function Is_Valid_Nmtokens
     (Nmtokens : Unicode.CES.Byte_Sequence;
      Version  : XML_Versions := XML_1_1) return Boolean;
   --  Whether Nmtokens is valid NMTOKENS as per 2.3

   function Is_Valid_Name
     (Name    : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Name is valid name as per 2.3 in the XML specifications

   function Is_Valid_Names
     (Name    : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Name contains one or more valid Name, separated by a single
   --  space character.

   function Is_Valid_NCname
     (Name    : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Name is valid NCname as per 2 in the XML namespaces
   --  specifications
   --  Colon should not be allowed when namespaces are supported, since names
   --  must then match NCName, as per 6 in XML Namespaces specifications

   function Is_Valid_NCnames
     (Name    : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Name contains one or more valid NCname, separated by a single
   --  space character.

   function Is_Valid_QName
     (Name    : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Name is valid QName as per 3 in the XML specifications

   type URI_Type is (URI_Absolute, URI_Relative_Ref, URI_None);

   function Check_URI
     (Name    : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return URI_Type;
   --  Check whether Name is a URI, and its type if it is. This is RFC 3986,
   --  see http://www.ietf.org/rfc/rfc3986.txt.

   function Is_Valid_URI
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  Check whether URI is a valid absolute or relative URI

   function Is_Valid_URN
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  True if Name is a valid URN (Uniform Ressource Name) identification, as
   --  per RFC 2141.
   --  See http://www.faqs.org/rfcs/rfc2141.html

   function Is_Valid_IRI
     (Name    : Unicode.CES.Byte_Sequence;
      Version : XML_Versions := XML_1_1) return Boolean;
   --  Whether Name is a valid IRI (Internationalized Resource Identifier), as
   --  per Namespaces in XML 1.1 definition
   --  See http://www.w3.org/TR/xml-names11/#dt-IRI

   function Contains_URI_Fragment
     (Name : Unicode.CES.Byte_Sequence) return Boolean;
   --  True if Name contains a URI fragment (starting with #)

   function Is_Valid_HexBinary
     (Str  : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Str only contains valid hexadecimal digits

   function Is_Valid_Base64Binary
     (Value : Unicode.CES.Byte_Sequence) return Boolean;
   --  Whether Str only contains valid base64Binary digits

   function Hash
     (Key : Unicode.CES.Byte_Sequence) return Interfaces.Unsigned_32;
   function Hash
     (Key : Unicode.CES.Byte_Sequence_Access) return Interfaces.Unsigned_32;
   --  Hash-code used for all htables indexed on strings

   function Equal (S1, S2 : Unicode.CES.Byte_Sequence_Access) return Boolean;
   --  Compare the byte_Sequence

   function Split_Qname (Qname : Unicode.CES.Byte_Sequence) return Integer;
   --  Return an index so that:
   --     Qname (Qname'First .. Result - 1) = <prefix>
   --     Qname (Result + 1 .. Qname'Last) = <local_name>

   function Collapse_Whitespaces (Str : String) return String;
   --  Collapse whitespaces in Str, according to the attributes normalization
   --  rule

   -----------
   -- Lists --
   -----------

   generic
      with procedure Callback (Str : Unicode.CES.Byte_Sequence);
   procedure For_Each_Item (Ch : Unicode.CES.Byte_Sequence);
   --  Iterate over each element of the list

   -------------
   -- Symbols --
   -------------

   package Symbol_Table_Pointers is new Sax.Pointers.Smart_Pointers
     (Encapsulated => Sax.Symbols.Symbol_Table_Record);

   subtype Symbol_Table is Symbol_Table_Pointers.Pointer;

   No_Symbol_Table : constant Symbol_Table :=
                       Symbol_Table_Pointers.Null_Pointer;

   function Allocate return Symbol_Table;
   --  Return a new symbol table

   function Find
     (Table : Symbol_Table; Str : Unicode.CES.Byte_Sequence)
      return Sax.Symbols.Symbol;
   --  Creates a new symbol in the symbol table.

   function Convert
     (Table : Symbol_Table; Sym : Sax.Symbols.Symbol)
      return Sax.Symbols.Symbol;
   pragma Inline (Convert);
   --  Store Sym in Table (this is not needed if it was already allocated in
   --  that table, although it is harmless).

   ----------------
   -- Namespaces --
   ----------------

   type XML_NS is private;
   No_XML_NS : constant XML_NS;
   --  A namespace and its prefix in the XML file (there might be multiple
   --  prefixes for a given namespace_URI)

   function Get_Prefix (NS : XML_NS) return Sax.Symbols.Symbol;
   function Get_URI (NS : XML_NS)    return Sax.Symbols.Symbol;
   pragma Inline (Get_Prefix, Get_URI);
   --  Return the URI for this namespace

   procedure Set_System_Id (NS : XML_NS; System_Id : Sax.Symbols.Symbol);
   function Get_System_Id  (NS : XML_NS) return Sax.Symbols.Symbol;
   --  Return the location of the file or stream used associated with that
   --  namespace

   procedure Increment_Count (NS : XML_NS);
   function Element_Count (NS : XML_NS) return Natural;
   --  Return the count of elements (or attributes) seen so far in this
   --  namespace. This does not include the count of uses in the current
   --  context (that is for the <element> we are currently parsing or its
   --  attributes).

   function Next_In_List (NS : XML_NS) return XML_NS;
   --  Return the next namespace in the list

   procedure Free (NS : in out XML_NS);
   --  Free NS and its successors in the list

   function Find_NS_In_List
     (List   : XML_NS;
      Prefix : Sax.Symbols.Symbol;
      Include_Default_NS : Boolean := True;
      List_Is_From_Element : Boolean) return XML_NS;
   function Find_NS_From_URI_In_List
     (List : XML_NS; URI : Sax.Symbols.Symbol) return XML_NS;
   --  Find in List the first matching the prefix.
   --  If Include_Default_NS is False, this will not return the "" namespace

   procedure Add_NS_To_List
     (List               : in out XML_NS;
      Same_As            : XML_NS := No_XML_NS;
      Prefix, URI        : Sax.Symbols.Symbol);
   --  Add a new namespace to the list (which might be empty to start with)

private

   type XML_NS_Record;
   type XML_NS is access XML_NS_Record;
   No_XML_NS : constant XML_NS := null;

   type XML_NS_Record is record
      Prefix    : Sax.Symbols.Symbol;
      URI       : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      System_Id : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Same_As   : XML_NS;  --  If set, URI is null
      Use_Count : Natural := 0;
      Next      : XML_NS;
   end record;
   --  Same_As points to the first prefix referencing the same namespace.
   --  A namespace must be freed before the ones it references (or you will get
   --  a Storage_Error).
   --  Use_Count will always be 0 if Same_As is not null, since the uses are
   --  incremented in only one namespace.

end Sax.Utils;
