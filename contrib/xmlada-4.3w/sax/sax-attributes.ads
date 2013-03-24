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

--  In addition to the SAX standard, we have added an extra field to
--  Attributes to memorize the default declaration for the attribute
--  (REQUIRED, IMPLIED, FIXED).
--  Likewise, enumerations are represented in a full structure, rather than
--  a simple string.
--  We have also merged the interfaces Attributes and Attributes_Impl, for
--  ease of use.

with Unicode.CES;
with Sax.Models;

package Sax.Attributes is

   type Attributes is tagged private;
   No_Attributes : constant Attributes;

   type Default_Declaration is (Required, Implied, Fixed, Default);
   --  See 3.3.2 in XML specifications

   type Attribute_Type is
     (Cdata, Id, Idref, Idrefs, Entity, Entities, Nmtoken, Nmtokens,
      Notation, Enumeration);
   --  See 3.3.1 in XML specifications. The last value "Enumeration"
   --  corresponds to a model like "(a|b)*",...

   --------------------------
   -- Attributes interface --
   --------------------------
   --  In the following functions, an empty string is returned when the
   --  index is out of bounds.
   --  Indexes are zero-based.

   function Get_Index
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Integer;
   function Get_Index
     (Attr       : Attributes;
      Local_Name : Unicode.CES.Byte_Sequence)  --  no namespace
      return Integer;
   --  Look up the index of an attribute by Namespace name
   --  (-1) is returned if there is no match

   function Get_Length (Attr : Attributes) return Natural;
   --  Return the number of attributes in the list

   function Get_Local_Name (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's local name by index

   function Get_Prefix (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return the prefix used for the attribute in the XML file

   function Get_Qname (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's qualified name by index

   function Get_Type (Attr : Attributes; Index : Natural)
      return Attribute_Type;
   --  Return an attribute's type by index

   function Get_Type (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Attribute_Type;
   --  Return an attribute's type by XML 1.0 qualified name

   function Get_Type
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Attribute_Type;
   --  Return an attribute's type by Namespace name, or "CDATA" if the type
   --  is unknown.

   function Get_URI (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   --  Return an attribute's Namespace URI by index

   function Get_Value (Attr : Attributes; Index : Natural)
      return Unicode.CES.Byte_Sequence;
   function Get_Value_As_Boolean
     (Attr : Attributes; Index : Natural) return Boolean;
   --  Return an attribute's value by index.
   --  The second function will test the value's attribute against the standard
   --  set of boolean values ("true", "1", "false", "0")

   function Get_Value (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;
   function Get_Value_As_Boolean
     (Attr : Attributes; Qname : Unicode.CES.Byte_Sequence) return Boolean;
   --  Return an attribute's value by XML 1.0 qualified name

   function Get_Value
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Unicode.CES.Byte_Sequence;
   function Get_Value_As_Boolean
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence)
      return Boolean;
   --  Return an attribute's value by Namespace name

   function Get_Content
     (Attr : Attributes; Index : Natural) return Sax.Models.Content_Model;
   --  Return the content model for the attribute.
   --  This function doesn't exist in the SAX 2.0 standard.
   --  If you need to keep a copy of the returned type, you must Ref it.

   procedure Set_Content
     (Attr    : Attributes;
      Index   : Natural;
      Content : Sax.Models.Content_Model);
   --  Set the content model for the attribute.
   --  Content is automatically Refed internally, so that caller is still
   --  responsible for Unref-ing any reference it owns.

   function Get_Default_Declaration
     (Attr : Attributes; Index : Natural) return Default_Declaration;
   --  Return the specification used for the default value of the attribute.
   --  This function is not part of the SAX 2.0 standard.

   procedure Add_Attribute
     (Attr       : in out Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Attribute_Type;
      Content    : Sax.Models.Content_Model;
      Value      : Unicode.CES.Byte_Sequence;
      Default_Decl : Default_Declaration := Default);
   --  Add an attribute to the end of the list.
   --  For the sake of speed, this function doesn't check if the attribute is
   --  already in the list, this is the responsability of the application.
   --  Content should be null unless Att_Type is Notation or Enumeration.
   --
   --  The counting for Content is incremented, so you are still responsible
   --  for calling Unref after this procedure.

   procedure Clear (Attr : in out Attributes);
   --  Clear the list of attributes for reuse (or to free the memory allocated
   --  for it). You should always call this procedure when you are done with
   --  the attribute list.

   procedure Remove_Attribute (Attr : in out Attributes; Index : Natural);
   --  Remove an attribute from the list, by index.

   procedure Set_Attribute
     (Attr       : in out Attributes;
      Index      : Natural;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence;
      Qname      : Unicode.CES.Byte_Sequence;
      Att_Type   : Attribute_Type;
      Content    : Sax.Models.Content_Model;
      Value      : Unicode.CES.Byte_Sequence;
      Default_Decl : Default_Declaration := Default);
   --  Set an attribute in the list.
   --  For the sake of speed, this function doesn't check if the attribute is
   --  already in the list, this is the responsability of the application.
   --  Content is Refed internally, so the caller still needs to Unref it if it
   --  owns a reference to the model

   procedure Set_Attributes
     (Attr : in out Attributes; From : Attributes'Class);
   --  Copy an entire attribute object

   procedure Set_Local_Name
     (Attr       : in out Attributes;
      Index      : Natural;
      Local_Name : Unicode.CES.Byte_Sequence);
   --   Set the local name of a specific attribute in the list

   procedure Set_Qname
     (Attr  : in out Attributes;
      Index : Natural;
      Qname : Unicode.CES.Byte_Sequence);
   --   Set the XML 1.0 qualified name of a specific attribute in the list

   procedure Set_Type
     (Attr     : in out Attributes;
      Index    : Natural;
      Att_Type : Attribute_Type);
   --   Set the type of a specific attribute in the list

   procedure Set_URI
     (Attr  : in out Attributes;
      Index : Natural;
      URI   : Unicode.CES.Byte_Sequence);
   --   Set the Namespace URI of a specific attribute in the list

   procedure Set_Value
     (Attr  : Attributes;
      Index : Natural;
      Value : Unicode.CES.Byte_Sequence);
   --   Set the value of a specific attribute in the list

   function Get_Non_Normalized_Value
     (Attr       : Attributes;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence) return Unicode.CES.Byte_Sequence;
   --  Get the value of the attribute before normalization

   Out_Of_Bounds : exception;
   --  Raised when Index is out of bounds in all the Set_* subprograms.

private

   type Attribute;
   type Attribute_Access is access Attribute;
   type Attribute is record
      URI          : Unicode.CES.Byte_Sequence_Access;
      Local_Name   : Unicode.CES.Byte_Sequence_Access;
      Value        : Unicode.CES.Byte_Sequence_Access;
      Non_Normalized_Value : Unicode.CES.Byte_Sequence_Access;
      Att_Type     : Attribute_Type;
      Qname        : Unicode.CES.Byte_Sequence_Access;
      Default_Decl : Default_Declaration;
      Content      : Sax.Models.Content_Model := Sax.Models.Unknown_Model;
      Next         : Attribute_Access;
   end record;

   type Attributes is tagged record
      Length : Natural := 0;
      First  : Attribute_Access;
      Last   : Attribute_Access;
   end record;

   No_Attributes : constant Attributes := (0, null, null);
end Sax.Attributes;
