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

--  This package provides support for Utf16 encoding of characters.
--
--  Although utf32 is simpler, it requires too much space to encode typical
--  strings. Indeed, only two bytes are currently needed to encode every
--  unicode character. Thus, utf16 was created in that purpose. Most characters
--  are encoded only on two bytes, although there is provision for a four-byte
--  encoding in some cases, using what is known as surrogate pairs.
--
--  As a result, utf16 is a variable-width encoding, and traversing a string is
--  a little bit less efficient that with Utf32.
--
--  Another inconvenient is that you can not easily search an ASCII character
--  in a string with the standard string manipulation functions, since you
--  might find the high-byte of a two-byte sequence.

with Unicode.CES.Utf32;
with Unicode.CCS;
with Unchecked_Deallocation;

package Unicode.CES.Utf16 is

   -----------
   -- Types --
   -----------

   subtype Utf16_String is String;
   type Utf16_String_Access is access Utf16_String;
   --  A UTF16-encoded string, undefined byte-order

   subtype Utf16_LE_String is Utf16_String;
   type Utf16_LE_String_Access is access Utf16_LE_String;
   --  Similar, but with little-endian byte-order

   subtype Utf16_BE_String is Utf16_String;
   --  Similar, but with big-endian byte-order

   -------------------------------------------
   -- Conversion to and from byte sequences --
   -------------------------------------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural);
   --  Return the byte sequence representing Char in the Utf16 character
   --  encoding form (little-endian)

   procedure Read
     (Str   : Utf16_LE_String;
      Index : in out Positive;
      Char  : out Unicode_Char);
   --  Return the character starting at location Index in Str
   --  Invalid_Encoding is raised if not valid byte sequence starts at Index.
   --  Incomplete_Encoding is raised if there is not enough characters for
   --  a valid encoding.

   procedure Read_BE
     (Str   : Utf16_BE_String;
      Index : in out Positive;
      Char  : out Unicode_Char);
   --  Return the character starting at location Index in Str (big-endian)

   procedure Encode_BE
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural);
   --  Return the byte sequence representing Char in the Utf16 character
   --  encoding form (big-endian)

   function Width (Char : Unicode_Char) return Natural;
   --  Return the number of bytes occupied by the Utf16 representation of Char

   function Length (Str : Utf16_String) return Natural;
   --  Return the number of characters in Str

   -------------------------------------------
   -- Conversion to and from Utf32-encoding --
   -------------------------------------------

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Utf16_LE_String;
   --  Return a new utf16-encoded string, from a Utf32-encoded string.

   function To_Utf32
     (Str : Utf16_LE_String)
      return Unicode.CES.Utf32.Utf32_LE_String;
   --  Return a new utf32-encoded string, from a Utf16-encoded string.

   ---------------------------
   -- Byte order conversion --
   ---------------------------

   function To_Unicode_LE
     (Str   : Utf16_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf16_LE_String;
   --  Convert Str to a Unicode, little-endian utf16-encoding byte sequence.
   --  Order is the order in which bytes are coded in Str. This is silently
   --  overriden in case Str as a BOM (byte-order-marker) at the beginning
   --  that specifies an explicit order.
   --  The BOM is removed from the resulting string
   --  Invalid_Encoding is raised if Str contains a BOM that indicates an
   --  encoding other than Utf16.

   function To_CS
     (Str   : Utf16_LE_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf16_String;
   --  Convert Str from Unicode to another character set, and possibly
   --  another ordering.
   --  No BOM is checked

   ---------------------
   -- Encoding Scheme --
   ---------------------

   Utf16_LE_Encoding : constant Encoding_Scheme :=
     (BOM    => Utf16_LE,
      Read   => Read'Access,
      Width  => Width'Access,
      Encode => Encode_Function'(Encode'Access),
      Length => Length'Access);

   Utf16_BE_Encoding : constant Encoding_Scheme :=
     (BOM    => Utf16_BE,
      Read   => Read_BE'Access,
      Width  => Width'Access,
      Encode => Encode_Function'(Encode_BE'Access),
      Length => Length'Access);

   ------------------
   -- Deallocation --
   ------------------

   procedure Free is new Unchecked_Deallocation
     (Utf16_String, Utf16_String_Access);
   procedure Free is new Unchecked_Deallocation
     (Utf16_LE_String, Utf16_LE_String_Access);
   --  Free the memory occupied by a utf16-encoded string

private
   pragma Inline (Width);
end Unicode.CES.Utf16;
