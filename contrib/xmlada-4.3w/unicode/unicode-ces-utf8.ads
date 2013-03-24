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

--  This package provides support for Utf8 encoding of characters.
--
--  Characters whose code is less than 128 are encoded as is in the
--  Utf8_String. As a result, such a string is compatible with a standard
--  String whose characters are all standard ASCII (and contains no
--  extended ASCII characters).
--  In that, one of the beauties of UTF-8 (and UTF-16) is that there is no
--  overlap, as opposed to what happens with other encodings. If you search
--  for an ASCII character within a Utf8_String, using the standard string
--  string or array manipulation functions, you will only find that character,
--  and not part of a longer sequence that encodes another character.
--  As a result, all the standard string-manipulation functions will work
--  as is (note however that the 'Length attribute doesn't represent the
--  number of characters in the string, but the number of bytes).
--
--  However, since characters can be encoded on one to six bytes, this means
--  that traversing a string is not as efficient as with other encodings.
--
--  Also, this encoding is not subject to byte-ordering constraints, since this
--  is only a sequence of bytes. It is self-synchronizing, in that you can
--  start anywhere in the string and find a synchronization point easily.

with Unicode.CES.Utf32;
with Unicode.CCS;
with Unchecked_Deallocation;

package Unicode.CES.Utf8 is

   -----------
   -- Types --
   -----------

   subtype Utf8_String is String;
   type Utf8_String_Access is access all Utf8_String;
   --  An UTF8-encoded string.

   -------------------------------------------
   -- Conversion to and from byte sequences --
   -------------------------------------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural);
   --  Set the byte sequence representing Char in the Utf8 character encoding.
   --  There must remain at least 6 characters in Output if you want to avoid
   --  Constraint_Errors.

   procedure Read
     (Str   : Utf8_String;
      Index : in out Positive;
      Char  : out Unicode_Char);
   --  Return the character starting at location Index in Str, and move Index
   --  to the beginning of the next location
   --  Invalid_Encoding is raised if not valid byte sequence starts at Index.
   --  Incomplete_Encoding is raised if there is not enough characters for
   --  a valid encoding.

   function Width (Char : Unicode_Char) return Natural;
   --  Return the number of bytes occupied by the Utf8 representation of Char

   function Length (Str : Utf8_String) return Natural;
   --  Return the number of characters in Str

   -------------------------------------------
   -- Conversion to and from Utf32-encoding --
   -------------------------------------------

   function From_Utf32 (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Utf8_String;
   --  Return a new Utf8-encoded string, from a Utf32-encoded string.

   function To_Utf32 (Str : Utf8_String)
      return Unicode.CES.Utf32.Utf32_LE_String;
   --  Return a new Utf32-encoded string, from a Utf8-encoded string.

   ---------------------------
   -- Byte order conversion --
   ---------------------------

   function To_Unicode_LE
     (Str   : Utf8_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf8_String;
   --  Convert str (character set is CS) to a Unicode
   --  little-endian byte-sequence
   --  If Str contains a BOM that indicates an encoding other than Utf8,
   --  Invalid_Encoding is raised.
   --  Order is irrelevant for utf8, but is kept for interface compatibility
   --  with other similar functions.

   function To_CS
     (Str   : Utf8_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf8_String;
   --  Convert Str (Unicode) to another character set

   ---------------------
   -- Encoding Scheme --
   ---------------------

   Utf8_Encoding : constant Encoding_Scheme :=
     (BOM    => Utf8_All,
      Read   => Read'Access,
      Width  => Width'Access,
      Encode => Encode_Function'(Encode'Access),
      Length => Length'Access);

   ------------------
   -- Deallocation --
   ------------------

   procedure Free is new Unchecked_Deallocation
     (Utf8_String, Utf8_String_Access);
   --  Free the memory occupied by a utf8-encoded string

private
   pragma Inline (Width);
end Unicode.CES.Utf8;
