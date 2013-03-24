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

--  This package provides support for Utf32-encoding.
--
--  The main advantage to this encoding is that each character is coded on
--  the same number of bytes, ie 4 bytes. It is thus very easy and fast to
--  traverse a byte sequence and get each character.
--
--  On the other hand, this also means that strings are much bigger than what
--  they should be (when using standard ASCII character, for instance, the
--  byte sequence is four times as big as it needs to be).
--  This encoding is also dependent on specific byte-ordering. You should thus
--  always convert your Utf32 strings to little-endian before usage (see
--  To_Utf32_LE below).

with Unchecked_Deallocation;
with Unicode.CCS;

package Unicode.CES.Utf32 is

   -----------
   -- Types --
   -----------

   subtype Utf32_String is String;
   type Utf32_String_Access is access Utf32_String;
   --  A UTF32-encoded string. Byte-order is unspecified

   subtype Utf32_LE_String is Utf32_String;
   type Utf32_LE_String_Access is access Utf32_LE_String;
   --  A Utf32-encoded, little-endian string.

   subtype Utf32_BE_String is Utf32_String;
   --  A Utf32-encoded, big-endian string.

   Utf32_Char_Width : constant := 4;
   --  Number of bytes used to represent every character in Utf32

   -------------------------------------------
   -- Conversion to and from byte sequences --
   -------------------------------------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural);
   --  Return the byte sequence representing Char in the Utf32 character
   --  encoding form.
   --  The character is encoded in little-endian byte order.
   --  Output must have at least Utf32_Char_Width characters available.

   procedure Read
     (Str   : Utf32_LE_String;
      Index : in out Positive;
      Char  : out Unicode_Char);
   --  Return the character starting at location Index in Str
   --  Invalid_Encoding is raised if not valid byte sequence starts at Index.
   --  Incomplete_Encoding is raised if there is not enough characters for
   --  a valid encoding.

   procedure Encode_BE
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural);
   --  Return the byte sequence representing Char in the Utf32 character
   --  encoding form.
   --  The character is encoded in big-endian byte order.

   procedure Read_BE
     (Str   : Utf32_BE_String;
      Index : in out Positive;
      Char  : out Unicode_Char);
   --  Same as Read, but when Str is in big-endian order

   function Width (Char : Unicode_Char) return Natural;
   --  Return the number of bytes occupied by the Utf32 representation of Char

   function Length (Str : Utf32_String) return Natural;
   --  Return the number of characters in Str

   ---------------------------
   -- Byte order conversion --
   ---------------------------

   function To_Unicode_LE
     (Str   : Utf32_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf32_LE_String;
   --  Convert a string from any byte-order, any character set (CS) to
   --  Unicode little-endian byte sequence
   --  Order is the order in which bytes are coded in Str. This is silently
   --  overriden in case Str has a BOM (byte-order-marker) at the beginning
   --  that specifies an explicit order.
   --  The BOM is removed from the resulting string
   --  Invalid_Encoding is raised if there is a BOM that indicates an
   --  encoding other than Utf32.

   function To_CS
     (Str   : Utf32_LE_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf32_String;
   --  Convert a Unicode, little-endian string to a string with any byte-order
   --  and a new character set.

   ---------------------
   -- Encoding Scheme --
   ---------------------

   Utf32_LE_Encoding : constant Encoding_Scheme :=
     (BOM    => Utf32_LE,
      Read   => Read'Access,
      Width  => Width'Access,
      Encode => Encode_Function'(Encode'Access),
      Length => Length'Access);

   Utf32_BE_Encoding : constant Encoding_Scheme :=
     (BOM    => Utf32_BE,
      Read   => Read_BE'Access,
      Width  => Width'Access,
      Encode => Encode_Function'(Encode_BE'Access),
      Length => Length'Access);

   ------------------
   -- Deallocation --
   ------------------

   procedure Free is new Unchecked_Deallocation
     (Utf32_String, Utf32_String_Access);
   procedure Free is new Unchecked_Deallocation
     (Utf32_LE_String, Utf32_LE_String_Access);
   --  Free the memory occupied by a utf32-encoded string

private
   pragma Inline (Width);
end Unicode.CES.Utf32;
