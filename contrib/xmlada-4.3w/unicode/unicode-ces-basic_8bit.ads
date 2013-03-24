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

--  This package implements a basic 8bit encoding.
--  Only code points from 16#00# to 16#FF# can be encoded in such strings.
--  These are the standard Ada Strings.
--
--  However, then can be used to read files that contain accented characters,
--  in combination with Unicode.CCS.Iso_8859_1 for instance

with Unicode.CES.Utf32;
with Unicode.CCS;
with Unchecked_Deallocation;

package Unicode.CES.Basic_8bit is

   -----------
   -- Types --
   -----------

   subtype Basic_8bit_String is String;
   type Basic_8bit_String_Access is access Basic_8bit_String;
   --  A heigh bit string, undefined byte-order

   -------------------------------------------
   -- Conversion to and from byte sequences --
   -------------------------------------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural);
   --  Return the byte sequence representing Char in the 8bit character
   --  encoding form
   --  Invalid_Encoding is raised if Char can not be converted.

   procedure Read
     (Str   : Basic_8bit_String;
      Index : in out Positive;
      Char  : out Unicode_Char);
   --  Return the character starting at location Index in Str

   function Width (Char : Unicode_Char) return Natural;
   --  Return the number of bytes occupied by the 8bit representation of Char

   function Length (Str : Basic_8bit_String) return Natural;
   --  Return the number of characters in Str

   ------------------------------------------
   -- Conversion to and from 8bit-encoding --
   ------------------------------------------

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Basic_8bit_String;
   --  Return a new string, from a utf32-encoded string.

   function To_Utf32
     (Str : Basic_8bit_String)
      return Unicode.CES.Utf32.Utf32_LE_String;
   --  Return a new utf32-encoded string, from a standard Ada string.

   ---------------------------------------------
   -- Byte order and character set conversion --
   ---------------------------------------------

   function To_Unicode_LE
     (Str   : Basic_8bit_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Basic_8bit_String;
   --  Convert Str to a Unicode string, assuming it contains code points from
   --  the character set CS.
   --  Byte-order is irrelevant for 8bit strings, but is kept for interface
   --  compatibility with other similar functions

   function To_CS
     (Str   : Basic_8bit_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Basic_8bit_String;
   --  Convert Str to the character set Cs, assuming it contains Unicode
   --  characters.

   ---------------------
   -- Encoding Scheme --
   ---------------------

   Basic_8bit_Encoding : constant Encoding_Scheme :=
     (BOM    => Unknown,
      Read   => Read'Access,
      Width  => Width'Access,
      Encode => Encode_Function'(Encode'Access),
      Length => Length'Access);

   ------------------
   -- Deallocation --
   ------------------

   procedure Free is new Unchecked_Deallocation
     (Basic_8bit_String, Basic_8bit_String_Access);

private
   pragma Inline (Width);
end Unicode.CES.Basic_8bit;
