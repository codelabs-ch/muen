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

--  This is the root of the hierarchy that provides different encoding
--  schemes.
--  Each of the child package provides function to convert to and from
--  Utf32-encoded string, which thus acts as the central encoding scheme.
--  In some cases, the string can be preceded by a BOM (byte-order mark),
--  that indicates the encoding used and the byte-order used for the
--  encoding. This BOM is always optional, and can never be mixed up with
--  other characters.

with Unchecked_Deallocation;

package Unicode.CES is

   -------------------
   -- Byte sequence --
   -------------------

   subtype Byte_Sequence is String;
   type Byte_Sequence_Access is access all Byte_Sequence;
   type Cst_Byte_Sequence_Access is access constant Byte_Sequence;
   --  A sequence of bytes. The encoding is unknown.

   procedure Free is new Unchecked_Deallocation
     (Byte_Sequence, Byte_Sequence_Access);

   -------------------------
   -- Byte order handling --
   -------------------------

   type Byte_Order is (High_Byte_First, Low_Byte_First);
   --  Order of bytes in word machines.

   Default_Byte_Order : constant Byte_Order := Low_Byte_First;

   ------------------------------
   -- Byte-order mark handling --
   ------------------------------

   type Bom_Type is
     (Utf8_All,  --  Utf8-encoding
      Utf16_LE,  --  Utf16 little-endian encoding
      Utf16_BE,  --  Utf16 big-endian encoding
      Utf32_LE,  --  Utf32 little-endian encoding
      Utf32_BE,  --  Utf32 big-endian encoding
      Ucs4_BE,   --  UCS-4, big endian machine (1234 order)
      Ucs4_LE,   --  UCS-4, little endian machine (4321 order)
      Ucs4_2143, --  UCS-4, unusual byte order (2143 order)
      Ucs4_3412, --  UCS-4, unusual byte order (3412 order)
      Unknown);  --  Unknown, assumed to be ASCII compatible
   --  the type of encoding used for a string, that can be deduced from the
   --  BOM.

   subtype Bom_Type_Utf16 is Bom_Type range Utf16_LE .. Utf16_BE;
   subtype Bom_Type_Utf32 is Bom_Type range Utf32_LE .. Utf32_BE;

   procedure Read_Bom
     (Str : String;
      Len : out Natural;
      BOM : out Bom_Type;
      XML_Support : Boolean := True);
   --  Read the optional Byte-Order-Mark at the beginning of the byte
   --  sequence Str.
   --  Len will contain the number of characters that made up that BOM, and
   --  that should be ignored when reading Str.
   --  If XML_Support is True, then the first four bytes of Str are also
   --  checked to recognize "<?xml", and thus distinguish in case there is no
   --  Byte-Order-Mark strictly speaking.

   function Write_Bom (BOM : Bom_Type) return String;
   --  Return a string suitable to output the BOM (Byte-order-mark). This
   --  should be added at the beginning of an output string, although this is
   --  not mandatory

   -----------------------
   -- Parsing functions --
   -----------------------

   --  All the packages in this hierarchy must provide at least two functions
   --  to read from a string (whatever its encoding).
   --
   --  These can be used to completly parse a string.
   --      J := Str'First;
   --      while J <= Str'Last loop
   --          C := Read (Str, J);    --   Read the character
   --          J := J + Width (C);    --   Move to the next
   --      end loop;

   type Read_Function is access
     procedure (Str   : Byte_Sequence;
                Index : in out Positive;
                Char  : out Unicode_Char);
   --  This function returns the character at position Index in the byte
   --  sequence Str, and moves Index to the start of the next character.
   --  If Str doesn't contain enough bytes for a valid encoding of a character,
   --  Incomplete_Encoding is raised.
   --  If Str contains an invalid byte sequence at Index, Invalid_Encoding
   --  is raised.

   type Width_Function is access
     function (Char : Unicode.Unicode_Char) return Natural;
   --  This function returns the number of bytes that encode Char in the
   --  specific encoding scheme.

   type Encode_Function is access
     procedure (Char   : Unicode_Char;
                Output : in out Byte_Sequence;
                Index  : in out Natural);
   --  This function converts Char to the appropriate byte sequence that
   --  represents it in the specific encoding.
   --  The byte sequence is stored in Output, starting at Index + 1. On exit,
   --  Index is left on the last character set in Output.

   type Length_Function is access
     function (Str : Byte_Sequence) return Natural;

   type Encoding_Scheme is record
      BOM             : Bom_Type;
      Read            : Read_Function;
      Width           : Width_Function;
      Encode          : Encode_Function;
      Length          : Length_Function;
   end record;
   --  BOM is the byte-order mark to write when first writting a file with this
   --  encoding

   --------------------
   -- Misc functions --
   --------------------

   function Index_From_Offset
     (Str : Byte_Sequence; Offset : Natural; Encoding : Encoding_Scheme)
      return Integer;
   --  return the index in Str starting at Offset.
   --  (-1) is returned if Offset is invalid (outside of the range of Str).

   ----------------
   -- Exceptions --
   ----------------

   Invalid_Encoding : exception;
   --  Raised whenever the byte sequence associated with a given encoding
   --  scheme is not valid.

   Incomplete_Encoding : exception;
   --  Raised whenever the byte sequence associated with a given encoding
   --  scheme is incomplete.

end Unicode.CES;
