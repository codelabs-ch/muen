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

with Unicode;
with Unicode.CES;

package Input_Sources.Strings is

   type String_Input is new Input_Source with private;
   type String_Input_Access is access all String_Input'Class;
   --  A special implementation of a reader, that reads from a unicode
   --  byte sequence.

   procedure Open
     (Str      : Unicode.CES.Cst_Byte_Sequence_Access;
      Encoding : Unicode.CES.Encoding_Scheme;
      Input    : out String_Input);
   --  Creates a new reader that reads from a byte-sequence, encoded with a
   --  specific encoding.
   --  No copy of Str is kept, we simply keep the pointer.

   procedure Open
     (Str      : Unicode.CES.Byte_Sequence;
      Encoding : Unicode.CES.Encoding_Scheme;
      Input    : out String_Input);
   --  Same as above, but a copy of the byte sequence is kept internally

   procedure Close (Input : in out String_Input);
   --  Free the memory occupied by Input

   procedure Next_Char
     (From : in out String_Input;
      C    : out Unicode.Unicode_Char);
   --  Return the next character in the string.

   function Eof (From : String_Input) return Boolean;
   --  True if From is past the last character in the string.

private
   type String_Input is new Input_Source with
      record
         Index    : Natural;
         Buffer   : Unicode.CES.Cst_Byte_Sequence_Access;

         Buffer2  : Unicode.CES.Byte_Sequence_Access;
         --  Only set if allocated in this package (second form of [Open])

         Encoding : Unicode.CES.Encoding_Scheme;
      end record;
end Input_Sources.Strings;
