------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Unicode.CCS;
with Unicode.CES;

--  This package groups a character set and an encoding scheme under names
--  assigned by the Internet Assigned Numbers Authority.
--  See http://www.iana.org/assignments/character-sets
--  These names are used in the <?xml encoding="..." ?> part of XML
--  documents.

package Unicode.Encodings is

   type Cst_String_Access is access constant String;

   type Unicode_Encoding is record
      Name            : Cst_String_Access;
      Character_Set   : Unicode.CCS.Character_Set;
      Encoding_Scheme : Unicode.CES.Encoding_Scheme;
   end record;

   function Get_By_Name (Name : String) return Unicode_Encoding;
   --  Return the unicode encoding from its name.
   --  Name is case insensitive

   function Convert
     (Str  : Unicode.CES.Byte_Sequence;
      From : Unicode_Encoding := Get_By_Name ("iso-8859-15");
      To   : Unicode_Encoding := Get_By_Name ("utf-8"))
      return Unicode.CES.Byte_Sequence;
   --  Convert a string between two encodings.

end Unicode.Encodings;
