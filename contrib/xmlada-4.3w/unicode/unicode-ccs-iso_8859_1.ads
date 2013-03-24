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

--  This package provides support for the ISO/8859-1 (aka Latin-1)
--  encoding.
--  The ISO-8859-1 character set, often simply referred to as Latin 1, can
--  represent most Western European languages including: Albanian, Catalan,
--  Danish, Dutch, English, Faroese, Finnish, French, Galician, German, Irish,
--  Icelandic, Italian, Norwegian, Portuguese, Spanish and Swedish.

package Unicode.CCS.Iso_8859_1 is

   Name1 : aliased constant String := "ISO-8859-1";
   Name2 : aliased constant String := "Latin1";

   function To_Unicode    (Char : Unicode_Char) return Unicode_Char;
   function To_Iso_8859_1 (Char : Unicode_Char) return Unicode_Char;

   Iso_8859_1_Character_Set : constant Character_Set :=
     (To_Unicode => To_Unicode'Access,
      To_CS      => To_Iso_8859_1'Access);
end Unicode.CCS.Iso_8859_1;
