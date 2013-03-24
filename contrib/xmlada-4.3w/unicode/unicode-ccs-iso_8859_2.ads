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

--  This package provides support for the ISO/8859-2 (aka Latin-2)
--  encoding.
--  The Latin 2 character set supports the Slavic languages of Central Europe
--  which use the Latin alphabet. The ISO-8859-2 set is used for the following
--  languages: Czech, Croat, German, Hungarian, Polish, Romanian, Slovak and
--  Slovenian.

package Unicode.CCS.Iso_8859_2 is

   Name1 : aliased constant String := "ISO-8859-2";
   Name2 : aliased constant String := "Latin2";

   function To_Unicode    (Char : Unicode_Char) return Unicode_Char;
   function To_Iso_8859_2 (Char : Unicode_Char) return Unicode_Char;

   Iso_8859_2_Character_Set : constant Character_Set :=
     (To_Unicode => To_Unicode'Access,
      To_CS      => To_Iso_8859_2'Access);
end Unicode.CCS.Iso_8859_2;
