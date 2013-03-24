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

with Ada.Exceptions; use Ada.Exceptions;

package body Unicode.CCS.Iso_8859_1 is

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char > 16#00FF# then
         Raise_Exception
           (Invalid_Code'Identity,
            "code " & Unicode_Char'Image (Char)
            & " is not available in Iso/8859-1");
      else
         return Char;
      end if;
   end To_Unicode;

   -------------------
   -- To_Iso_8859_1 --
   -------------------

   function To_Iso_8859_1 (Char : Unicode_Char) return Unicode_Char
      renames To_Unicode;

end Unicode.CCS.Iso_8859_1;
