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

with Unicode.CES; use Unicode.CES;

package body DOM.Core.Notations is

   ---------------
   -- Public_Id --
   ---------------

   function Public_Id (N : Notation) return DOM_String is
   begin
      if N.Public_ID = null then
         return "";
      else
         return N.Public_ID.all;
      end if;
   end Public_Id;

   ---------------
   -- System_Id --
   ---------------

   function System_Id (N : Notation) return DOM_String is
   begin
      if N.System_ID = null then
         return "";
      else
         return N.System_ID.all;
      end if;
   end System_Id;
end DOM.Core.Notations;
