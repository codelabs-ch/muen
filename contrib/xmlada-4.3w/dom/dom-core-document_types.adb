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

package body DOM.Core.Document_Types is

   function Create_Document_Type
     (Implementation  : DOM_Implementation;
      Qualified_Name  : DOM_String;
      Public_Id       : DOM_String;
      System_Id       : DOM_String;
      Internal_Subset : DOM_String) return Document_Type
   is
      pragma Warnings (Off, Implementation);
      pragma Warnings (Off, Qualified_Name);
      pragma Warnings (Off, Public_Id);
      pragma Warnings (Off, System_Id);
      pragma Warnings (Off, Internal_Subset);
   begin
      return null;
   end Create_Document_Type;
end DOM.Core.Document_Types;
