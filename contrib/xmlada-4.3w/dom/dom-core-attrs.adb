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

package body DOM.Core.Attrs is

   ---------------
   -- Specified --
   ---------------

   function Specified (Att : Attr) return Boolean is
   begin
      return Att.Specified;
   end Specified;

   -------------------
   -- Owner_Element --
   -------------------

   function Owner_Element (Att : Attr) return Element is
   begin
      if Att.Owner_Element.Node_Type = Element_Node then
         return Att.Owner_Element;
      else
         return null;
      end if;
   end Owner_Element;

   -----------
   -- Is_Id --
   -----------

   function Is_Id (Att : Attr) return Boolean is
   begin
      return Att.Is_Id;
   end Is_Id;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Att : Attr; Value : Sax.Symbols.Symbol) is
   begin
      Att.Attr_Value := Value;
      Att.Specified := True;
   end Set_Value;

end DOM.Core.Attrs;
