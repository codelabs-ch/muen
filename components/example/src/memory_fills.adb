--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with System;
with Interfaces;

with Example_Component.Memory;

with SK.Strings;

with Log;

package body Memory_Fills
is

   use type Interfaces.Unsigned_64;

   package Cspecs renames Example_Component.Memory;

   subtype Region_Range is Interfaces.Unsigned_64 range
     0 .. Cspecs.Filled_Region_Size - 1;

   type Fill_Region_Type is array (Region_Range) of Interfaces.Unsigned_8
   with
      Size        => SK.Page_Size * 8,
      Object_Size => SK.Page_Size * 8;

   Fill_Pattern : constant Interfaces.Unsigned_8 := 16#5a#;

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Memory_Region : constant Fill_Region_Type
   with
      Import,
      Address => System'To_Address (Cspecs.Filled_Region_Address);
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   -------------------------------------------------------------------------

   procedure Validate_Region_Content
   is
      use type Interfaces.Unsigned_8;
   begin
      for I in Memory_Region'Range loop
         if Memory_Region (I) /= Fill_Pattern then
            Log.Put_Line
              (Item => "Filled memory region contains unexpected pattern "
               & SK.Strings.Img (Memory_Region (I)) & " @ " & SK.Strings.Img
               (Cspecs.Filled_Region_Address + I));
            return;
         end if;
      end loop;
      Log.Put_Line (Item => "Filled memory region contains expected pattern");
   end Validate_Region_Content;

end Memory_Fills;
