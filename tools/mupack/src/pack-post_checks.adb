--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Streams;

with Mutools.Image;

package body Pack.Post_Checks
is

   Mboot_Magic : constant Ada.Streams.Stream_Element_Array (1 .. 4)
     := (16#02#, 16#b0#, 16#ad#, 16#1b#);

   -------------------------------------------------------------------------

   procedure Clear renames Check_Procs.Clear;

   -------------------------------------------------------------------------

   function Get_Count return Natural renames Check_Procs.Get_Count;

   -------------------------------------------------------------------------

   procedure Multiboot_Header (Data : Content_Providers.Param_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type Ada.Streams.Stream_Element_Array;

      --  Skip first MiB, contains only 0s.
      First_8k : constant Ada.Streams.Stream_Element_Array
        := Mutools.Image.Get_Buffer (Image   => Data.Image,
                                     Address => 16#100000#,
                                     Size    => 16#2000#);
   begin
      for I in First_8k'First .. First_8k'Last - (Mboot_Magic'Length - 1) loop
         if First_8k (I .. I + Mboot_Magic'Length - 1) = Mboot_Magic then
            return;
         end if;
      end loop;

      raise Check_Error with "No Multiboot header found in system image";
   end Multiboot_Header;

   -------------------------------------------------------------------------

   procedure Register_All
   is
   begin
      Check_Procs.Register (Process => Multiboot_Header'Access);
   end Register_All;

   -------------------------------------------------------------------------

   procedure Run (Data : Content_Providers.Param_Type) renames Check_Procs.Run;

end Pack.Post_Checks;
