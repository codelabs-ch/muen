--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

package body Terminals
is

   --  Virtual text console framebuffer.
   type Framebuffer_Type is array (1 .. SK.Page_Size) of SK.Byte;
   for Framebuffer_Type'Size use 8 * SK.Page_Size;

   Framebuffers : array (Slot_Range) of Framebuffer_Type;
   for Framebuffers'Address use System'To_Address (16#10000#);
   for Framebuffers'Size use Slot_Range'Last * 8 * SK.Page_Size;

   --  VGA output page.
   VGA_Out : Framebuffer_Type;
   for VGA_Out'Address use System'To_Address (16#000b_8000#);

   Active_Slot : Slot_Range := Slot_Range'First;
   pragma Atomic (Active_Slot);

   -------------------------------------------------------------------------

   function Get_Active_Slot return Slot_Range
   is
   begin
      return Active_Slot;
   end Get_Active_Slot;

   -------------------------------------------------------------------------

   procedure Run
   is
      use type SK.Byte;
   begin
      loop
         for I in VGA_Out'Range loop
            if VGA_Out (I) /= Framebuffers (Active_Slot) (I) then
               VGA_Out (I) := Framebuffers (Active_Slot) (I);
            end if;
         end loop;
      end loop;
   end Run;

   -------------------------------------------------------------------------

   procedure Set (Slot : Slot_Range)
   is
   begin
      Active_Slot := Slot;
   end Set;

end Terminals;
