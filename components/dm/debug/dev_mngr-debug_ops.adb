--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Strings;
with SK.Bitops;

package body Dev_Mngr.Debug_Ops
is

   --  Max. writable bit position for each access width. Used in PCI config
   --  space write path to emit warnings.
   Max_Write_Widths : constant array (0 .. 2) of SK.Byte
     := (0 => 7, 1 => 15, 2 => 31);

   procedure Find_Highest_Bit_Set is new SK.Bitops.Find_Highest_Bit_Set
     (Search_Range => SK.Bitops.Word32_Pos);

   -------------------------------------------------------------------------

   procedure Check_Warn_PCI_Write_Width
     (Value     : SK.Word32;
      Width_Idx : Natural)
   is
      use type SK.Byte;

      High_Bit : SK.Bitops.Word64_Pos;
      Found    : Boolean;
   begin
      Find_Highest_Bit_Set
        (Field => SK.Word64 (Value),
         Found => Found,
         Pos   => High_Bit);
      if Found and then SK.Byte (High_Bit) > Max_Write_Widths (Width_Idx)
      then
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Pciconf: WARNING request to write bit position "
               & SK.Strings.Img (SK.Byte (High_Bit))
               & " instead of allowed max "
               & SK.Strings.Img (Max_Write_Widths (Width_Idx))));
      end if;
   end Check_Warn_PCI_Write_Width;

end Dev_Mngr.Debug_Ops;
