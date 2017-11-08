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

package body Devices.Pciconf.Addrspace
with
   Refined_State => (Memory => Space)
is

   --  Read data from device PCI config space at given offset.
   generic
      type Element_Type is mod <>;
   function Read
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return Element_Type
   with
      Global => (Input => Space),
      Volatile_Function;

   --  Write data to device PCI config space at given offset.
   generic
      type Element_Type is mod <>;
   procedure Write
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : Element_Type)
   with
      Global => (In_Out => Space);

   -------------------------------------------------------------------------

   function Read
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return Element_Type
   is
      Res : Element_Type := 0;
   begin
      for I in 0 .. Element_Type'Size / 8 - 1 loop
         declare
            Val : constant SK.Byte := Space (SID) (Natural (Offset) + I);
         begin
            Res := Res or Element_Type (Val) * 2 ** (8 * I);
         end;
      end loop;

      return Res;
   end Read;

   -------------------------------------------------------------------------

   function Read_Byte_Impl   is new Read (Element_Type => SK.Byte);
   function Read_Word16_Impl is new Read (Element_Type => SK.Word16);
   function Read_Word32_Impl is new Read (Element_Type => SK.Word32);

   function Read_Byte
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return SK.Byte
      renames Read_Byte_Impl;

   function Read_Word16
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return SK.Word16
      renames Read_Word16_Impl;

   function Read_Word32
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return SK.Word32
      renames Read_Word32_Impl;

   -------------------------------------------------------------------------

   procedure Write
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : Element_Type)
   is
   begin
      for I in 0 .. Element_Type'Size / 8 - 1 loop
         declare
            Val : constant SK.Byte := SK.Byte'Mod (Value / 2 ** (8 * I));
         begin
            Space (SID) (Natural (Offset) + I) := Val;
         end;
      end loop;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Byte_Impl   is new Write (Element_Type => SK.Byte);
   procedure Write_Word16_Impl is new Write (Element_Type => SK.Word16);
   procedure Write_Word32_Impl is new Write (Element_Type => SK.Word32);

   procedure Write_Byte
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : SK.Byte)
      renames Write_Byte_Impl;

   procedure Write_Word16
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : SK.Word16)
      renames Write_Word16_Impl;

   procedure Write_Word32
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : SK.Word32)
      renames Write_Word32_Impl;

end Devices.Pciconf.Addrspace;
