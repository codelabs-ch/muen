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

with SK;

with Types;

package Devices.Pciconf
with
   Abstract_State => State
is

   --  Init PCI config space emulation.
   procedure Init (Device_Base : SK.Word64)
   with
     Global => (Output => State);

   --  Mediate PCI config space access.
   procedure Mediate
     (Info   :     Types.EPTV_Info_Type;
      Action : out Types.Subject_Action_Type)
   with
      Global => (Input => State);

private

   type Field_Type is new SK.Byte;

   type Access_Width_Type is
     (Access_8,
      Access_16,
      Access_32);

   --  Virtual read functions.
   type Vread_Type is
     (Vread_None,
      Vread_Cap_Pointer,
      Vread_BAR,
      Vread_MSI_Cap_ID_Next);

   --  Virtual write functions.
   type Vwrite_Type is
     (Vwrite_None,
      Vwrite_BAR);

   All_Virt : constant := SK.Byte'First;
   No_Virt  : constant := SK.Byte'Last;

   --  Config entry for a specific PCI config space field at given offset.
   --
   --  Read_Mask specifies which bits from the real hardware are directly
   --  returned and which ones are masked out. A mask of 16#ffff_0000# for
   --  example would return the real bits 31:16 and mask 15:0.
   --
   --  Write_Mask specifies which bits are directly written into the hardware
   --  field at given offset. Masked bits are first read from the real hardware
   --  value and then merged with the request before writing. If Write_Mask is
   --  All_Virt, the write request is ignored.
   --
   --  Vread specifies a virtual read function to emulate certain bits (which
   --  might be masked out from the real hw value by using the read mask
   --  field).
   type Config_Entry_Type is record
      Offset      : Field_Type;
      Read_Mask   : SK.Word32;
      Vread       : Vread_Type;
      Write_Mask  : SK.Word32;
      Write_Width : Access_Width_Type;
      Vwrite      : Vwrite_Type;
   end record;

   --  Append new config entry.
   procedure Append_Config (C : Config_Entry_Type);

end Devices.Pciconf;
