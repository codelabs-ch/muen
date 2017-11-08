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

with System;

with Musinfo;

with Config;

private package Devices.Pciconf.Addrspace
with
   Abstract_State => (Memory with Part_Of  => Pciconf.State,
                                  External => (Async_Readers, Async_Writers))
is

   --  Read byte from device PCI config space at given offset.
   function Read_Byte
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return SK.Byte
   with
      Global => (Input => Memory),
      Volatile_Function;

   --  Read 16-bit word from device PCI config space at given offset.
   function Read_Word16
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return SK.Word16
   with
      Global => (Input => Memory),
      Volatile_Function;

   --  Read 32-bit word from device PCI config space at given offset.
   function Read_Word32
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type)
      return SK.Word32
   with
      Global => (Input => Memory),
      Volatile_Function;

   --  Write byte to device PCI config space at given offset.
   procedure Write_Byte
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : SK.Byte)
   with
      Global => (In_Out => Memory);

   --  Write 16-bit word to device PCI config space at given offset.
   procedure Write_Word16
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : SK.Word16)
   with
      Global => (In_Out => Memory);

   --  Write 32-bit word to device PCI config space at given offset.
   procedure Write_Word32
     (SID    : Musinfo.SID_Type;
      Offset : Field_Type;
      Value  : SK.Word32)
   with
      Global => (In_Out => Memory);

private

   type Device_Space_Type is array (0 .. SK.Page_Size - 1) of SK.Byte;

   type Addrspace_Type is array (Musinfo.SID_Type) of Device_Space_Type
   with
      Component_Size => SK.Page_Size * 8,
      Pack;

   Space : Addrspace_Type
   with
      Import,
      Volatile,
      Async_Readers,
      Async_Writers,
      Part_Of => Memory,
      Address => System'To_Address (Config.MMConf_Base_Address),
      Size    => Interfaces."*" (Config.MMConf_Size, 8);

end Devices.Pciconf.Addrspace;
