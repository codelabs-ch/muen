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

with Interfaces;

with SK;

with Musinfo.Instance;

with Config;
with Types;
with Subject_Info;

package Devices.Pciconf
with
   Abstract_State => (State with External => (Async_Readers, Async_Writers)),
   Initializes    => State
is

   --  Emulate PCI config space access at given address.
   procedure Emulate
     (GPA    :     SK.Word64;
      Info   :     Types.EPTV_Info_Type;
      Action : out Types.Subject_Action_Type)
   with
      Global => (Input  => Musinfo.Instance.State,
                 In_Out => (State, Subject_Info.State)),
      Pre    => Musinfo.Instance.Is_Valid
                  and GPA in Config.MMConf_Region;

private

   type Field_Type is new SK.Byte;

   subtype BAR_Field_Type is Field_Type range 16#10# .. 16#24#;

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
      Vwrite_Command,
      Vwrite_BAR,
      Vwrite_XUSB2PR,
      Vwrite_PSSEN);

   Read_All_Virt : constant := SK.Byte'First;
   Read_No_Virt  : constant := SK.Byte'Last;

   type Write_Perm_Type is
     (Write_Denied,
      Write_Direct,
      Write_Virt);

   --  Rule entry for a specific PCI config space field at given offset.
   --
   --  Read_Mask specifies which bits from the real hardware are directly
   --  returned and which ones are masked out. A mask of 16#ffff_0000# for
   --  example would return the real bits 31:16 and mask 15:0.
   --
   --  Vread specifies a virtual read function to emulate certain bits (which
   --  might be masked out from the real hw value by using the read mask
   --  field).
   --
   --  The Write_Perm field specifies whether a write request is denied,
   --  directly passed to hardware or virtualized.
   type Rule_Type is record
      Offset      : Field_Type;
      Read_Mask   : SK.Word32;
      Vread       : Vread_Type;
      Write_Perm  : Write_Perm_Type;
      Write_Width : Access_Width_Type;
      Vwrite      : Vwrite_Type;
   end record;

   type Rule_Array is array (Positive range <>) of Rule_Type;

   type BAR_State_Type is
     (BAR_Address,
      BAR_Size);

   type BAR_Type is record
      State   : BAR_State_Type;
      Address : SK.Word32;
      Size    : SK.Word32;
   end record;

   Null_BAR : constant BAR_Type
     := (State  => BAR_Address,
         others => 0);

   type BAR_Range is range 0 .. 5;

   type BAR_Array is array (BAR_Range) of BAR_Type;

   --  Device state of a managed device.
   type Device_Type is record
      SID              : Musinfo.SID_Type;
      MSI_Cap_Offset   : Field_Type;
      MSI_X_Cap_Offset : Field_Type;
      BARs             : BAR_Array;
      Rules            : Rule_Array (1 .. 12);
   end record;

   --  Required to avoid implicit loops. We consider devices with the same SID
   --  as equal.
   overriding
   function "=" (Left, Right : Device_Type) return Boolean
   is (Interfaces."=" (Left.SID, Right.SID));

   --  Append new rule to device state.
   procedure Append_Rule
     (Device : in out Device_Type;
      Rule   :        Rule_Type);

end Devices.Pciconf;
