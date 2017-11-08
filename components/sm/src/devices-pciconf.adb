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

with SK.Bitops;
with SK.Strings;

with Musinfo.Instance;

with Config;
with Debug_Ops;
with Devices.Pciconf.Quirks;
with Devices.Pciconf.Addrspace;
with Devices.Pciconf.Field_Access;

package body Devices.Pciconf
with
   SPARK_Mode => Off -- Q725-014
is

   package SI renames Subject_Info;
   package FA renames Field_Access;

   use type SK.Byte;

   --  See PCI Local Bus Specification Revision 3.0, section 6.1.
   Field_Vendor          : constant := 16#00#;
   Field_Device          : constant := 16#02#;
   Field_Command         : constant := 16#04#;
   Field_Cache_Line_Size : constant := 16#0c#;
   Field_Latency_Timer   : constant := 16#0d#;
   Field_Header          : constant := 16#0e#;
   Field_BIST            : constant := 16#0f#;
   Field_Revision_Class  : constant := 16#08#;
   Field_BAR0            : constant := 16#10#;
   Field_BAR1            : constant := 16#14#;
   Field_BAR2            : constant := 16#18#;
   Field_BAR3            : constant := 16#1c#;
   Field_BAR4            : constant := 16#20#;
   Field_BAR5            : constant := 16#24#;
   Field_Cap_Pointer     : constant := 16#34#;

   Field_MSI_Ctrl : constant := 16#02#;

   MSI_Cap_ID   : constant := 16#05#;
   MSI_X_Cap_ID : constant := 16#11#;

   No_Cap : constant := SK.Byte'Last;

   Null_Rule : constant Rule_Type
     := (Offset      => Field_Type'Last,
         Read_Mask   => Read_All_Virt,
         Vread       => Vread_None,
         Write_Perm  => Write_Denied,
         Write_Width => Access_8,
         Vwrite      => Vwrite_None);

   Global_Rules : constant Rule_Array
     := (1      => (Offset      => Field_Command,
                    Read_Mask   => Read_No_Virt,
                    Vread       => Vread_None,
                    Write_Perm  => Write_Virt,
                    Write_Width => Access_16,
                    Vwrite      => Vwrite_Command),
         2      => (Offset      => Field_Cache_Line_Size,
                    Read_Mask   => Read_No_Virt,
                    Vread       => Vread_None,
                    Write_Perm  => Write_Direct,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None),
         3      => (Offset      => Field_Latency_Timer,
                    Read_Mask   => Read_No_Virt,
                    Vread       => Vread_None,
                    Write_Perm  => Write_Direct,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None),
         4      => (Offset      => Field_BIST,
                    Read_Mask   => Read_No_Virt,
                    Vread       => Vread_None,
                    Write_Perm  => Write_Direct,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None),
         5      => (Offset      => Field_BAR0,
                    Read_Mask   => Read_All_Virt,
                    Vread       => Vread_BAR,
                    Write_Perm  => Write_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         6      => (Offset      => Field_BAR1,
                    Read_Mask   => Read_All_Virt,
                    Vread       => Vread_BAR,
                    Write_Perm  => Write_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         7      => (Offset      => Field_BAR2,
                    Read_Mask   => Read_All_Virt,
                    Vread       => Vread_BAR,
                    Write_Perm  => Write_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         8      => (Offset      => Field_BAR3,
                    Read_Mask   => Read_All_Virt,
                    Vread       => Vread_BAR,
                    Write_Perm  => Write_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         9      => (Offset      => Field_BAR4,
                    Read_Mask   => Read_All_Virt,
                    Vread       => Vread_BAR,
                    Write_Perm  => Write_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         10     => (Offset      => Field_BAR5,
                    Read_Mask   => Read_All_Virt,
                    Vread       => Vread_BAR,
                    Write_Perm  => Write_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         11     => (Offset      => Field_Cap_Pointer,
                    Read_Mask   => Read_All_Virt,
                    Vread       => Vread_Cap_Pointer,
                    Write_Perm  => Write_Denied,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None));

   subtype Read_Idx_Type is SK.Byte range 0 .. 3;

   --  Since EPT faults do not provide the actual width of the access, we need
   --  to calculate it from the offset into the PCI config space of a device
   --  (i.e. Offset mod 4, see the PCI Local Bus Specification, Revision 3.0,
   --  Figure 6-1, Type 00h Configuration Space Header).
   Read_Widths : constant array (Read_Idx_Type) of Access_Width_Type
     := (0 => Access_32,
         1 => Access_8,
         2 => Access_16,
         3 => Access_8);

   Null_Device : constant Device_Type
     := (SID              => Musinfo.Null_SID,
         Base_Address     => 0,
         MSI_Cap_Offset   => No_Cap,
         MSI_X_Cap_Offset => No_Cap,
         BARs             => (others => Null_BAR),
         Rules            => (others => Null_Rule));

   type Device_Array is array (1 .. 4) of Device_Type;

   Null_Devices : constant Device_Array := (others => Null_Device);

   Device_DB : Device_Array := Null_Devices;

   --  Init PCI config space emulation for given device.
   procedure Init
     (Device : out Device_Type;
      SID    :     Musinfo.SID_Type;
      Base   :     SK.Word64)
   with
      Global => (Output => State);

   --  Return device for given SID. If no device with the specified SID is
   --  present, Null_Device is returned.
   function Get_Device (SID : Musinfo.SID_Type) return Device_Type;

   --  Add given device to devices DB. If a device with the same SID exists,
   --  the entry is replaced.
   procedure Insert_Device (Device : Device_Type);

   --  Get rule for given device and offset. The global rules are searched
   --  first, if no match is found the per-device rules are consulted.
   function Get_Rule
     (Offset : Field_Type;
      Device : Device_Type)
      return Rule_Type
   with
      Global => (Input => Global_Rules);

   --  Append per-device rules for MSI/MSI-X fields starting at given offset
   --  with specified feature flags (64-bit or maskable). See PCI specification
   --  3.0, sections 6.8.1/6.8.2.
   procedure Append_MSI_Rules
     (Device : in out Device_Type;
      Offset :        Field_Type;
      Cap_ID :        SK.Byte;
      Flags  :        SK.Byte)
   with
      Global => (In_Out => Global_Rules);

   --  Perform virtualized read operation for specified device at given offset.
   function Vread
     (Device    : Device_Type;
      Operation : Vread_Type;
      Offset    : Field_Type)
      return SK.Word64;

   --  Return virtualized capability pointer value of given device.
   function Read_Cap_Pointer (Device : Device_Type) return Field_Type;

   --  Return virtualized MSI cap ID and next pointer.
   function Read_MSI_Cap_ID_Next
     (Device : Device_Type;
      Offset : Field_Type)
      return SK.Word16;

   --  Return virtualized BAR value for specified device at given offset.
   function Read_BAR
     (Device : Device_Type;
      Offset : Field_Type)
      return SK.Word32;

   --  Perform virtualized write operation for specified device, offset and
   --  value.
   procedure Vwrite
     (Device    : Device_Type;
      Operation : Vwrite_Type;
      Offset    : Field_Type;
      Value     : SK.Word32);

   --  Write value to BAR at given offset.
   procedure Write_BAR
     (Device : Device_Type;
      Offset : Field_Type;
      Value  : SK.Word32);

   --  Write given value to command register.
   procedure Write_Command
     (Base  : SK.Word64;
      Value : SK.Word16);

   -------------------------------------------------------------------------

   function Read_Config (GPA : SK.Word64) return Element_Type
   is
      Val : Element_Type
      with
         Import,
         Address => System'To_Address (GPA);
   begin
      return Val;
   end Read_Config;

   -------------------------------------------------------------------------

   procedure Write_Config
     (GPA   : SK.Word64;
      Value : Element_Type)
   is
      Val : Element_Type
      with
         Import,
         Address => System'To_Address (GPA);
   begin
      Val := Value;
   end Write_Config;

   -------------------------------------------------------------------------

   procedure Append_Rule
     (Device : in out Device_Type;
      Rule   :        Rule_Type)
   is
   begin
      for R of Device.Rules loop
         if R.Offset = Field_Type'Last or else Rule.Offset = R.Offset then
            pragma Debug
              (Rule.Offset = R.Offset,
               Debug_Ops.Put_Line
                 (Item => "Pciconf: WARNING overwriting rule for offset "
                  & SK.Strings.Img (SK.Byte (R.Offset))));
            R := Rule;
            return;
         end if;
      end loop;

      pragma Debug (Debug_Ops.Put_Line
                    (Item => "Pciconf: WARNING rules array is full"));
   end Append_Rule;

   -------------------------------------------------------------------------

   procedure Append_MSI_Rules
     (Device : in out Device_Type;
      Offset :        Field_Type;
      Cap_ID :        SK.Byte;
      Flags  :        SK.Byte)
   is
      MSI_Cap_Bit_64   : constant := 7;
      MSI_Cap_Bit_Mask : constant := 8;

      type MSI_Field_Offsets is record
         Msg_Data : Field_Type;
         Mask     : Field_Type;
         Pending  : Field_Type;
      end record;

      Field_Offsets : constant array (Boolean) of MSI_Field_Offsets
        := (False => (Msg_Data => 16#08#,
                      Mask     => 16#0c#,
                      Pending  => 16#10#),
            True  => (Msg_Data => 16#0c#,
                      Mask     => 16#10#,
                      Pending  => 16#14#));

      Is_MSI_64 : constant Boolean
        := SK.Bitops.Bit_Test
          (Value => SK.Word64 (Flags),
           Pos   => MSI_Cap_Bit_64);
   begin
      Append_Rule (Device => Device,
                   Rule   => (Offset      => Offset,
                              Read_Mask   => 16#ffff_0000#,
                              Vread       => Vread_MSI_Cap_ID_Next,
                              Write_Perm  => Write_Denied,
                              Write_Width => Access_16,
                              Vwrite      => Vwrite_None));
      Append_Rule (Device => Device,
                   Rule   => (Offset      => Offset + Field_MSI_Ctrl,
                              Read_Mask   => Read_No_Virt,
                              Vread       => Vread_None,
                              Write_Perm  => Write_Direct,
                              Write_Width => Access_16,
                              Vwrite      => Vwrite_None));
      Append_Rule (Device => Device,
                   Rule   => (Offset      => Offset + 16#04#,
                              Read_Mask   => Read_No_Virt,
                              Vread       => Vread_None,
                              Write_Perm  => Write_Direct,
                              Write_Width => Access_32,
                              Vwrite      => Vwrite_None));

      if Cap_ID = MSI_Cap_ID then

         --  MSI

         if Is_MSI_64 then
            Append_Rule
              (Device => Device,
               Rule   => (Offset      => Offset + 16#08#, --  Msg Upper
                          Read_Mask   => Read_No_Virt,
                          Vread       => Vread_None,
                          Write_Perm  => Write_Direct,
                          Write_Width => Access_32,
                          Vwrite      => Vwrite_None));
         end if;

         Append_Rule (Device => Device,
                      Rule   => (Offset      => Offset
                                 + Field_Offsets (Is_MSI_64).Msg_Data,
                                 Read_Mask   => Read_No_Virt,
                                 Vread       => Vread_None,
                                 Write_Perm  => Write_Direct,
                                 Write_Width => Access_16,
                                 Vwrite      => Vwrite_None));

         if SK.Bitops.Bit_Test
           (Value => SK.Word64 (Flags),
            Pos   => MSI_Cap_Bit_Mask)
         then

            --  Mask Bits

            Append_Rule (Device => Device,
                         Rule   => (Offset      => Offset + Field_Offsets
                                    (Is_MSI_64).Mask,
                                    Read_Mask   => Read_No_Virt,
                                    Vread       => Vread_None,
                                    Write_Perm  => Write_Direct,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));

            --  Pending Bits

            Append_Rule (Device => Device,
                         Rule   => (Offset      => Offset
                                    + Field_Offsets (Is_MSI_64).Pending,
                                    Read_Mask   => Read_No_Virt,
                                    Vread       => Vread_None,
                                    Write_Perm  => Write_Direct,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));
         end if;
      else

         --  MSI-X

         Append_Rule (Device => Device,
                      Rule   => (Offset      => Offset + 16#08#, -- PBA
                                 Read_Mask   => Read_No_Virt,
                                 Vread       => Vread_None,
                                 Write_Perm  => Write_Direct,
                                 Write_Width => Access_32,
                                 Vwrite      => Vwrite_None));
      end if;
   end Append_MSI_Rules;

   -------------------------------------------------------------------------

   function Get_Device (SID : Musinfo.SID_Type) return Device_Type
   is
      use type Musinfo.SID_Type;
   begin
      for D of Device_DB loop
         if D.SID = SID then
            return D;
         end if;
      end loop;

      return Null_Device;
   end Get_Device;

   -------------------------------------------------------------------------

   function Get_Rule
     (Offset : Field_Type;
      Device : Device_Type)
      return Rule_Type
   is
      --  Lookup rule for given offset.
      function Get_Rule
        (Rules : Rule_Array;
         O     : Field_Type)
         return Rule_Type;

      ----------------------------------------------------------------------

      function Get_Rule
        (Rules : Rule_Array;
         O     : Field_Type)
         return Rule_Type
      is
      begin
         for R of Rules loop
            exit when R = Null_Rule;
            if R.Offset = O then
               return R;
            end if;
         end loop;

         return Null_Rule;
      end Get_Rule;

      Res : Rule_Type;
   begin
      Res := Get_Rule (Rules => Global_Rules,
                       O     => Offset);

      if Res = Null_Rule then
         Res := Get_Rule (Rules => Device.Rules,
                          O     => Offset);
      end if;

      return Res;
   end Get_Rule;

   -------------------------------------------------------------------------

   function Read_BAR
     (Device : Device_Type;
      Offset : Field_Type)
      return SK.Word32
   is
      Res : SK.Word32;
      Idx : constant BAR_Range := BAR_Range (Offset - Field_BAR0) / 4;
   begin
      case Device.BARs (Idx).State is
         when BAR_Address => Res := Device.BARs (Idx).Address;
         when BAR_Size    => Res := Device.BARs (Idx).Size;
      end case;

      return Res;
   end Read_BAR;

   -------------------------------------------------------------------------

   function Read_Cap_Pointer (Device : Device_Type) return Field_Type
   is
      Res : Field_Type := 0;
   begin
      if Device.MSI_Cap_Offset /= No_Cap then
         Res := Device.MSI_Cap_Offset;
      elsif Device.MSI_X_Cap_Offset /= No_Cap then
         Res := Device.MSI_X_Cap_Offset;
      end if;

      return Res;
   end Read_Cap_Pointer;

   -------------------------------------------------------------------------

   function Read_MSI_Cap_ID_Next
     (Device : Device_Type;
      Offset : Field_Type)
      return SK.Word16
   is
      use type SK.Word16;

      Res : SK.Word16 := 0;
   begin
      if Offset = Device.MSI_X_Cap_Offset then
         Res := MSI_X_Cap_ID;
      else
         if Device.MSI_X_Cap_Offset /= No_Cap then
            Res := SK.Word16 (Device.MSI_X_Cap_Offset) * 2 ** 8;
         end if;

         Res := Res or SK.Word16 (MSI_Cap_ID);
      end if;

      return Res;
   end Read_MSI_Cap_ID_Next;

   -------------------------------------------------------------------------

   function Vread
     (Device    : Device_Type;
      Operation : Vread_Type;
      Offset    : Field_Type)
      return SK.Word64
   is
      Res : SK.Word64;
   begin
      case Operation is
         when Vread_BAR => Res := SK.Word64
              (Read_BAR (Device => Device,
                         Offset => Offset));
         when Vread_Cap_Pointer => Res := SK.Word64
              (Read_Cap_Pointer (Device => Device));
         when Vread_MSI_Cap_ID_Next => Res := SK.Word64
              (Read_MSI_Cap_ID_Next (Device => Device,
                                     Offset => Offset));
         when Vread_None => Res := 0;
      end case;

      return Res;
   end Vread;

   -------------------------------------------------------------------------

   procedure Write_BAR
     (Device : Device_Type;
      Offset : Field_Type;
      Value  : SK.Word32)
   is
      use type SK.Word32;

      Update : Device_Type        := Device;
      Idx    : constant BAR_Range := BAR_Range (Offset - Field_BAR0) / 4;
   begin
      if Value = SK.Word32'Last then
         Update.BARs (Idx).State := BAR_Size;
      else
         Update.BARs (Idx).State := BAR_Address;
      end if;

      Insert_Device (Device => Update);
   end Write_BAR;

   -------------------------------------------------------------------------

   procedure Write_Command
     (Base  : SK.Word64;
      Value : SK.Word16)
   is
      use type SK.Word16;
      use type SK.Word64;

      --  Only allow:
      --  * I/O space
      --  * Memory space
      --  * Bus master
      --  * Interrupt disable
      --
      --  Set all other bits to 0 (state after #RST, see Command Register Bits
      --  in the PCI Local Bus Specification, Revision 3.0.
      Allowed : constant := 02#0100_0000_0111#;
   begin
      FA.Write_Config16 (GPA   => Base + Field_Command,
                         Value => Value and Allowed);
   end Write_Command;

   -------------------------------------------------------------------------

   procedure Vwrite
     (Device    : Device_Type;
      Operation : Vwrite_Type;
      Offset    : Field_Type;
      Value     : SK.Word32)
   is
   begin
      case Operation is
         when Vwrite_BAR => Write_BAR
              (Device => Device,
               Offset => Offset,
               Value  => Value);
         when Vwrite_Command => Write_Command
              (Base  => Device.Base_Address,
               Value => SK.Word16'Mod (Value));
         when Vwrite_XUSB2PR => Quirks.Write_XUSB2PR
              (SID   => Device.SID,
               Value => SK.Word16'Mod (Value));
         when Vwrite_PSSEN => Quirks.Write_PSSEN
              (SID   => Device.SID,
               Value => SK.Byte'Mod (Value));
         when Vwrite_None => null;
      end case;
   end Vwrite;

   -------------------------------------------------------------------------

   procedure Init
     (Device : out Device_Type;
      SID    :     Musinfo.SID_Type;
      Base   :     SK.Word64)
   is
      use type SK.Word32;
      use type SK.Word64;
   begin
      Device := (SID              => SID,
                 Base_Address     => Base,
                 MSI_Cap_Offset   => No_Cap,
                 MSI_X_Cap_Offset => No_Cap,
                 BARs             => (others => Null_BAR),
                 Rules            => (others => Null_Rule));

      --  BARs

      for I in Device.BARs'Range loop
         declare
            BAR_Addr : constant SK.Word64
              := Base + Field_BAR0 + SK.Word64 (I * 4);
         begin
            Device.BARs (I).Address := FA.Read_Config32 (GPA => BAR_Addr);
            FA.Write_Config32 (GPA   => BAR_Addr,
                               Value => SK.Word32'Last);
            Device.BARs (I).Size := FA.Read_Config32 (GPA => BAR_Addr);
            FA.Write_Config32 (GPA   => BAR_Addr,
                               Value => Device.BARs (I).Address);
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "Pciconf " & SK.Strings.Img (SID) & ":"
                  & " BAR" & SK.Strings.Img_Nobase (SK.Byte (I))
                  & " address " & SK.Strings.Img (Device.BARs (I).Address)
                  & " size " & SK.Strings.Img (Device.BARs (I).Size)));
         end;
      end loop;

      --  Caps

      declare
         use type SK.Word16;

         type Search_Range is range 1 .. 48;

         subtype Header_Field_Range is Field_Type range 0 .. 16#3f#;

         Val    : SK.Word16;
         Offset : Field_Type := Field_Type
           (FA.Read_Config8 (GPA => Base + Field_Cap_Pointer));
      begin
         Search :
         for S in Search_Range loop
            exit Search when Offset = 0 or Offset in Header_Field_Range;

            Val := FA.Read_Config16 (GPA => Base + SK.Word64 (Offset));

            if SK.Byte (Val) = MSI_Cap_ID
              or else SK.Byte (Val) = MSI_X_Cap_ID
            then
               if SK.Byte (Val) = MSI_Cap_ID then
                  Device.MSI_Cap_Offset   := Offset;
               else
                  Device.MSI_X_Cap_Offset := Offset;
               end if;
               pragma Debug
                 (Debug_Ops.Put_Line
                    (Item => "Pciconf " & SK.Strings.Img (SID)
                     & ": MSI(X) cap ID " & SK.Strings.Img (SK.Byte (Val))
                     & " @ offset " & SK.Strings.Img (SK.Byte (Offset))));
               Append_MSI_Rules
                 (Device => Device,
                  Offset => Offset,
                  Cap_ID => SK.Byte (Val),
                  Flags  => FA.Read_Config8
                    (GPA => Base + SK.Word64 (Offset) + Field_MSI_Ctrl));
            end if;

            Offset := Field_Type (Val / 2 ** 8);
         end loop Search;
      end;

      --  PCI config space quirks.

      Quirks.Register
        (Dev_State => Device,
         Vendor    => FA.Read_Config16 (GPA => Base),
         Device    => FA.Read_Config16 (GPA => Base + Field_Device),
         Class     => FA.Read_Config32
           (GPA => Base + Field_Revision_Class) / 2 ** 8);
   end Init;

   -------------------------------------------------------------------------

   procedure Insert_Device (Device : Device_Type)
   is
      use type Musinfo.SID_Type;
   begin
      for D of Device_DB loop
         if D.SID = Musinfo.Null_SID or else D.SID = Device.SID then
            D := Device;
            return;
         end if;
      end loop;

      pragma Debug (Debug_Ops.Put_Line
                    (Item => "Pciconf: WARNING device array is full"));
   end Insert_Device;

   -------------------------------------------------------------------------

   procedure Emulate
     (Info   :     Types.EPTV_Info_Type;
      Action : out Types.Subject_Action_Type)
   is
      use type SK.Word32;
      use type SK.Word64;
      use type Musinfo.Dev_Info_Type;

      Base_Mask : constant := 16#ffff_f000#;

      Header   : SK.Byte;
      RAX      : SK.Word64                 := 0;
      GPA      : constant SK.Word64        := SI.State.Guest_Phys_Addr;
      Dev_Base : constant SK.Word64        := GPA and Base_Mask;
      Offset   : constant Field_Type       := Field_Type (GPA);
      SID      : constant Musinfo.SID_Type := Musinfo.SID_Type
        (Interfaces.Shift_Right
           (Value  => GPA - Config.MMConf_Base_Address,
            Amount => 12));
      Dev_Info : constant Musinfo.Dev_Info_Type
        := Musinfo.Instance.Device_By_SID (SID => SID);
      Device   : Device_Type := Get_Device (SID => SID);
      Rule     : Rule_Type;
   begin
      Action := Types.Subject_Continue;

      if Device = Null_Device then
         if Dev_Info = Musinfo.Null_Dev_Info then

            --  Set result to 16#ffff# to indicate a non-existent device.

            SI.State.Regs.RAX := 16#ffff#;
            return;
         end if;

         Header := FA.Read_Config8 (GPA => Dev_Base + Field_Header);
         if Header /= 0 then
            pragma Debug (Debug_Ops.Put_Line
                          (Item => "Pciconf " & SK.Strings.Img (SID)
                           & ": Unsupported header " & SK.Strings.Img (Header)
                           & " for device with base address "
                           & SK.Strings.Img (Dev_Base)));
            return;
         end if;

         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Pciconf " & SK.Strings.Img (SID) & ": Init of device "
               & "with base address " & SK.Strings.Img (Dev_Base)));
         Init (Device => Device,
               SID    => SID,
               Base   => Dev_Base);
         Insert_Device (Device => Device);
      end if;

      Rule := Get_Rule
        (Offset => Offset,
         Device => Device);

      if Info.Read then
         pragma Debug
           (Debug_Ops.Put_String
              (Item => "Pciconf " & SK.Strings.Img (SID) & ": Read "));
         declare
            Width : constant Access_Width_Type := Read_Widths
              (SK.Byte (Offset) mod 4);
         begin

            --  Read real value if not fully virtualized.

            if Rule = Null_Rule or else Rule.Read_Mask /= Read_All_Virt then
               case Width is
                  when Access_8  => RAX := SK.Word64
                       (FA.Read_Config8  (GPA => GPA));
                  when Access_16 => RAX := SK.Word64
                       (FA.Read_Config16 (GPA => GPA));
                  when Access_32 => RAX := SK.Word64
                       (FA.Read_Config32 (GPA => GPA));
               end case;

               --  Mask out bits as specified by config entry.

               if Rule /= Null_Rule and then Rule.Read_Mask /= Read_No_Virt
               then
                  RAX := RAX and SK.Word64 (Rule.Read_Mask);
               end if;
            end if;

            --  Merge in virtualized bits.

            if Rule /= Null_Rule and then Rule.Vread /= Vread_None then
               RAX := RAX or Vread
                 (Device    => Device,
                  Operation => Rule.Vread,
                  Offset    => Offset);
            end if;
            pragma Debug (Rule /= Null_Rule
                          and Rule.Read_Mask = Read_All_Virt,
                          Debug_Ops.Put_String (Item => "(ALLVIRT)"));
            pragma Debug (Rule /= Null_Rule
                          and Rule.Read_Mask /= Read_All_Virt
                          and Rule.Read_Mask /= Read_No_Virt,
                          Debug_Ops.Put_String (Item => "(VIRT)"));
            pragma Debug (Debug_Ops.Put_Line
                          (Item => " @ " & SK.Strings.Img (GPA) & ": "
                           & SK.Strings.Img (RAX)));
            SI.State.Regs.RAX := RAX;
         end;
      elsif Info.Write then
         RAX := SI.State.Regs.RAX;

         pragma Debug
           (Rule /= Null_Rule,
            Debug_Ops.Check_Warn_PCI_Write_Width
              (Value     => RAX,
               Width_Idx => Access_Width_Type'Pos (Rule.Write_Width)));
         pragma Debug (Debug_Ops.Put_String
                       (Item => "Pciconf "
                        & SK.Strings.Img (SID) & ": Write"));

         if Rule /= Null_Rule then
            case Rule.Write_Perm is
               when Write_Denied => null;
               when Write_Direct =>
                  case Rule.Write_Width is
                     when Access_8  => FA.Write_Config8
                          (GPA   => GPA,
                           Value => SK.Byte (RAX));
                     when Access_16 => FA.Write_Config16
                          (GPA   => GPA,
                           Value => SK.Word16 (RAX));
                     when Access_32 => FA.Write_Config32
                          (GPA   => GPA,
                           Value => SK.Word32 (RAX));
                  end case;
               when Write_Virt =>
                  Vwrite (Device    => Device,
                          Operation => Rule.Vwrite,
                          Offset    => Offset,
                          Value     => SK.Word32'Mod (SI.State.Regs.RAX));
                  pragma Debug (Debug_Ops.Put_String (Item => " (ALLVIRT)"));
            end case;
         end if;
         pragma Debug (Rule = Null_Rule
                       or else Rule.Write_Perm = Write_Denied,
                       Debug_Ops.Put_String (Item => " (DENIED)"));
         pragma Debug (Debug_Ops.Put_Line
                       (Item => " @ " & SK.Strings.Img (GPA) & ": "
                        & SK.Strings.Img (RAX)));
      end if;
   end Emulate;

end Devices.Pciconf;
