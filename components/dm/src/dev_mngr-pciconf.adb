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

with SK.Bitops;
with SK.Strings;

with Dev_Mngr.Debug_Ops;
with Dev_Mngr.Pciconf.Quirks;
with Dev_Mngr.Pciconf.Addrspace;

package body Dev_Mngr.Pciconf
with
   Refined_State => (State => (Device_DB, Addrspace.Memory))
is

   use type SK.Byte;

   --  See PCI Local Bus Specification Revision 3.0, section 6.1.
   Field_Vendor          : constant Offset_Type := 16#00#;
   Field_Device          : constant Offset_Type := 16#02#;
   Field_Command         : constant Offset_Type := 16#04#;
   Field_Cache_Line_Size : constant Offset_Type := 16#0c#;
   Field_Latency_Timer   : constant Offset_Type := 16#0d#;
   Field_Header          : constant Offset_Type := 16#0e#;
   Field_BIST            : constant Offset_Type := 16#0f#;
   Field_Revision_Class  : constant Offset_Type := 16#08#;
   Field_BAR0            : constant Offset_Type := 16#10#;
   Field_BAR1            : constant Offset_Type := 16#14#;
   Field_BAR2            : constant Offset_Type := 16#18#;
   Field_BAR3            : constant Offset_Type := 16#1c#;
   Field_BAR4            : constant Offset_Type := 16#20#;
   Field_BAR5            : constant Offset_Type := 16#24#;
   Field_Cap_Pointer     : constant Offset_Type := 16#34#;

   Field_MSI_Ctrl : constant Offset_Type := 16#02#;

   MSI_Cap_ID   : constant := 16#05#;
   MSI_X_Cap_ID : constant := 16#11#;

   No_Cap : constant := SK.Byte'Last;

   Null_Rule : constant Rule_Type
     := (Offset      => Offset_Type'Last,
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
      SID    :     Musinfo.SID_Type)
   with
      Global => (In_Out => Addrspace.Memory);

   --  Return device for given SID. If no device with the specified SID is
   --  present, Null_Device is returned.
   function Get_Device (SID : Musinfo.SID_Type) return Device_Type;

   --  Add given device to devices DB. If a device with the same SID exists,
   --  the entry is replaced.
   procedure Insert_Device (Device : Device_Type);

   --  Lookup rule for given offset. Null_Rule is returned if no rule with that
   --  offset is found.
   function Get_Rule
     (Rules  : Rule_Array;
      Offset : Offset_Type)
      return Rule_Type;

   --  Get rule for given device and offset. The global rules are searched
   --  first, if no match is found the per-device rules are consulted.
   function Get_Rule
     (Offset : Offset_Type;
      Device : Device_Type)
      return Rule_Type;

   --  Append per-device rules for MSI/MSI-X fields starting at given offset
   --  with specified feature flags (64-bit or maskable). See PCI specification
   --  3.0, sections 6.8.1/6.8.2.
   procedure Append_MSI_Rules
     (Device : in out Device_Type;
      Offset :        Offset_Type;
      Cap_ID :        SK.Byte;
      Flags  :        SK.Byte);

   --  Perform virtualized read operation for specified device at given offset.
   function Vread
     (Device    : Device_Type;
      Operation : Vread_Type;
      Offset    : Offset_Type)
      return SK.Word32;

   --  Return virtualized capability pointer value of given device.
   function Read_Cap_Pointer (Device : Device_Type) return Offset_Type;

   --  Return virtualized MSI cap ID and next pointer.
   function Read_MSI_Cap_ID_Next
     (Device : Device_Type;
      Offset : Offset_Type)
      return SK.Word16;

   --  Return virtualized BAR value for specified device at given offset.
   function Read_BAR
     (Device : Device_Type;
      Offset : BAR_Offset_Type)
      return SK.Word32;

   --  Perform virtualized write operation for specified device, offset and
   --  value.
   procedure Vwrite
     (Device    : Device_Type;
      Operation : Vwrite_Type;
      Offset    : Offset_Type;
      Value     : SK.Word32)
   with
      Global => (In_Out => (Device_DB, Addrspace.Memory));

   --  Write value to BAR at given offset.
   procedure Write_BAR
     (Device : Device_Type;
      Offset : BAR_Offset_Type;
      Value  : SK.Word32);

   --  Write given value to command register of device specified by SID.
   procedure Write_Command
     (SID   : Musinfo.SID_Type;
      Value : SK.Word16)
   with
      Global => (In_Out => Addrspace.Memory);

   -------------------------------------------------------------------------

   procedure Append_Rule
     (Device : in out Device_Type;
      Rule   :        Rule_Type)
   is
   begin
      pragma Debug (Device.Rules (Device.Rules'Last) /= Null_Rule,
                    Debug_Ops.Put_Line
                      (Item => "Pciconf: WARNING rules array is full"));
      for R of Device.Rules loop
         if R.Offset = Offset_Type'Last or else Rule.Offset = R.Offset then
            pragma Debug
              (Rule.Offset = R.Offset,
               Debug_Ops.Put_Line
                 (Item => "Pciconf: WARNING overwriting rule for offset "
                  & SK.Strings.Img (SK.Byte (R.Offset))));
            R := Rule;
            exit;
         end if;
      end loop;
   end Append_Rule;

   -------------------------------------------------------------------------

   procedure Append_MSI_Rules
     (Device : in out Device_Type;
      Offset :        Offset_Type;
      Cap_ID :        SK.Byte;
      Flags  :        SK.Byte)
   is
      MSI_Cap_Bit_64   : constant := 7;
      MSI_Cap_Bit_Mask : constant := 8;

      type MSI_Field_Offsets is record
         Msg_Data : Offset_Type;
         Mask     : Offset_Type;
         Pending  : Offset_Type;
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

      Res : Device_Type := Null_Device;
   begin
      for D of Device_DB loop
         if D.SID = SID then
            Res := D;
            exit;
         end if;
      end loop;

      return Res;
   end Get_Device;

   -------------------------------------------------------------------------

   function Get_Rule
     (Rules  : Rule_Array;
      Offset : Offset_Type)
      return Rule_Type
   is
      Res : Rule_Type := Null_Rule;
   begin
      for R of Rules loop
         exit when R = Null_Rule;
         if R.Offset = Offset then
            Res := R;
         end if;
      end loop;

      return Res;
   end Get_Rule;

   -------------------------------------------------------------------------

   function Get_Rule
     (Offset : Offset_Type;
      Device : Device_Type)
      return Rule_Type
   is
      Res : Rule_Type;
   begin
      Res := Get_Rule (Rules  => Global_Rules,
                       Offset => Offset);

      if Res = Null_Rule then
         Res := Get_Rule (Rules  => Device.Rules,
                          Offset => Offset);
      end if;

      return Res;
   end Get_Rule;

   -------------------------------------------------------------------------

   procedure Insert_Device (Device : Device_Type)
   is
      use type Musinfo.SID_Type;
   begin
      pragma Debug (Device_DB (Device_DB'Last) /= Null_Device,
                    Debug_Ops.Put_Line
                      (Item => "Pciconf: WARNING device array is full"));

      for D of Device_DB loop
         if D.SID = Musinfo.Null_SID or else D.SID = Device.SID then
            D := Device;
            exit;
         end if;
      end loop;
   end Insert_Device;

   -------------------------------------------------------------------------

   function Read_BAR
     (Device : Device_Type;
      Offset : BAR_Offset_Type)
      return SK.Word32
   is
      Res : SK.Word32;
      Idx : constant BAR_Range := BAR_Range ((Offset - Field_BAR0) / 4);
   begin
      case Device.BARs (Idx).State is
         when BAR_Address => Res := Device.BARs (Idx).Address;
         when BAR_Size    => Res := Device.BARs (Idx).Size;
      end case;

      return Res;
   end Read_BAR;

   -------------------------------------------------------------------------

   function Read_Cap_Pointer (Device : Device_Type) return Offset_Type
   is
      Res : Offset_Type := 0;
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
      Offset : Offset_Type)
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
      Offset    : Offset_Type)
      return SK.Word32
   is
      Res : SK.Word32 := 0;
   begin
      case Operation is
         when Vread_BAR =>
            if Offset in BAR_Offset_Type then
               Res := Read_BAR (Device => Device,
                                Offset => Offset);
            end if;
            pragma Debug
              (Offset not in BAR_Offset_Type,
               Debug_Ops.Put (Item => " [invalid BAR offset "
                              & SK.Strings.Img (SK.Byte (Offset)) & "]"));
         when Vread_Cap_Pointer => Res := SK.Word32
              (Read_Cap_Pointer (Device => Device));
         when Vread_MSI_Cap_ID_Next => Res := SK.Word32
              (Read_MSI_Cap_ID_Next (Device => Device,
                                     Offset => Offset));
         when Vread_None => null;
      end case;

      return Res;
   end Vread;

   -------------------------------------------------------------------------

   procedure Write_BAR
     (Device : Device_Type;
      Offset : BAR_Offset_Type;
      Value  : SK.Word32)
   is
      use type SK.Word32;

      Update : Device_Type        := Device;
      Idx    : constant BAR_Range := BAR_Range ((Offset - Field_BAR0) / 4);
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
     (SID   : Musinfo.SID_Type;
      Value : SK.Word16)
   is
      use type SK.Word16;

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
      Addrspace.Write_Word16 (SID    => SID,
                              Offset => Field_Command,
                              Value  => Value and Allowed);
   end Write_Command;

   -------------------------------------------------------------------------

   procedure Vwrite
     (Device    : Device_Type;
      Operation : Vwrite_Type;
      Offset    : Offset_Type;
      Value     : SK.Word32)
   is
   begin
      case Operation is
         when Vwrite_BAR =>
            if Offset in BAR_Offset_Type then
               Write_BAR
                 (Device => Device,
                  Offset => Offset,
                  Value  => Value);
            end if;
            pragma Debug
              (Offset not in BAR_Offset_Type,
               Debug_Ops.Put (Item => " [invalid BAR offset "
                              & SK.Strings.Img (SK.Byte (Offset)) & "]"));
         when Vwrite_Command => Write_Command
              (SID   => Device.SID,
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
      SID    :     Musinfo.SID_Type)
   is
      use type SK.Word32;
   begin
      Device := (SID              => SID,
                 MSI_Cap_Offset   => No_Cap,
                 MSI_X_Cap_Offset => No_Cap,
                 BARs             => (others => Null_BAR),
                 Rules            => (others => Null_Rule));

      --  BARs

      for I in Device.BARs'Range loop
         declare
            BAR_Offset : constant Offset_Type
              := Field_BAR0 + Offset_Type (I * 4);
         begin
            Device.BARs (I).Address := Addrspace.Read_Word32
              (SID    => SID,
               Offset => BAR_Offset);
            Addrspace.Write_Word32
              (SID    => SID,
               Offset => BAR_Offset,
               Value  => SK.Word32'Last);
            Device.BARs (I).Size := Addrspace.Read_Word32
              (SID    => SID,
               Offset => BAR_Offset);
            Addrspace.Write_Word32
              (SID    => SID,
               Offset => BAR_Offset,
               Value  => Device.BARs (I).Address);
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

         subtype Header_Field_Range is Offset_Type range 0 .. 16#3f#;

         Val    : SK.Word16;
         Offset : Offset_Type := Offset_Type
           (Addrspace.Read_Byte (SID    => SID,
                                 Offset => Field_Cap_Pointer));
      begin
         Search :
         for S in Search_Range loop
            exit Search when Offset = 0 or Offset in Header_Field_Range;

            Val := Addrspace.Read_Word16
              (SID    => SID,
               Offset => Offset);

            if SK.Byte'Mod (Val) = MSI_Cap_ID
              or else SK.Byte'Mod (Val) = MSI_X_Cap_ID
            then
               if SK.Byte'Mod (Val) = MSI_Cap_ID then
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
                  Cap_ID => SK.Byte'Mod (Val),
                  Flags  => Addrspace.Read_Byte
                    (SID    => SID,
                     Offset => Offset + Field_MSI_Ctrl));
            end if;

            Offset := Offset_Type (Val / 2 ** 8);
         end loop Search;
      end;

      --  PCI config space quirks.

      declare
         Vendor_ID : constant SK.Word16
           := Addrspace.Read_Word16
             (SID    => SID,
              Offset => Field_Vendor);
         Device_ID : constant SK.Word16
           := Addrspace.Read_Word16
             (SID    => SID,
              Offset => Field_Device);
         Class : constant SK.Word32
           := Addrspace.Read_Word32
             (SID    => SID,
              Offset => Field_Revision_Class);
      begin
         Quirks.Register
           (Dev_State => Device,
            Vendor    => Vendor_ID,
            Device    => Device_ID,
            Class     => Class / 2 ** 8);
      end;
   end Init;

   -------------------------------------------------------------------------

   procedure Emulate
     (SID    :     Musinfo.SID_Type;
      Op     :     Emul_Req_Op_Type;
      Offset :     Offset_Type;
      Value  :     SK.Word32;
      Result : out SK.Word32)
   is
      use type SK.Word32;
      use type SK.Word64;
      use type Musinfo.Dev_Info_Type;

      Header   : SK.Byte;
      Dev_Info : constant Musinfo.Dev_Info_Type
        := Musinfo.Instance.Device_By_SID (SID => SID);
      Device   : Device_Type := Get_Device (SID => SID);
      Rule     : Rule_Type;
   begin
      Result := 0;

      if Device = Null_Device then
         if Dev_Info = Musinfo.Null_Dev_Info then

            --  Set result to 16#ffff# to indicate a non-existent device.

            Result := 16#ffff#;
            return;
         end if;

         Header := Addrspace.Read_Byte
           (SID    => SID,
            Offset => Field_Header);
         if Header /= 0 then
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "Pciconf " & SK.Strings.Img (SID)
                  & ": Unsupported header " & SK.Strings.Img (Header)));
            return;
         end if;

         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Pciconf " & SK.Strings.Img (SID)
               & ": Initializing device"));
         Init (Device => Device,
               SID    => SID);
         Insert_Device (Device => Device);
      end if;

      Rule := Get_Rule
        (Offset => Offset,
         Device => Device);

      if Op = Emul_Req_Read then
         pragma Debug
           (Debug_Ops.Put (Item => "Pciconf "
                           & SK.Strings.Img (SID) & ": Read "));
         declare
            Width : constant Access_Width_Type := Read_Widths
              (SK.Byte (Offset) mod 4);
         begin

            --  Read real value if not fully virtualized.

            if Rule = Null_Rule or else Rule.Read_Mask /= Read_All_Virt then
               case Width is
                  when Access_8  => Result := SK.Word32
                       (Addrspace.Read_Byte
                          (SID    => SID,
                           Offset => Offset));
                  when Access_16 => Result := SK.Word32
                       (Addrspace.Read_Word16
                          (SID    => SID,
                           Offset => Offset));
                  when Access_32 => Result := Addrspace.Read_Word32
                       (SID    => SID,
                        Offset => Offset);
               end case;

               --  Mask out bits as specified by config entry.

               if Rule /= Null_Rule and then Rule.Read_Mask /= Read_No_Virt
               then
                  Result := Result and Rule.Read_Mask;
               end if;
            end if;

            --  Merge in virtualized bits.

            if Rule /= Null_Rule and then Rule.Vread /= Vread_None then
               Result := Result or Vread
                 (Device    => Device,
                  Operation => Rule.Vread,
                  Offset    => Offset);
            end if;
            pragma Debug (Rule /= Null_Rule
                          and Rule.Read_Mask = Read_All_Virt,
                          Debug_Ops.Put (Item => "(ALLVIRT)"));
            pragma Debug (Rule /= Null_Rule
                          and Rule.Read_Mask /= Read_All_Virt
                          and Rule.Read_Mask /= Read_No_Virt,
                          Debug_Ops.Put (Item => "(VIRT)"));
            pragma Debug (Debug_Ops.Put_Line
                          (Item => " @ " & SK.Strings.Img
                           (SK.Byte (Offset)) & ": "
                           & SK.Strings.Img (Result)));
         end;
      elsif Op = Emul_Req_Write then
         pragma Debug
           (Rule /= Null_Rule,
            Debug_Ops.Check_Warn_PCI_Write_Width
              (Value     => Value,
               Width_Idx => Access_Width_Type'Pos (Rule.Write_Width)));
         pragma Debug (Debug_Ops.Put (Item => "Pciconf " & SK.Strings.Img (SID)
                                      & ": Write"));

         if Rule /= Null_Rule then
            case Rule.Write_Perm is
               when Write_Denied => null;
               when Write_Direct =>
                  case Rule.Write_Width is
                     when Access_8  => Addrspace.Write_Byte
                          (SID    => SID,
                           Offset => Offset,
                           Value  => SK.Byte'Mod (Value));
                     when Access_16 => Addrspace.Write_Word16
                          (SID    => SID,
                           Offset => Offset,
                           Value  => SK.Word16'Mod (Value));
                     when Access_32 => Addrspace.Write_Word32
                          (SID    => SID,
                           Offset => Offset,
                           Value  => SK.Word32'Mod (Value));
                  end case;
               when Write_Virt =>
                  Vwrite (Device    => Device,
                          Operation => Rule.Vwrite,
                          Offset    => Offset,
                          Value     => SK.Word32'Mod (Value));
                  pragma Debug (Debug_Ops.Put (Item => " (ALLVIRT)"));
            end case;
         end if;
         pragma Debug (Rule = Null_Rule
                       or else Rule.Write_Perm = Write_Denied,
                       Debug_Ops.Put (Item => " (DENIED)"));
         pragma Debug (Debug_Ops.Put_Line
                       (Item => " @ " & SK.Strings.Img (SK.Byte (Offset))
                        & ": " & SK.Strings.Img (Value)));
      end if;
   end Emulate;

end Dev_Mngr.Pciconf;
