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

with Interfaces;

with SK.Bitops;
with SK.Strings;

with Musinfo.Instance;

with Config;
with Debug_Ops;
with Devices.Pciconf.Quirks;

package body Devices.Pciconf
with
   SPARK_Mode => Off -- Q725-014
is

   package SI renames Subject_Info;

   use type SK.Byte;

   --  See PCI Local Bus Specification Revision 3.0, section 6.1.
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

   MSI_Cap_Bit_64   : constant := 7;
   MSI_Cap_Bit_Mask : constant := 8;

   No_Cap : constant := SK.Byte'Last;

   Null_Config : constant Config_Entry_Type
     := (Offset      => Field_Type'Last,
         Read_Mask   => Read_All_Virt,
         Vread       => Vread_None,
         Write_Perm  => Write_Denied,
         Write_Width => Access_8,
         Vwrite      => Vwrite_None);

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

   type BAR_Array is array (0 .. 5) of BAR_Type;

   type Config_Array is array (1 .. 24) of Config_Entry_Type;

   --  Contains pre-defined read/write rules, room for MSI/MSI-X capability
   --  handling and PCI quirks for certain devices.
   Rules : Config_Array
     := (1      => (Offset      => Field_Command,
                    Read_Mask   => Read_No_Virt,
                    Vread       => Vread_None,
                    Write_Perm  => Write_Direct,
                    Write_Width => Access_16,
                    Vwrite      => Vwrite_None),
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
                    Vwrite      => Vwrite_None),
         others => Null_Config);

   subtype Read_Idx_Type is SK.Byte range 0 .. 3;

   Read_Widths : constant array (Read_Idx_Type) of Access_Width_Type
     := (0 => Access_32,
         1 => Access_8,
         2 => Access_16,
         3 => Access_8);

   --  Device state.

   type Device_Type is record
      Initialized      : Boolean;
      MSI_Cap_Offset   : Field_Type;
      MSI_X_Cap_Offset : Field_Type;
      BARs             : BAR_Array;
   end record;

   Null_Device : constant Device_Type
     := (Initialized      => False,
         MSI_Cap_Offset   => No_Cap,
         MSI_X_Cap_Offset => No_Cap,
         BARs             => (others => Null_BAR));

   Device : Device_Type := Null_Device;

   --  Get config entry for given offset.
   function Get_Config (Offset : Field_Type) return Config_Entry_Type
   with
      Global => (Input => Rules);

   --  Append config entries for MSI/MSI-X fields starting at given offset with
   --  specified feature flags (64-bit or maskable). See PCI specification 3.0,
   --  sections 6.8.1/6.8.2.
   procedure Append_MSI_Config
     (Offset : Field_Type;
      Cap_ID : SK.Byte;
      Flags  : SK.Word16)
   with
      Global => (In_Out => Rules);

   generic
      type Element_Type is mod <>;
   function Read_Config (GPA : SK.Word64) return SK.Word64;

   generic
      type Element_Type is mod <>;
   procedure Write_Config
     (GPA   : SK.Word64;
      Value : Element_Type);

   --  Perform virtualized read operation at given offset.
   function Vread
     (V : Vread_Type;
      O : Field_Type)
      return SK.Word64;

   --  Return virtualized capability pointer value.
   function Read_Cap_Pointer (Offset : Field_Type) return Field_Type;

   --  Return virtualized MSI cap ID and next pointer.
   function Read_MSI_Cap_ID_Next (Offset : Field_Type) return SK.Word16;

   --  Return virtualized BAR value at given offset.
   function Read_BAR (Offset : Field_Type) return SK.Word32;

   --  Perform virtualized write operation at given offset.
   procedure Vwrite
     (V : Vwrite_Type;
      O : Field_Type);

   --  Write BAR at given offset.
   procedure Write_BAR (Offset : Field_Type);

   -------------------------------------------------------------------------

   function Read_Config (GPA : SK.Word64) return SK.Word64
   is
      Val : Element_Type
      with
         Import,
         Address => System'To_Address (GPA);
   begin
      return SK.Word64 (Val);
   end Read_Config;

   function Read_Config8  is new Read_Config (Element_Type => SK.Byte);
   function Read_Config16 is new Read_Config (Element_Type => SK.Word16);
   function Read_Config32 is new Read_Config (Element_Type => SK.Word32);

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

   procedure Write_Config8  is new Write_Config (Element_Type => SK.Byte);
   procedure Write_Config16 is new Write_Config (Element_Type => SK.Word16);
   procedure Write_Config32 is new Write_Config (Element_Type => SK.Word32);

   -------------------------------------------------------------------------

   procedure Append_Config (C : Config_Entry_Type)
   is
   begin
      for Rule of Rules loop
         if Rule.Offset = Field_Type'Last then
            Rule := C;
            return;
         end if;
      end loop;

      pragma Debug (Debug_Ops.Put_Line
                    (Item => "Pciconf: WARNING rules array is full"));
   end Append_Config;

   -------------------------------------------------------------------------

   procedure Append_MSI_Config
     (Offset : Field_Type;
      Cap_ID : SK.Byte;
      Flags  : SK.Word16)
   is
   begin
      Append_Config (C => (Offset      => Offset,
                           Read_Mask   => 16#ffff_0000#,
                           Vread       => Vread_MSI_Cap_ID_Next,
                           Write_Perm  => Write_Denied,
                           Write_Width => Access_16,
                           Vwrite      => Vwrite_None));
      Append_Config (C => (Offset      => Offset + Field_MSI_Ctrl,
                           Read_Mask   => Read_No_Virt,
                           Vread       => Vread_None,
                           Write_Perm  => Write_Direct,
                           Write_Width => Access_16,
                           Vwrite      => Vwrite_None));
      Append_Config (C => (Offset      => Offset + 16#04#,
                           Read_Mask   => Read_No_Virt,
                           Vread       => Vread_None,
                           Write_Perm  => Write_Direct,
                           Write_Width => Access_32,
                           Vwrite      => Vwrite_None));

      if Cap_ID = MSI_Cap_ID then

         --  MSI

         if not SK.Bitops.Bit_Test
           (Value => SK.Word64 (Flags),
            Pos   => MSI_Cap_Bit_64)
         then

            --  32-bit

            Append_Config (C => (Offset      => Offset + 16#08#,
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

               Append_Config (C => (Offset      => Offset + 16#0c#,
                                    Read_Mask   => Read_No_Virt,
                                    Vread       => Vread_None,
                                    Write_Perm  => Write_Direct,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));

               --  Pending Bits

               Append_Config (C => (Offset      => Offset + 16#10#,
                                    Read_Mask   => Read_No_Virt,
                                    Vread       => Vread_None,
                                    Write_Perm  => Write_Direct,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));
            end if;
         else

            --  64-bit

            Append_Config (C => (Offset      => Offset + 16#08#,
                                 Read_Mask   => Read_No_Virt,
                                 Vread       => Vread_None,
                                 Write_Perm  => Write_Direct,
                                 Write_Width => Access_32,
                                 Vwrite      => Vwrite_None));
            Append_Config (C => (Offset      => Offset + 16#0c#,
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

               Append_Config (C => (Offset      => Offset + 16#10#,
                                    Read_Mask   => Read_No_Virt,
                                    Vread       => Vread_None,
                                    Write_Perm  => Write_Direct,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));

               --  Pending Bits

               Append_Config (C => (Offset      => Offset + 16#14#,
                                    Read_Mask   => Read_No_Virt,
                                    Vread       => Vread_None,
                                    Write_Perm  => Write_Direct,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));
            end if;
         end if;
      else

         --  MSI-X

         Append_Config (C => (Offset      => Offset + 16#08#, -- PBA
                              Read_Mask   => Read_No_Virt,
                              Vread       => Vread_None,
                              Write_Perm  => Write_Direct,
                              Write_Width => Access_32,
                              Vwrite      => Vwrite_None));
      end if;
   end Append_MSI_Config;

   -------------------------------------------------------------------------

   function Get_Config (Offset : Field_Type) return Config_Entry_Type
   is
      Res : Config_Entry_Type := Null_Config;
   begin
      for R of Rules loop
         exit when R = Null_Config;
         if R.Offset = Offset then
            Res := R;
            exit;
         end if;
      end loop;
      return Res;
   end Get_Config;

   -------------------------------------------------------------------------

   function Read_BAR (Offset : Field_Type) return SK.Word32
   is
      Res : SK.Word32;
      Idx : constant Natural := Natural (Offset - 16#10#) / 4;
   begin
      case Device.BARs (Idx).State is
         when BAR_Address => Res := Device.BARs (Idx).Address;
         when BAR_Size    => Res := Device.BARs (Idx).Size;
      end case;

      return Res;
   end Read_BAR;

   -------------------------------------------------------------------------

   function Read_Cap_Pointer (Offset : Field_Type) return Field_Type
   is
      pragma Unreferenced (Offset);

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

   function Read_MSI_Cap_ID_Next (Offset : Field_Type) return SK.Word16
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
     (V : Vread_Type;
      O : Field_Type)
      return SK.Word64
   is
      Res : SK.Word64;
   begin
      case V is
         when Vread_BAR             => Res := SK.Word64
              (Read_BAR (Offset => O));
         when Vread_Cap_Pointer     => Res := SK.Word64
              (Read_Cap_Pointer (Offset => O));
         when Vread_MSI_Cap_ID_Next => Res := SK.Word64
              (Read_MSI_Cap_ID_Next (Offset => O));
         when Vread_None            => Res := 0;
      end case;

      return Res;
   end Vread;

   -------------------------------------------------------------------------

   procedure Write_BAR (Offset : Field_Type)
   is
      use type SK.Word32;

      Idx : constant Natural   := Natural (Offset - 16#10#) / 4;
      RAX : constant SK.Word64 := SI.State.Regs.RAX;
   begin
      if SK.Word32 (RAX) = SK.Word32'Last then
         Device.BARs (Idx).State := BAR_Size;
      else
         Device.BARs (Idx).State := BAR_Address;
      end if;
   end Write_BAR;

   -------------------------------------------------------------------------

   procedure Vwrite
     (V : Vwrite_Type;
      O : Field_Type)
   is
   begin
      case V is
         when Vwrite_BAR  => Write_BAR (Offset => O);
         when Vwrite_None => null;
      end case;
   end Vwrite;

   -------------------------------------------------------------------------

   procedure Init (Device_Base : SK.Word64)
   is
      use type SK.Word16;
      use type SK.Word64;
   begin
      --  TODO make loop bound

      --  BARs

      for I in Device.BARs'Range loop
         declare
            BAR_Addr : constant SK.Word64
              := Device_Base + Field_BAR0 + SK.Word64 (I * 4);
         begin
            Device.BARs (I).Address := SK.Word32
              (Read_Config32 (GPA => BAR_Addr));
            Write_Config32 (GPA   => BAR_Addr,
                            Value => SK.Word32'Last);
            Device.BARs (I).Size := SK.Word32
              (Read_Config32 (GPA => BAR_Addr));
            Write_Config32 (GPA   => BAR_Addr,
                            Value => Device.BARs (I).Address);
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "Pciconf: BAR" & SK.Strings.Img_Nobase (SK.Byte (I))
                  & " address " & SK.Strings.Img (Device.BARs (I).Address)
                  & " size " & SK.Strings.Img (Device.BARs (I).Size)));
         end;
      end loop;

      --  Caps

      declare
         Val    : SK.Word16;
         Offset : Field_Type;
      begin
         Offset := Field_Type
           (Read_Config8
              (GPA => Device_Base + Field_Cap_Pointer));

         Search :
         loop
            Val := SK.Word16
              (Read_Config16 (GPA => Device_Base + SK.Word64 (Offset)));
            if SK.Byte (Val) = MSI_Cap_ID or else SK.Byte (Val) = MSI_X_Cap_ID
            then
               if SK.Byte (Val) = MSI_Cap_ID then
                  Device.MSI_Cap_Offset   := Offset;
               else
                  Device.MSI_X_Cap_Offset := Offset;
               end if;
               pragma Debug
                 (Debug_Ops.Put_Line
                    (Item => "Pciconf: MSI(X) cap ID "
                     & SK.Strings.Img (SK.Byte (Val)) & " @ offset "
                     & SK.Strings.Img (SK.Byte (Offset))));
               Append_MSI_Config
                 (Offset => Offset,
                  Cap_ID => SK.Byte (Val),
                  Flags  => SK.Word16
                    (Read_Config8
                         (GPA => Device_Base + SK.Word64 (Offset) + 16#02#)));
            end if;

            exit Search when Val / 2 ** 8 = 0;

            Offset := Field_Type (Val / 2 ** 8);
         end loop Search;
      end;

      --  PCI config space quirks.

      Quirks.Register
        (Vendor => SK.Word16 (Read_Config16 (GPA => Device_Base)),
         Device => SK.Word16 (Read_Config16
           (GPA => Device_Base + Field_Device)),
         Class  => SK.Word32
           (Read_Config32
                (GPA => Device_Base + Field_Revision_Class) / 2 ** 8));
   end Init;

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
      RAX      : SK.Word64                  := 0;
      GPA      : constant SK.Word64         := SI.State.Guest_Phys_Addr;
      Dev_Base : constant SK.Word64         := GPA and Base_Mask;
      Offset   : constant Field_Type        := Field_Type (GPA);
      SID      : constant Musinfo.SID_Type  := Musinfo.SID_Type
        (Interfaces.Shift_Right
           (Value  => GPA - Config.MMConf_Base_Address,
            Amount => 12));
      Dev_Info : constant Musinfo.Dev_Info_Type
        := Musinfo.Instance.Device_By_SID (SID => SID);
      Conf     : Config_Entry_Type;
   begin
      Action := Types.Subject_Continue;

      if Dev_Info = Musinfo.Null_Dev_Info then

         --  Set result to 16#ffff# to indicate a non-existent device.

         SI.State.Regs.RAX := 16#ffff#;
         return;
      end if;

      Header := SK.Byte (Read_Config8 (GPA => Dev_Base + Field_Header));
      if Header /= 0 then
         pragma Debug (Debug_Ops.Put_Line
                       (Item => "Pciconf: Unsupported header "
                        & SK.Strings.Img (Header) & " for device with SID "
                        & SK.Strings.Img (SID) & " and base address "
                        & SK.Strings.Img (Dev_Base)));
         return;
      end if;

      if not Device.Initialized then
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Pciconf: Init of device with SID "
               & SK.Strings.Img (SID) & " and base address "
               & SK.Strings.Img (Dev_Base)));
         Init (Device_Base => Dev_Base);
         Device.Initialized := True;
      end if;

      Conf := Get_Config (Offset => Offset);

      if Info.Read then
         pragma Debug (Debug_Ops.Put_String (Item => "Pciconf: Read "));
         declare
            Width : constant Access_Width_Type := Read_Widths
              (SK.Byte (Offset) mod 4);
         begin

            --  Read real value if not fully virtualized.

            if Conf = Null_Config or else Conf.Read_Mask /= Read_All_Virt then
               case Width is
                  when Access_8  => RAX := Read_Config8  (GPA => GPA);
                  when Access_16 => RAX := Read_Config16 (GPA => GPA);
                  when Access_32 => RAX := Read_Config32 (GPA => GPA);
               end case;

               --  Mask out bits as specified by config entry.

               if Conf /= Null_Config and then Conf.Read_Mask /= Read_No_Virt
               then
                  RAX := RAX and SK.Word64 (Conf.Read_Mask);
               end if;
            end if;

            --  Merge in virtualized bits.

            if Conf /= Null_Config and then Conf.Vread /= Vread_None then
               RAX := RAX or Vread
                 (V => Conf.Vread,
                  O => Offset);
            end if;
            pragma Debug (Conf /= Null_Config
                          and Conf.Read_Mask = Read_All_Virt,
                          Debug_Ops.Put_String (Item => "(ALLVIRT)"));
            pragma Debug (Conf /= Null_Config
                          and Conf.Read_Mask /= Read_All_Virt
                          and Conf.Read_Mask /= Read_No_Virt,
                          Debug_Ops.Put_String (Item => "(VIRT)"));
            pragma Debug (Debug_Ops.Put_Line
                          (Item => " @ " & SK.Strings.Img (GPA) & ": "
                           & SK.Strings.Img (RAX)));
            SI.State.Regs.RAX := RAX;
         end;
      end if;

      if Info.Write then
         RAX := SI.State.Regs.RAX;

         pragma Debug
           (Conf /= Null_Config,
            Debug_Ops.Check_Warn_PCI_Write_Width
              (RAX       => RAX,
               Width_Idx => Access_Width_Type'Pos (Conf.Write_Width)));
         pragma Debug (Debug_Ops.Put_String (Item => "Pciconf: Write"));

         if Conf /= Null_Config then
            case Conf.Write_Perm is
               when Write_Denied => null;
               when Write_Direct =>
                  case Conf.Write_Width is
                     when Access_8  => Write_Config8
                          (GPA   => GPA,
                           Value => SK.Byte (RAX));
                     when Access_16 => Write_Config16
                          (GPA   => GPA,
                           Value => SK.Word16 (RAX));
                     when Access_32 => Write_Config32
                          (GPA   => GPA,
                           Value => SK.Word32 (RAX));
                  end case;
               when Write_Virt =>
                  Vwrite (V => Conf.Vwrite,
                          O => Offset);
                  pragma Debug (Debug_Ops.Put_String (Item => " (ALLVIRT)"));
            end case;
         end if;
         pragma Debug (Conf = Null_Config
                       or else Conf.Write_Perm = Write_Denied,
                       Debug_Ops.Put_String (Item => " (DENIED)"));
         pragma Debug (Debug_Ops.Put_Line
                       (Item => " @ " & SK.Strings.Img (GPA) & ": "
                        & SK.Strings.Img (RAX)));
      end if;
   end Emulate;

end Devices.Pciconf;
