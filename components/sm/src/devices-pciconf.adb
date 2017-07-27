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

with Debug_Ops;
with Subject_Info;
with Devices.Pciconf.Quirks;

package body Devices.Pciconf
with
   SPARK_Mode => Off -- Q725-014
is

   package SI renames Subject_Info;

   use type SK.Byte;

   --  See PCI Local Bus Specification Revision 3.0, section 6.1.
   Field_Command         : constant := 16#04#;
   Field_Cache_Line_Size : constant := 16#0c#;
   Field_Latency_Timer   : constant := 16#0d#;
   Field_BIST            : constant := 16#0f#;
   Field_BAR0            : constant := 16#10#;
   Field_BAR1            : constant := 16#14#;
   Field_BAR2            : constant := 16#18#;
   Field_BAR3            : constant := 16#1c#;
   Field_BAR4            : constant := 16#20#;
   Field_BAR5            : constant := 16#24#;
   Field_Cap_Pointer     : constant := 16#34#;

   No_Cap : constant := SK.Byte'Last;

   Null_Config : constant Config_Entry_Type
     := (Offset      => Field_Type'Last,
         Read_Mask   => All_Virt,
         Vread       => Vread_None,
         Write_Mask  => All_Virt,
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
   Config : Config_Array
     := (1      => (Offset      => Field_Command,
                    Read_Mask   => No_Virt,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_16,
                    Vwrite      => Vwrite_None),
         2      => (Offset      => Field_Cache_Line_Size,
                    Read_Mask   => No_Virt,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None),
         3      => (Offset      => Field_Latency_Timer,
                    Read_Mask   => No_Virt,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None),
         4      => (Offset      => Field_BIST,
                    Read_Mask   => No_Virt,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None),
         5      => (Offset      => Field_BAR0,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_BAR,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         6      => (Offset      => Field_BAR1,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_BAR,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         7      => (Offset      => Field_BAR2,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_BAR,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         8      => (Offset      => Field_BAR3,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_BAR,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         9      => (Offset      => Field_BAR4,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_BAR,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         10     => (Offset      => Field_BAR5,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_BAR,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_32,
                    Vwrite      => Vwrite_BAR),
         11     => (Offset      => Field_Cap_Pointer,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_Cap_Pointer,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_8,
                    Vwrite      => Vwrite_None),
         others => Null_Config);

   MSI_Cap_ID   : constant := 16#05#;
   MSI_X_Cap_ID : constant := 16#11#;

   MSI_Cap_Bit_64   : constant := 7;
   MSI_Cap_Bit_Mask : constant := 8;

   subtype Read_Idx_Type is SK.Byte range 0 .. 3;

   Read_Widths : constant array (Read_Idx_Type) of Access_Width_Type
     := (0 => Access_32,
         1 => Access_8,
         2 => Access_16,
         3 => Access_8);

   --  Device state.

   MSI_Cap_Offset   : Field_Type := No_Cap;
   MSI_X_Cap_Offset : Field_Type := No_Cap;

   BARs : BAR_Array := (others => Null_BAR);

   --  Get config entry for given offset.
   function Get_Config (Offset : Field_Type) return Config_Entry_Type
   with
      Global => (Input => Config);

   --  Append config entries for MSI/MSI-X fields starting at given offset with
   --  specified feature flags (64-bit or maskable). See PCI specification 3.0,
   --  sections 6.8.1/6.8.2.
   procedure Append_MSI_Config
     (Offset : Field_Type;
      Cap_ID : SK.Byte;
      Flags  : SK.Word16)
   with
      Global => (In_Out => Config);

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
      --  TODO: Signal full array.

      for Config_Entry of Config loop
         if Config_Entry.Offset = Field_Type'Last then
            Config_Entry := C;
            exit;
         end if;
      end loop;
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
                           Write_Mask  => All_Virt,
                           Write_Width => Access_16,
                           Vwrite      => Vwrite_None));
      Append_Config (C => (Offset      => Offset + 16#02#,
                           Read_Mask   => No_Virt,
                           Vread       => Vread_None,
                           Write_Mask  => No_Virt,
                           Write_Width => Access_16,
                           Vwrite      => Vwrite_None));
      Append_Config (C => (Offset      => Offset + 16#04#,
                           Read_Mask   => No_Virt,
                           Vread       => Vread_None,
                           Write_Mask  => No_Virt,
                           Write_Width => Access_32,
                           Vwrite      => Vwrite_None));

      if Cap_ID = MSI_Cap_ID then
         if SK.Bitops.Bit_Test
           (Value => SK.Word64 (Flags),
            Pos   => MSI_Cap_Bit_64)
         then
            Append_Config (C => (Offset      => Offset + 16#08#,
                                 Read_Mask   => No_Virt,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_32,
                                 Vwrite      => Vwrite_None));
            Append_Config (C => (Offset      => Offset + 16#0c#,
                                 Read_Mask   => No_Virt,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_16,
                                 Vwrite      => Vwrite_None));

            if SK.Bitops.Bit_Test
              (Value => SK.Word64 (Flags),
               Pos   => MSI_Cap_Bit_Mask)
            then
               Append_Config (C => (Offset      => Offset + 16#10#,
                                    Read_Mask   => No_Virt,
                                    Vread       => Vread_None,
                                    Write_Mask  => No_Virt,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));
               Append_Config (C => (Offset      => Offset + 16#14#,
                                    Read_Mask   => No_Virt,
                                    Vread       => Vread_None,
                                    Write_Mask  => No_Virt,
                                    Write_Width => Access_32,
                                    Vwrite      => Vwrite_None));
            end if;
         end if;
      else

         --  MSI-X

         Append_Config (C => (Offset      => Offset + 16#08#,
                              Read_Mask   => No_Virt,
                              Vread       => Vread_None,
                              Write_Mask  => No_Virt,
                              Write_Width => Access_32,
                              Vwrite      => Vwrite_None));
      end if;
   end Append_MSI_Config;

   -------------------------------------------------------------------------

   function Get_Config (Offset : Field_Type) return Config_Entry_Type
   is
      Res : Config_Entry_Type := Null_Config;
   begin
      for C of Config loop
         exit when C = Null_Config;
         if C.Offset = Offset then
            Res := C;
            exit;
         end if;
      end loop;
      return Res;
   end Get_Config;

   -------------------------------------------------------------------------

   function Read_BAR (Offset : Field_Type) return SK.Word32
   is
      Idx : constant Natural := Natural (Offset - 16#10#) / 4;
   begin
      case BARs (Idx).State is
         when BAR_Address => return BARs (Idx).Address;
         when BAR_Size    => return BARs (Idx).Size;
      end case;
   end Read_BAR;

   -------------------------------------------------------------------------

   function Read_Cap_Pointer (Offset : Field_Type) return Field_Type
   is
      pragma Unreferenced (Offset);
   begin
      if MSI_Cap_Offset /= No_Cap then
         return MSI_Cap_Offset;
      elsif MSI_X_Cap_Offset /= No_Cap then
         return MSI_X_Cap_Offset;
      end if;

      return 0;
   end Read_Cap_Pointer;

   -------------------------------------------------------------------------

   function Read_MSI_Cap_ID_Next (Offset : Field_Type) return SK.Word16
   is
      use type SK.Word16;

      Res : SK.Word16 := 0;
   begin
      if Offset = MSI_X_Cap_Offset then
         return MSI_X_Cap_ID;
      else
         if MSI_X_Cap_Offset /= No_Cap then
            Res := SK.Word16 (MSI_X_Cap_Offset) * 2 ** 8;
         end if;

         return Res or SK.Word16 (MSI_Cap_ID);
      end if;
   end Read_MSI_Cap_ID_Next;

   -------------------------------------------------------------------------

   function Vread
     (V : Vread_Type;
      O : Field_Type)
      return SK.Word64
   is
   begin
      case V is
         when Vread_BAR             => return SK.Word64
              (Read_BAR (Offset => O));
         when Vread_Cap_Pointer     => return SK.Word64
              (Read_Cap_Pointer (Offset => O));
         when Vread_MSI_Cap_ID_Next => return SK.Word64
              (Read_MSI_Cap_ID_Next (Offset => O));

         when Vread_None            => return 0;
      end case;
   end Vread;

   -------------------------------------------------------------------------

   procedure Init (Device_Base : SK.Word64)
   is
      use type SK.Word16;
      use type SK.Word64;
   begin
      --  TODO check status if we have caps
      --  TODO make loop bound

      --  BARs

      for I in BARs'Range loop
         declare
            BAR_Addr : constant SK.Word64
              := Device_Base + 16#10# + SK.Word64 (I * 4);
         begin
            BARs (I).Address := SK.Word32 (Read_Config32 (GPA => BAR_Addr));
            Write_Config32 (GPA   => BAR_Addr,
                            Value => SK.Word32'Last);
            BARs (I).Size := SK.Word32 (Read_Config32 (GPA => BAR_Addr));
            Write_Config32 (GPA   => BAR_Addr,
                            Value => BARs (I).Address);
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "PCICONF BAR" & SK.Strings.Img_Nobase (SK.Byte (I))
                  & " address " & SK.Strings.Img (BARs (I).Address)
                  & " size " & SK.Strings.Img (BARs (I).Size)));
         end;
      end loop;

      --  Caps

      declare
         Val    : SK.Word16;
         Offset : Field_Type;
      begin
         Offset := Field_Type (Read_Config8 (GPA => Device_Base + 16#34#));

         Search :
         loop
            Val := SK.Word16
              (Read_Config16 (GPA => Device_Base + SK.Word64 (Offset)));
            if SK.Byte (Val) = MSI_Cap_ID or else SK.Byte (Val) = MSI_X_Cap_ID
            then
               if SK.Byte (Val) = MSI_Cap_ID then
                  MSI_Cap_Offset   := Offset;
               else
                  MSI_X_Cap_Offset := Offset;
               end if;
               pragma Debug
                 (Debug_Ops.Put_Line
                    (Item => "PCICONF MSI(X) cap ID "
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
         Device => SK.Word16 (Read_Config16 (GPA => Device_Base + 16#02#)),
         Class  => SK.Word32
           (Read_Config32 (GPA => Device_Base + 16#08#) / 2 ** 8));
   end Init;

   -------------------------------------------------------------------------

   procedure Mediate
     (Info   :     Types.EPTV_Info_Type;
      Action : out Types.Subject_Action_Type)
   is
      use type SK.Word32;
      use type SK.Word64;

      RAX    : SK.Word64                  := 0;
      GPA    : constant SK.Word64         := SI.State.Guest_Phys_Addr;
      Offset : constant Field_Type        := Field_Type (GPA);
      Conf   : constant Config_Entry_Type := Get_Config
        (Offset => Offset);
   begin
      Action := Types.Subject_Continue;

      if Info.Read then
         pragma Debug (Debug_Ops.Put_String (Item => "PCICONF read "));
         declare
            Width : constant Access_Width_Type := Read_Widths
              (SK.Byte (Offset) mod 4);
         begin

            --  Read real value if not fully virtualized.

            if Conf = Null_Config or else Conf.Read_Mask /= All_Virt then
               case Width is
                  when Access_8  => RAX := Read_Config8  (GPA => GPA);
                  when Access_16 => RAX := Read_Config16 (GPA => GPA);
                  when Access_32 => RAX := Read_Config32 (GPA => GPA);
               end case;

               --  Mask out bits as specified by config entry.

               if Conf /= Null_Config and then Conf.Read_Mask /= No_Virt then
                  RAX := RAX and SK.Word64 (Conf.Read_Mask);
               end if;
            end if;

            --  Merge in virtualized bits.

            if Conf /= Null_Config and then Conf.Vread /= Vread_None then
               RAX := RAX or Vread
                 (V => Conf.Vread,
                  O => Offset);
            end if;
            pragma Debug (Conf /= Null_Config and Conf.Read_Mask = All_Virt,
                          Debug_Ops.Put_String (Item => "(ALLVIRT)"));
            pragma Debug (Conf /= Null_Config
                          and Conf.Read_Mask /= All_Virt
                          and Conf.Read_Mask /= No_Virt,
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
         pragma Debug (Debug_Ops.Put_String (Item => "PCICONF write"));

         if Conf /= Null_Config then
            if Conf.Write_Mask /= All_Virt then
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
            else

               --  Call virtual write procedure.

               Vwrite (V => Conf.Vwrite,
                       O => Offset);
               pragma Debug (Debug_Ops.Put_String (Item => " (ALLVIRT)"));
            end if;
         end if;
         pragma Debug (Conf = Null_Config,
                       Debug_Ops.Put_String (Item => " (DENIED)"));
         pragma Debug (Debug_Ops.Put_Line
                       (Item => " @ " & SK.Strings.Img (GPA) & ": "
                        & SK.Strings.Img (RAX)));
      end if;
   end Mediate;

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

   procedure Write_BAR (Offset : Field_Type)
   is
      use type SK.Word32;

      Idx : constant Natural   := Natural (Offset - 16#10#) / 4;
      RAX : constant SK.Word64 := SI.State.Regs.RAX;
   begin
      if SK.Word32 (RAX) = SK.Word32'Last then
         BARs (Idx).State := BAR_Size;
      else
         BARs (Idx).State := BAR_Address;
      end if;
   end Write_BAR;

end Devices.Pciconf;
