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

with SK.Strings;

with Debug_Ops;
with Subject_Info;

package body Devices.Pciconf
with
   SPARK_Mode => Off -- Q725-014
is

   package SI renames Subject_Info;

   subtype Field_Type is SK.Byte;

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

   type Access_Width_Type is
     (Access_8,
      Access_16,
      Access_32);

   MSI_Cap_Offset   : SK.Byte := SK.Byte'Last;
   MSI_X_Cap_Offset : SK.Byte := SK.Byte'Last;

   No_Virt : constant := SK.Byte'Last;

   --  Virtual read function config.
   type Vread_Type is
     (Vread_None,
      Vread_Cap_Pointer);

   type Config_Entry_Type is record
      Offset    : Field_Type;
      Width     : Access_Width_Type;
      Read_Mask : SK.Word32;
   end record;

   Null_Config : constant Config_Entry_Type
     := (Offset    => Field_Type'Last,
         Width     => Access_8,
         Read_Mask => 0);

   type Config_Array is array (1 .. 24) of Config_Entry_Type;

   --  Contains pre-defined read/write rules, room for MSI/MSI-X capability
   --  handling and PCI quirks for certain devices.
   Config : Config_Array
     := (1      => (Offset    => Field_Command,
                    Width     => Access_16,
                    Read_Mask => SK.Word32'Last),
         2      => (Offset    => Field_Cache_Line_Size,
                    Width     => Access_8,
                    Read_Mask => SK.Word32'Last),
         3      => (Offset    => Field_Latency_Timer,
                    Width     => Access_8,
                    Read_Mask => SK.Word32'Last),
         4      => (Offset    => Field_BIST,
                    Width     => Access_8,
                    Read_Mask => SK.Word32'Last),
         5      => (Offset    => Field_BAR0,
                    Width     => Access_32,
                    Read_Mask => SK.Word32'Last),
         6      => (Offset    => Field_BAR1,
                    Width     => Access_32,
                    Read_Mask => SK.Word32'Last),
         7      => (Offset    => Field_BAR2,
                    Width     => Access_32,
                    Read_Mask => SK.Word32'Last),
         8      => (Offset    => Field_BAR3,
                    Width     => Access_32,
                    Read_Mask => SK.Word32'Last),
         9      => (Offset    => Field_BAR4,
                    Width     => Access_32,
                    Read_Mask => SK.Word32'Last),
         10     => (Offset    => Field_BAR5,
                    Width     => Access_32,
                    Read_Mask => SK.Word32'Last),
         11     => (Offset    => Field_BAR5,
                    Width     => Access_32,
                    Read_Mask => SK.Word32'Last),
         others => Null_Config);

   MSI_Next_Mask : constant := 16#ffff_00ff#;

   MSI_Pci_Cap_ID   : constant := 16#05#;
   MSI_X_Pci_Cap_ID : constant := 16#11#;

   subtype Read_Idx_Type is SK.Byte range 0 .. 3;

   Read_Widths : constant array (Read_Idx_Type) of Access_Width_Type
     := (0 => Access_32,
         1 => Access_16,
         2 => Access_16,
         3 => Access_8);

   --  Get config entry for given offset.
   function Get_Config (Offset : Field_Type) return Config_Entry_Type
   with
      Global => (Input => Config);

   --  Append new config entry.
   procedure Append_Config (W : Config_Entry_Type)
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

   -------------------------------------------------------------------------

   procedure Append_Config (W : Config_Entry_Type)
   is
      use type Field_Type;
   begin
      --  TODO: Signal full array.

      for C of Config loop
         if C.Offset = Field_Type'Last then
            C := W;
            exit;
         end if;
      end loop;
   end Append_Config;

   -------------------------------------------------------------------------

   function Get_Config (Offset : Field_Type) return Config_Entry_Type
   is
      use type Field_Type;

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

   procedure Init (Device_Base : SK.Word64)
   is
      use type SK.Word16;
      use type SK.Word64;
      use type Field_Type;

      Val    : SK.Word16;
      Offset : SK.Byte;
   begin
      --  TODO check status if we have caps
      --  TODO make loop bound

      Offset := SK.Byte (Read_Config8 (GPA => Device_Base + 16#34#));

      Search :
      loop
         Val := SK.Word16
           (Read_Config16 (GPA => Device_Base + SK.Word64 (Offset)));
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "PCICONF cap is " & SK.Strings.Img (Val)));
         if SK.Byte (Val) = MSI_Pci_Cap_ID then
            MSI_Cap_Offset := Offset;
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "PCICONF MSI cap @ offset "
                  & SK.Strings.Img (MSI_Cap_Offset)));
            Append_Config (W => (Offset    => Offset,
                                 Width     => Access_8,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#01#,
                                 Width     => Access_8,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#02#,
                                 Width     => Access_16,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#04#,
                                 Width     => Access_32,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#08#,
                                 Width     => Access_16,
                                 Read_Mask => SK.Word32'Last));
         elsif SK.Byte (Val) = MSI_X_Pci_Cap_ID then
            MSI_X_Cap_Offset := Offset;
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "PCICONF MSI X cap @ offset "
                  & SK.Strings.Img (MSI_X_Cap_Offset)));
            Append_Config (W => (Offset    => Offset,
                                 Width     => Access_8,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#01#,
                                 Width     => Access_8,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#02#,
                                 Width     => Access_16,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#04#,
                                 Width     => Access_32,
                                 Read_Mask => SK.Word32'Last));
            Append_Config (W => (Offset    => Offset + 16#08#,
                                 Width     => Access_32,
                                 Read_Mask => SK.Word32'Last));
         end if;

         exit Search when Val / 2 ** 8 = 0;

         Offset := SK.Byte (Val / 2 ** 8);
      end loop Search;
   end Init;

   -------------------------------------------------------------------------

   procedure Mediate
     (Info   :     Types.EPTV_Info_Type;
      Action : out Types.Subject_Action_Type)
   is
      use type SK.Byte;
      use type SK.Word32;
      use type SK.Word64;

      RAX    : SK.Word64;
      GPA    : constant SK.Word64         := SI.State.Guest_Phys_Addr;
      Offset : constant SK.Byte           := SK.Byte (GPA);
      Conf   : constant Config_Entry_Type := Get_Config
        (Offset => Offset);
   begin
      Action := Types.Subject_Continue;

      if Info.Read then
         declare
            Width : constant Access_Width_Type := Read_Widths (Offset mod 4);
         begin
            RAX := 0;

            case Offset is
               when Field_Cap_Pointer =>
                  RAX := SK.Word64 (MSI_Cap_Offset);
               when others =>
                  case Width is
                     when Access_8  => RAX := Read_Config8  (GPA => GPA);
                     when Access_16 => RAX := Read_Config16 (GPA => GPA);
                     when Access_32 => RAX := Read_Config32 (GPA => GPA);
                  end case;
            end case;

            if Conf /= Null_Config and then Conf.Read_Mask /= No_Virt then
               RAX := RAX and SK.Word64 (Conf.Read_Mask);
            end if;

            --  Cap virtualization.

            if Offset = MSI_Cap_Offset then
               RAX := RAX and MSI_Next_Mask;
               if MSI_X_Cap_Offset /= SK.Byte'Last then
                  RAX := RAX or 16#a0# * 2 ** 8;
               end if;
            end if;

            if Offset = MSI_X_Cap_Offset then
               RAX := RAX and MSI_Next_Mask;
            end if;
            pragma Debug (Debug_Ops.Put_Line
                          (Item => "PCICONF read "
                           & "@ " & SK.Strings.Img (GPA) & ": "
                           & SK.Strings.Img (RAX)));
            SI.State.Regs.RAX := RAX;
         end;
      end if;

      if Info.Write then
         if Conf /= Null_Config then
            RAX := SI.State.Regs.RAX;
            pragma Debug (Debug_Ops.Put_Line
                          (Item => "PCICONF write "
                           & "@ " & SK.Strings.Img (GPA) & ": "
                           & SK.Strings.Img (RAX)));
            case Conf.Width is
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
         end if;
         pragma Debug
           (Conf = Null_Config,
            Debug_Ops.Put_Line
              (Item => "PCICONF write (DENIED) "
               & "@ " & SK.Strings.Img (GPA) & ": "
               & SK.Strings.Img (RAX)));
      end if;
   end Mediate;

end Devices.Pciconf;
