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

   use type SK.Byte;

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

   No_Cap : constant := SK.Byte'Last;

   MSI_Cap_Offset   : SK.Byte := No_Cap;
   MSI_X_Cap_Offset : SK.Byte := No_Cap;

   All_Virt : constant := SK.Byte'First;
   No_Virt  : constant := SK.Byte'Last;

   --  Virtual read function config.
   type Vread_Type is
     (Vread_None,
      Vread_Cap_Pointer,
      Vread_MSI_Cap_ID_Next,
      Vread_MSI_X_Cap_ID_Next);

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
   end record;

   Null_Config : constant Config_Entry_Type
     := (Offset      => Field_Type'Last,
         Read_Mask   => All_Virt,
         Vread       => Vread_None,
         Write_Mask  => All_Virt,
         Write_Width => Access_8);

   type Config_Array is array (1 .. 24) of Config_Entry_Type;

   --  Contains pre-defined read/write rules, room for MSI/MSI-X capability
   --  handling and PCI quirks for certain devices.
   Config : Config_Array
     := (1      => (Offset      => Field_Command,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_16),
         2      => (Offset      => Field_Cache_Line_Size,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_8),
         3      => (Offset      => Field_Latency_Timer,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_8),
         4      => (Offset      => Field_BIST,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_8),
         5      => (Offset      => Field_BAR0,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_32),
         6      => (Offset      => Field_BAR1,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_32),
         7      => (Offset      => Field_BAR2,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_32),
         8      => (Offset      => Field_BAR3,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_32),
         9      => (Offset      => Field_BAR4,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_32),
         10     => (Offset      => Field_BAR5,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_32),
         11     => (Offset      => Field_BAR5,
                    Read_Mask   => SK.Word32'Last,
                    Vread       => Vread_None,
                    Write_Mask  => No_Virt,
                    Write_Width => Access_32),
         12     => (Offset      => Field_Cap_Pointer,
                    Read_Mask   => All_Virt,
                    Vread       => Vread_Cap_Pointer,
                    Write_Mask  => All_Virt,
                    Write_Width => Access_8),
         others => Null_Config);

   MSI_Cap_ID   : constant := 16#05#;
   MSI_X_Cap_ID : constant := 16#11#;

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

   --  Perform virtualized read operation.
   function Vread (F : Vread_Type) return SK.Word64;

   --  Return virtualized capability pointer value.
   function Read_Cap_Pointer return SK.Byte;

   --  Return virtualized MSI cap ID and next pointer.
   function Read_MSI_Cap_ID_Next return SK.Word16;

   --  Return virtualized MSI-X cap ID and next pointer.
   function Read_MSI_X_Cap_ID_Next return SK.Word16 is (MSI_X_Cap_ID);

   -------------------------------------------------------------------------

   procedure Append_Config (W : Config_Entry_Type)
   is
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

   function Read_Cap_Pointer return SK.Byte
   is
   begin
      if MSI_Cap_Offset /= No_Cap then
         return MSI_Cap_Offset;
      elsif MSI_X_Cap_Offset /= No_Cap then
         return MSI_X_Cap_Offset;
      end if;

      return 0;
   end Read_Cap_Pointer;

   -------------------------------------------------------------------------

   function Read_MSI_Cap_ID_Next return SK.Word16
   is
      use type SK.Word16;

      Res : SK.Word16 := 0;
   begin
      if MSI_X_Cap_Offset /= No_Cap then
         Res := SK.Word16 (MSI_X_Cap_Offset) * 2 ** 8;
      end if;

      return Res or SK.Word16 (MSI_Cap_ID);
   end Read_MSI_Cap_ID_Next;

   -------------------------------------------------------------------------

   function Vread (F : Vread_Type) return SK.Word64
   is
   begin
      case F is
         when Vread_Cap_Pointer       => return SK.Word64 (Read_Cap_Pointer);
         when Vread_MSI_Cap_ID_Next   => return SK.Word64
              (Read_MSI_Cap_ID_Next);
         when Vread_MSI_X_Cap_ID_Next => return SK.Word64
              (Read_MSI_X_Cap_ID_Next);
         when Vread_None              => return 0;
      end case;
   end Vread;

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
         if SK.Byte (Val) = MSI_Cap_ID then
            MSI_Cap_Offset := Offset;
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "PCICONF MSI cap @ offset "
                  & SK.Strings.Img (MSI_Cap_Offset)));
            Append_Config (W => (Offset      => Offset,
                                 Read_Mask   => 16#ffff_0000#,
                                 Vread       => Vread_MSI_Cap_ID_Next,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_8));
            Append_Config (W => (Offset      => Offset + 16#01#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_8));
            Append_Config (W => (Offset      => Offset + 16#02#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_16));
            Append_Config (W => (Offset      => Offset + 16#04#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_32));
            Append_Config (W => (Offset      => Offset + 16#08#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_16));
         elsif SK.Byte (Val) = MSI_X_Cap_ID then
            MSI_X_Cap_Offset := Offset;
            pragma Debug
              (Debug_Ops.Put_Line
                 (Item => "PCICONF MSI X cap @ offset "
                  & SK.Strings.Img (MSI_X_Cap_Offset)));
            Append_Config (W => (Offset      => Offset,
                                 Read_Mask   => 16#ffff_0000#,
                                 Vread       => Vread_MSI_X_Cap_ID_Next,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_8));
            Append_Config (W => (Offset      => Offset + 16#01#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_8));
            Append_Config (W => (Offset      => Offset + 16#02#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_16));
            Append_Config (W => (Offset      => Offset + 16#04#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_32));
            Append_Config (W => (Offset      => Offset + 16#08#,
                                 Read_Mask   => SK.Word32'Last,
                                 Vread       => Vread_None,
                                 Write_Mask  => No_Virt,
                                 Write_Width => Access_32));
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
               RAX := RAX or Vread (F => Conf.Vread);
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
