--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Unchecked_Conversion;

with SK;

with Subject.Text_IO;

with Subject_Info;

package body Exit_Handlers.IO_Instruction
is

   use Subject_Info;

   --  Types related to I/O instruction specific exit qualification.

   type Access_Size_Type is mod 2 ** 3
     with Size => 3;

   One_Byte  : constant Access_Size_Type := 0;
   Two_Byte  : constant Access_Size_Type := 1;
   Four_Byte : constant Access_Size_Type := 3;

   type Direction_Type is (Dir_Out, Dir_In)
     with Size => 1;
   for Direction_Type use
     (Dir_Out => 0,
      Dir_In  => 1);

   type Operand_Encoding_Type is (DX, Immediate)
     with Size => 1;
   for Operand_Encoding_Type use
     (DX        => 0,
      Immediate => 1);

   type IO_Info_Type is record
      Size         : Access_Size_Type;
      Direction    : Direction_Type;
      String_Instr : Boolean;
      REP_Prefixed : Boolean;
      Op_Encoding  : Operand_Encoding_Type;
      Port_Number  : SK.Word16;
   end record
     with Size => 64;

   for IO_Info_Type use record
      Size         at 0 range  0 ..  2;
      Direction    at 0 range  3 ..  3;
      String_Instr at 0 range  4 ..  4;
      REP_Prefixed at 0 range  5 ..  5;
      Op_Encoding  at 0 range  6 ..  6;
      Port_Number  at 0 range 16 .. 31;
   end record;

   --  Return I/O instruction information from exit qualification, as specified
   --  by Intel SDM Vol. 3C, section 27.2.1, table 27-5.
   function To_IO_Info (Qualification : SK.Word64) return IO_Info_Type;

   --  Returns True if the I/O operation is a reboot request.
   function Is_Reboot_Request (Info : IO_Info_Type) return Boolean;

   -------------------------------------------------------------------------

   function Is_Reboot_Request (Info : IO_Info_Type) return Boolean
   is
      use type SK.Byte;
      use type SK.Word16;
   begin
      return Info.Port_Number = 16#64#
        and then Info.Direction = Dir_Out
        and then SK.Byte'Mod (State.Regs.RAX) = 16#fe#;
   end Is_Reboot_Request;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      use type SK.Word64;

      Info : IO_Info_Type;
   begin
      Halt := False;

      Info := To_IO_Info (Qualification => State.Exit_Qualification);

      if Info.String_Instr or Info.REP_Prefixed then
         Subject.Text_IO.Put_Line
           (Item => "I/O instructions with string and REP not supported");
         Halt := True;
      else
         case Info.Size is
            when One_Byte =>
               case Info.Port_Number is
                  --  Only handle these ports by now:
                  when 16#20#  |    --  PIC_MASTER_CMD      (hardcoded)
                       16#21#  |    --  PIC_MASTER_DATA     (hardcoded)
                       16#a0#  |    --  PIC_SLAVE_CMD       (hardcoded)
                       16#a1#  |    --  PIC_SLAVE_DATA      (hardcoded)
                       16#40#  |    --  i8253/4 PIT_CH0     (hardcoded)
                       16#43#  |    --  i8253/4 PIT_MODE    (hardcoded)
                       16#60#  |    --  i8042 DATA          (configurable)
                       16#64#  |    --  i8042 CMD/STATUS    (configurable)
                       16#80#  |    --  PORT80              (hardcoded)
                       16#2e9# |    --  COM 4               (configurable)
                       16#2f9# |    --  COM 2               (configurable)
                       16#2fa# |    --  82C710 C&T mouse port chip   (conf.)
                       16#390# |    --  82C710 C&T mouse port chip   (conf.)
                       16#391# |    --  82C710 C&T mouse port chip   (conf.)
                       16#3e9# |    --  COM 3               (configurable)
                       16#3f9# |    --  COM 1               (configurable)
                       16#3fa# |    --  82C710 C&T mouse port chip   (conf.)
                       16#4d0# |    --  PIC_ELCR1           (hardcoded,ACPI)
                       16#4d1# |    --  PIC_ELCR2           (hardcoded,ACPI)
                       16#cf8# |    --  PCI Addr            (hardcoded)
                       16#cf9# |    --  PCI Addr            (hardcoded)
                       16#cfa# |    --  PCI Addr            (hardcoded)
                       16#cfb# |    --  PCI Addr            (hardcoded)
                       16#cfc# |    --  PCI Data            (hardcoded)
                       16#cfd# |    --  PCI Data            (hardcoded)
                       16#cfe# |    --  PCI Data            (hardcoded)
                       16#cff# =>   --  PCI Data            (hardcoded)
                     --  ignore writes, read 00/ff

                     Subject.Text_IO.Put_Word16
                       (Item => Info.Port_Number);
                     Subject.Text_IO.Put_String (Item => " ");

                     case Info.Direction is
                        when Dir_In =>
                           Subject.Text_IO.Put_String (Item => "read.");
                           case Info.Port_Number is
                              when others =>
                                 State.Regs.RAX := State.Regs.RAX or 16#ff#;
                           end case;
                        when Dir_Out =>
                           Subject.Text_IO.Put_String (Item => "write: ");
                           Subject.Text_IO.Put_Byte
                             (Item => SK.Byte (State.Regs.RAX and 16#ff#));
                     end case;
                     Subject.Text_IO.New_Line;
                  when others =>
                     Subject.Text_IO.Put_String
                       (Item => "Unhandled byte access to I/O port ");
                     Subject.Text_IO.Put_Word16 (Item => Info.Port_Number);
                     Subject.Text_IO.New_Line;
                     Halt := True;
               end case;
            when Two_Byte =>
               case Info.Port_Number is
                  --  Only handle these ports by now:
                  when 16#0cf8# |   --  PCI Addr            (hardcoded)
                       16#0cfa# |   --  PCI Addr            (hardcoded)
                       16#0cfc# |   --  PCI Data            (hardcoded)
                       16#0cfe# =>  --  PCI Data            (hardcoded)
                     Subject.Text_IO.Put_String (Item => " ");
                     Subject.Text_IO.Put_Word16 (Item => Info.Port_Number);
                     Subject.Text_IO.Put_String (Item => "  ");
                     case Info.Direction is
                        when Dir_In =>
                           Subject.Text_IO.Put_String (Item => "read.");
                           State.Regs.RAX := State.Regs.RAX or 16#ffff#;
                        when Dir_Out =>
                           Subject.Text_IO.Put_String (Item => "write: ");
                           Subject.Text_IO.Put_Word16
                             (Item => SK.Word16 (State.Regs.RAX and 16#ffff#));
                     end case;
                     Subject.Text_IO.New_Line;
                  when others =>
                     Subject.Text_IO.Put_String
                       (Item => "Unhandled word16 access to I/O port ");
                     Subject.Text_IO.Put_Word16 (Item => Info.Port_Number);
                     Subject.Text_IO.New_Line;
                     Halt := True;
               end case;
            when Four_Byte =>
               case Info.Port_Number is
                  --  Only handle these ports by now:
                  when 16#0cf8# |   --  PCI Addr            (hardcoded)
                       16#0cfc# =>  --  PCI Data            (hardcoded)
                     Subject.Text_IO.Put_String (Item => " ");
                     Subject.Text_IO.Put_Word16 (Item => Info.Port_Number);
                     Subject.Text_IO.Put_String (Item => "  ");
                     case Info.Direction is
                        when Dir_In =>
                           Subject.Text_IO.Put_String (Item => "read.");
                           State.Regs.RAX := State.Regs.RAX or 16#ffff_ffff#;
                        when Dir_Out =>
                           Subject.Text_IO.Put_String (Item => "write: ");
                           Subject.Text_IO.Put_Word32
                             (Item => SK.Word32
                                (State.Regs.RAX and 16#ffff_ffff#));
                     end case;
                     Subject.Text_IO.New_Line;
                  when others =>
                     Subject.Text_IO.Put_String
                       (Item => "Unhandled word32 access to I/O port ");
                     Subject.Text_IO.Put_Word16 (Item => Info.Port_Number);
                     Subject.Text_IO.New_Line;
                     Halt := True;
               end case;
            when others =>
               Subject.Text_IO.Put_String
                 (Item => "I/O instruction with invalid access size 16#");
               Subject.Text_IO.Put_Byte (Item => SK.Byte (Info.Size));
               Subject.Text_IO.Put_Line (Item => "#");
               Halt := True;
         end case;

         if Is_Reboot_Request (Info => Info) then
            Subject.Text_IO.Put_Line
              (Item => "Reboot requested via pulse of CPU RESET pin");
            Halt := True;
         end if;
      end if;
   end Process;

   -------------------------------------------------------------------------

   function To_IO_Info (Qualification : SK.Word64) return IO_Info_Type
   is
      function To_IO_Information is new Ada.Unchecked_Conversion
        (Source => SK.Word64,
         Target => IO_Info_Type);
   begin
      return To_IO_Information (Qualification);
   end To_IO_Info;

end Exit_Handlers.IO_Instruction;
