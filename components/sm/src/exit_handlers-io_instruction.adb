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
      Reserved     : Bit_Array (1 .. 9);
      Port_Number  : SK.Word16;
   end record
     with Size => 64;

   for IO_Info_Type use record
      Size         at 0 range  0 ..  2;
      Direction    at 0 range  3 ..  3;
      String_Instr at 0 range  4 ..  4;
      REP_Prefixed at 0 range  5 ..  5;
      Op_Encoding  at 0 range  6 ..  6;
      Reserved     at 0 range  7 .. 15;
      Port_Number  at 0 range 16 .. 31;
   end record;

   -------------------------------------------------------------------------

   --  Ignore acces to port: Do nothing on write, fake read.
   procedure Ignore_Access (Info : IO_Info_Type)
   with
      Global  => (In_Out => Subject_Info.State),
      Depends => (Subject_Info.State =>+ Info)
   is
      use type SK.Word64;

      RAX  : constant SK.Word64 := State.Regs.RAX;
      Mask : SK.Word64          := 0;
   begin
      pragma Debug (Subject.Text_IO.Put_Word16
                    (Item => Info.Port_Number));
      pragma Debug (Subject.Text_IO.Put_String (Item => " ignore "));

      case Info.Size is
         when One_Byte  => Mask := 16#ff#;
         when Two_Byte  => Mask := 16#ffff#;
         when Four_Byte => Mask := 16#ffff_ffff#;
         when others    => null;
      end case;

      if Info.Direction = Dir_In then
         State.Regs.RAX := RAX or Mask;
      end if;

      pragma Debug (Info.Direction = Dir_In,
                    Subject.Text_IO.Put_String (Item => "read"));
      pragma Debug (Info.Direction = Dir_Out,
                    Subject.Text_IO.Put_String (Item => "write "));
      pragma Debug (Info.Direction = Dir_Out,
                    Subject.Text_IO.Put_Word32 (SK.Word32'Mod (RAX and Mask)));
      pragma Debug (Subject.Text_IO.New_Line);
   end Ignore_Access;

   -------------------------------------------------------------------------

   --  Emulate i8042 controller.
   procedure Emulate_i8042
     (Info :     IO_Info_Type;
      Halt : out Boolean)
   with
      Global  => (In_Out => Subject_Info.State),
      Depends => (Subject_Info.State =>+ Info,
                  Halt => (Info, Subject_Info.State))
   is
      use type SK.Byte;
      use type SK.Word16;
      use type SK.Word64;

      RAX : constant SK.Word64 := State.Regs.RAX;
   begin
      Halt := False;

      if Info.Port_Number = 16#64#
        and then Info.Direction = Dir_Out
        and then SK.Byte'Mod (RAX) = 16#fe#
      then
         pragma Debug
           (Subject.Text_IO.Put_Line
              (Item => "Reboot requested via pulse of CPU RESET pin"));
         Halt := True;
      elsif Info.Port_Number = 16#64# and Info.Direction = Dir_In then
         State.Regs.RAX := RAX and not 16#ff#;
      end if;
   end Emulate_i8042;

   -------------------------------------------------------------------------

   --  Return I/O instruction information from exit qualification, as specified
   --  by Intel SDM Vol. 3C, section 27.2.1, table 27-5.
   function To_IO_Info (Qualification : SK.Word64) return IO_Info_Type
   is
      function To_IO_Information is new Ada.Unchecked_Conversion
        (Source => SK.Word64,
         Target => IO_Info_Type);
   begin
      return To_IO_Information (Qualification);
   end To_IO_Info;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      Info   : IO_Info_Type;
      Exit_Q : constant SK.Word64 := State.Exit_Qualification;
   begin
      Halt := False;

      Info := To_IO_Info (Qualification => Exit_Q);

      if Info.String_Instr or Info.REP_Prefixed then
         pragma Debug
           (Subject.Text_IO.Put_Line
              (Item => "I/O instructions with string and REP not supported"));
         Halt := True;
      elsif Info.Size not in One_Byte | Two_Byte | Four_Byte then
         pragma Debug
           (Subject.Text_IO.Put_Line
              (Item => "I/O instruction with invalid access size 16#"));
         pragma Debug (Subject.Text_IO.Put_Byte (Item => SK.Byte (Info.Size)));
         pragma Debug (Subject.Text_IO.Put_Line (Item => "#"));
         Halt := True;
      else
         case Info.Port_Number is
            when 16#0020# |  --  PIC_MASTER_CMD   (hardcoded)
                 16#0021# |  --  PIC_MASTER_DATA  (hardcoded)
                 16#00a0# |  --  PIC_SLAVE_CMD    (hardcoded)
                 16#00a1# |  --  PIC_SLAVE_DATA   (hardcoded)
                 16#0040# |  --  i8253/4 PIT_CH0  (hardcoded)
                 16#0043# |  --  i8253/4 PIT_MODE (hardcoded)
                 16#0080# |  --  PORT80           (hardcoded)
                 16#02e9# |  --  COM 4            (configurable)
                 16#02f9# |  --  COM 2            (configurable)
                 16#02fa# |  --  82C710 C&T mouse port chip   (conf.)
                 16#0390# |  --  82C710 C&T mouse port chip   (conf.)
                 16#0391# |  --  82C710 C&T mouse port chip   (conf.)
                 16#03e9# |  --  COM 3            (configurable)
                 16#03f9# |  --  COM 1            (configurable)
                 16#03fa# |  --  82C710 C&T mouse port chip   (conf.)
                 16#04d0# |  --  PIC_ELCR1        (hardcoded,ACPI)
                 16#04d1# |  --  PIC_ELCR2        (hardcoded,ACPI)
                 16#0cf8# |  --  PCI Addr         (hardcoded)
                 16#0cf9# |  --  PCI Addr         (hardcoded)
                 16#0cfa# |  --  PCI Addr         (hardcoded)
                 16#0cfb# |  --  PCI Addr         (hardcoded)
                 16#0cfc# |  --  PCI Data         (hardcoded)
                 16#0cfd# |  --  PCI Data         (hardcoded)
                 16#0cfe# |  --  PCI Data         (hardcoded)
                 16#0cff# => --  PCI Data         (hardcoded)
               Ignore_Access (Info => Info);
            when 16#60# | 16#64# =>
               Emulate_i8042 (Info => Info,
                              Halt => Halt);
            when others =>
               pragma Debug (Subject.Text_IO.Put_String
                             (Item => "Unhandled access to I/O port "));
               pragma Debug (Subject.Text_IO.Put_Word16
                             (Item => Info.Port_Number));
               pragma Debug (Subject.Text_IO.New_Line);
               Halt := True;
         end case;
      end if;
   end Process;

end Exit_Handlers.IO_Instruction;
