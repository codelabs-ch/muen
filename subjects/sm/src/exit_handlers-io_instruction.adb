--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

with Subject.Text_IO;

with Subject_Info;

package body Exit_Handlers.IO_Instruction
is

   use Subject_Info;

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
      use type SK.Word64;
   begin
      Halt := False;

      if (State.Exit_Qualification and 48) /= 0 then
         Subject.Text_IO.Put_Line
           ("I/O instructions with string and REP not supported");
         Halt := True;
      else
         case State.Exit_Qualification and 7 is --  Size of access
            when 0 =>      --  1-byte
               case State.Exit_Qualification / 2 ** 16 is --  Port number
                  --  Only handle these ports by now:
                  when 16#20#  |    --  PIC_MASTER_CMD      (hardcoded)
                       16#21#  |    --  PIC_MASTER_DATA     (hardcoded)
                       16#a0#  |    --  PIC_SLAVE_CMD       (hardcoded)
                       16#a1#  |    --  PIC_SLAVE_DATA      (hardcoded)
                       16#40#  |    --  i8253/4 PIT_CH0     (hardcoded)
                       16#43#  |    --  i8253/4 PIT_MODE    (hardcoded)
                       16#60#  |    --  i8042 DATA          (configurable)
                       16#64#  |    --  i8042 CMD/STATUS    (configurable)
                       16#70#  |    --  RTC CMD             (hardcoded)
                       16#71#  |    --  RTC DATA            (hardcoded)
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
                     case State.Exit_Qualification / 2 ** 16 is
                        when 16#40# =>
                           Subject.Text_IO.Put_String
                             ("(40) i8253/4 PIT_CH0  ");
                        when 16#43# =>
                           Subject.Text_IO.Put_String
                             ("(43) i8253/4 PIT_MODE ");
                        when 16#70# =>
                           Subject.Text_IO.Put_String ("(70) RTC CMD  ");
                        when 16#71# =>
                           Subject.Text_IO.Put_String ("(71) RTC DATA ");
                        when others =>
                           Subject.Text_IO.Put_String (" ");
                           Subject.Text_IO.Put_Word16
                             (SK.Word16 ((State.Exit_Qualification / 2 ** 16)
                              and 16#ffff#));
                           Subject.Text_IO.Put_String ("  ");
                     end case;
                     if ((State.Exit_Qualification / 2 ** 3) and 1) = 1 then
                        Subject.Text_IO.Put_String ("read.");
                        case State.Exit_Qualification / 2 ** 16 is
                           when 16#70# | 16#71# =>
                              State.Regs.RAX :=
                                State.Regs.RAX and not 16#ff#;
                           when others =>
                              State.Regs.RAX := State.Regs.RAX or 16#ff#;
                        end case;
                     else
                        Subject.Text_IO.Put_String ("write: ");
                        Subject.Text_IO.Put_Byte
                          (Item => SK.Byte (State.Regs.RAX and 16#ff#));
                     end if;
                     Subject.Text_IO.New_Line;
                  when others =>
                     Halt := True;
               end case;
            when 1 =>      --  2-byte
               case State.Exit_Qualification / 2 ** 16 is --  Port number
                  --  Only handle these ports by now:
                  when 16#0cf8# |   --  PCI Addr            (hardcoded)
                       16#0cfa# |   --  PCI Addr            (hardcoded)
                       16#0cfc# |   --  PCI Data            (hardcoded)
                       16#0cfe# =>  --  PCI Data            (hardcoded)
                     Subject.Text_IO.Put_String (" ");
                     Subject.Text_IO.Put_Word16
                       (SK.Word16 ((State.Exit_Qualification / 2 ** 16) and
                          16#ffff#));
                     Subject.Text_IO.Put_String ("  ");
                     if ((State.Exit_Qualification / 2 ** 3) and 1) = 1 then
                        Subject.Text_IO.Put_String ("read.");
                        State.Regs.RAX := State.Regs.RAX or 16#ffff#;
                     else
                        Subject.Text_IO.Put_String ("write: ");
                        Subject.Text_IO.Put_Word16
                          (Item => SK.Word16 (State.Regs.RAX and 16#ffff#));
                     end if;
                     Subject.Text_IO.New_Line;
                  when others =>
                     Halt := True;
               end case;
            when 3 =>      --  4-byte
               case State.Exit_Qualification / 2 ** 16 is --  Port number
                  --  Only handle these ports by now:
                  when 16#0cf8# |   --  PCI Addr            (hardcoded)
                       16#0cfc# =>  --  PCI Data            (hardcoded)
                     Subject.Text_IO.Put_String (" ");
                     Subject.Text_IO.Put_Word16
                       (SK.Word16 ((State.Exit_Qualification / 2 ** 16) and
                          16#ffff#));
                     Subject.Text_IO.Put_String ("  ");
                     if ((State.Exit_Qualification / 2 ** 3) and 1) = 1 then
                        Subject.Text_IO.Put_String ("read.");
                        State.Regs.RAX := State.Regs.RAX or 16#ffff_ffff#;
                     else
                        Subject.Text_IO.Put_String ("write: ");
                        Subject.Text_IO.Put_Word32
                          (Item => SK.Word32
                             (State.Regs.RAX and 16#ffff_ffff#));
                     end if;
                     Subject.Text_IO.New_Line;
                  when others =>
                     Halt := True;
               end case;
            when others => --  not used
               Subject.Text_IO.Put_Line
                 ("I/O instruction with invalid access size");
               Halt := True;
         end case;
      end if;
   end Process;

end Exit_Handlers.IO_Instruction;
