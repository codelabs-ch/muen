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

with System;

with SK.CPU;
with SK.Hypercall;
with SK.Constants;

with Skp;

with Subject.Text_IO;

with Interrupts;
with Handler;

procedure Sm
is
   use type SK.Word64;

   TSC_Counter   : SK.Word64 := 0;

   Id            : Skp.Subject_Id_Type;
   Dump_And_Halt : Boolean := False;
   State         : SK.Subject_State_Type;
   for State'Address use System'To_Address (16#1e0000#);
begin
   Subject.Text_IO.Init;
   Subject.Text_IO.Put_Line ("SM subject running");
   Interrupts.Initialize;

   SK.CPU.Sti;
   SK.CPU.Hlt;

   loop
      Id := Handler.Current_Subject;

      if State.Exit_Reason = SK.Constants.EXIT_REASON_CPUID then

         --  Minimal CPUID emulation. Code inspired by the emulation done by
         --  the lguest hypervisor in the Linux kernel
         --  (see arch/x86/lguest/boot.c, lguest_cpuid function).
         --  For reference values see e.g.
         --  http://www.cpu-world.com/cgi-bin/CPUID.pl?CPUID=26937&RAW_DATA=1

         case State.Regs.RAX is
            when 0 =>

               --  Get vendor ID.

               --  Return the vendor ID for a GenuineIntel processor and set
               --  the highest valid CPUID number to 1.

               State.Regs.RAX := 1;
               State.Regs.RBX := 16#756e_6547#;
               State.Regs.RCX := 16#6c65_746e#;
               State.Regs.RDX := 16#4965_6e69#;
            when 1 =>

               --  Processor Info and Feature Bits.
               --                    i486    SX
               --                        \  /
               State.Regs.RAX := 16#0000_0420#;
               State.Regs.RBX := 16#0000_0000#;
               State.Regs.RCX := 16#0000_0000#;
               --  We currently provide no features.
               State.Regs.RDX := 16#0000_0000#;
            when 16#8000_0000# =>

               --  Get Highest Extended Function Supported.
               --  Disabled for now.

               State.Regs.RAX := 16#8000_0000#;
            when others =>
               Subject.Text_IO.Put_String (Item => "Unknown CPUID function ");
               Subject.Text_IO.Put_Word64 (Item => State.Regs.RAX);
               Subject.Text_IO.New_Line;
               Dump_And_Halt := True;
         end case;

      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_INVLPG
        or else State.Exit_Reason = SK.Constants.EXIT_REASON_DR_ACCESS
      then

         --  Ignore INVLPG and MOV DR for now.
         null;

      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_RDTSC then
         State.Regs.RAX := TSC_Counter and 16#ffff_ffff#;
         State.Regs.RDX := TSC_Counter / 2 ** 32;
         TSC_Counter := TSC_Counter + 1;

      elsif State.Exit_Reason = SK.Constants.EXIT_REASON_IO_INSTRUCTION then
         case State.Exit_Qualification / 2 ** 16 is
            when 16#64# =>
               null;
            when others =>
               Subject.Text_IO.Put_String (Item => "I/O instruction on port ");
               Subject.Text_IO.Put_Word16
                 (Item => SK.Word16 (State.Exit_Qualification / 2 ** 16));
               Subject.Text_IO.New_Line;
         end case;

         if (State.Exit_Qualification and 48) /= 0 then
            --  String and REP not supported
            Dump_And_Halt := True;
         else
            case State.Exit_Qualification and 7 is --  Size of access
               when 0 =>      --  1-byte
                  case State.Exit_Qualification / 2 ** 16 is --  Port number
                     --  Only handle these ports by now:
                     when 16#21#  |    --  PIC_MASTER_IMR      (hardcoded)
                          16#a1#  |    --  PIC_SLAVE_IMR       (hardcoded)
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
                          16#3fa# =>   --  82C710 C&T mouse port chip   (conf.)
                        --  ignore writes, read 00/ff
                        case State.Exit_Qualification / 2 ** 16 is
                           when 16#21# =>
                              Subject.Text_IO.Put_String
                                ("i8259 PIC_MASTER_IMR ");
                           when 16#a1# =>
                              Subject.Text_IO.Put_String
                                ("i8259 PIC_SLAVE_IMR ");
                           when 16#40# =>
                              Subject.Text_IO.Put_String ("i8253/4 PIT_CH0  ");
                           when 16#43# =>
                              Subject.Text_IO.Put_String ("i8253/4 PIT_MODE ");
                           when 16#64# =>
                              null;
                           when 16#70# =>
                              Subject.Text_IO.Put_String ("RTC CMD  ");
                           when 16#71# =>
                              Subject.Text_IO.Put_String ("RTC DATA ");
                           when 16#80# =>
                              Subject.Text_IO.Put_String ("PORT80 ");
                           when others =>
                              Subject.Text_IO.Put_String ("16#");
                              Subject.Text_IO.Put_Word16 (SK.Word16
                                ((State.Exit_Qualification / 2 ** 16) and
                                 16#ffff#));
                              Subject.Text_IO.Put_String ("# ");
                        end case;
                        if ((State.Exit_Qualification / 2 ** 3) and 1) = 1 then
                           case State.Exit_Qualification / 2 ** 16 is
                              when 16#64# =>
                                 null;
                              when others =>
                                 Subject.Text_IO.Put_String ("read.");
                           end case;
                           case State.Exit_Qualification / 2 ** 16 is
                              when 16#70# | 16#71# =>
                                 State.Regs.RAX :=
                                    State.Regs.RAX and not 16#ff#;
                              when others =>
                                    State.Regs.RAX := State.Regs.RAX or 16#ff#;
                           end case;
                        else
                           case State.Exit_Qualification / 2 ** 16 is
                              when 16#64# =>
                                 null;
                              when others =>
                                 Subject.Text_IO.Put_String ("write: ");
                                 Subject.Text_IO.Put_Byte
                                   (Item => SK.Byte
                                      (State.Regs.RAX and 16#ff#));
                           end case;
                        end if;
                        Subject.Text_IO.New_Line;
                     when others =>
                        Dump_And_Halt := True;
                  end case;
               when 1 =>      --  2-byte
                  Dump_And_Halt := True;
               when 3 =>      --  4-byte
                  Dump_And_Halt := True;
               when others => --  not used
                  Subject.Text_IO.Put_String ("Invalid access size");
                  Dump_And_Halt := True;
            end case;
         end if;

      else
         Subject.Text_IO.Put_String (Item => "Unhandled trap for subject ");
         Subject.Text_IO.Put_Byte   (Item => SK.Byte (Id));
         Subject.Text_IO.Put_String (Item => " EXIT (");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.Exit_Reason));
         Subject.Text_IO.Put_String (Item => ":");
         Subject.Text_IO.Put_Word32
           (Item => SK.Word32 (State.Exit_Qualification));
         Subject.Text_IO.Put_String (Item => ":");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Interrupt_Info));
         Subject.Text_IO.Put_Line   (Item => ")");

         Dump_And_Halt := True;
      end if;

      if not Dump_And_Halt then
         State.RIP := State.RIP + State.Instruction_Len;
         SK.Hypercall.Trigger_Event (Number => SK.Byte (Id));
      else
         Subject.Text_IO.Put_String ("EIP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RIP));
         Subject.Text_IO.Put_String (" CS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.CS));
         Subject.Text_IO.Put_String (" EFLAGS: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RFLAGS));
         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_String ("ESP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.RSP));
         Subject.Text_IO.Put_String (" SS : ");
         Subject.Text_IO.Put_Word16 (Item => SK.Word16 (State.SS));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "EAX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RAX));
         Subject.Text_IO.Put_String (Item => " EBX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RBX));
         Subject.Text_IO.Put_String (Item => " ECX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RCX));
         Subject.Text_IO.Put_String (Item => " EDX: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RDX));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "ESI: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RSI));
         Subject.Text_IO.Put_String (Item => " EDI: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RDI));
         Subject.Text_IO.Put_String (Item => " EBP: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.Regs.RBP));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.Put_String (Item => "CR0: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR0));
         Subject.Text_IO.Put_String (Item => " CR2: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR2));
         Subject.Text_IO.Put_String (Item => " CR3: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR3));
         Subject.Text_IO.Put_String (Item => " CR4: ");
         Subject.Text_IO.Put_Word32 (Item => SK.Word32 (State.CR4));
         Subject.Text_IO.New_Line;

         Subject.Text_IO.New_Line;
         Subject.Text_IO.Put_Line (Item => "Halting execution");

         loop
            SK.CPU.Hlt;
         end loop;
      end if;
   end loop;
end Sm;
