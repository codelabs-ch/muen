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

with SK.KC;
with SK.CPU;
with SK.Apic;
with SK.Locks;

package body SK.Dump
is

   -------------------------------------------------------------------------

   procedure Dump_Registers
     (GPR : CPU_Registers_Type;
      RIP : Word64; CS  : Word64; RFL : Word64; RSP : Word64; SS  : Word64;
      CR0 : Word64; CR2 : Word64; CR3 : Word64; CR4 : Word64)
   is
   begin
      KC.Put_String ("RIP: ");
      KC.Put_Word64 (Item => RIP);
      KC.Put_String (" CS : ");
      KC.Put_Word16 (Item => Word16 (CS));
      KC.New_Line;
      KC.Put_String ("RSP: ");
      KC.Put_Word64 (Item => RSP);
      KC.Put_String (" SS : ");
      KC.Put_Word16 (Item => Word16 (SS));
      KC.New_Line;

      KC.Put_String (Item => "RAX: ");
      KC.Put_Word64 (Item => GPR.RAX);
      KC.Put_String (Item => " RBX: ");
      KC.Put_Word64 (Item => GPR.RBX);
      KC.Put_String (Item => " RCX: ");
      KC.Put_Word64 (Item => GPR.RCX);
      KC.New_Line;

      KC.Put_String (Item => "RDX: ");
      KC.Put_Word64 (Item => GPR.RDX);
      KC.Put_String (Item => " RSI: ");
      KC.Put_Word64 (Item => GPR.RSI);
      KC.Put_String (Item => " RDI: ");
      KC.Put_Word64 (Item => GPR.RDI);
      KC.New_Line;

      KC.Put_String (Item => "RBP: ");
      KC.Put_Word64 (Item => GPR.RBP);
      KC.Put_String (Item => " R08: ");
      KC.Put_Word64 (Item => GPR.R08);
      KC.Put_String (Item => " R09: ");
      KC.Put_Word64 (Item => GPR.R09);
      KC.New_Line;

      KC.Put_String (Item => "R10: ");
      KC.Put_Word64 (Item => GPR.R10);
      KC.Put_String (Item => " R11: ");
      KC.Put_Word64 (Item => GPR.R11);
      KC.Put_String (Item => " R12: ");
      KC.Put_Word64 (Item => GPR.R12);
      KC.New_Line;

      KC.Put_String (Item => "R13: ");
      KC.Put_Word64 (Item => GPR.R13);
      KC.Put_String (Item => " R14: ");
      KC.Put_Word64 (Item => GPR.R14);
      KC.Put_String (Item => " R15: ");
      KC.Put_Word64 (Item => GPR.R15);
      KC.New_Line;

      KC.Put_String (Item => "CR0: ");
      KC.Put_Word64 (Item => CR0);
      KC.Put_String (Item => " CR2: ");
      KC.Put_Word64 (Item => CR2);
      KC.Put_String (Item => " CR3: ");
      KC.Put_Word64 (Item => CR3);
      KC.New_Line;

      KC.Put_String (Item => "CR4: ");
      KC.Put_Word64 (Item => CR4);
      KC.Put_String (" EFL: ");
      KC.Put_Word32 (Item => Word32 (RFL));
      KC.New_Line;
   end Dump_Registers;
   pragma Inline_Always (Dump_Registers);

   -------------------------------------------------------------------------

   procedure Print_State (Context : Isr_Context_Type)
   is
   begin
      Locks.Spin_Lock;
      KC.New_Line;
      KC.Put_String (Item => "[CPU ");
      KC.Put_Byte   (Item => Apic.Get_ID);
      KC.Put_Line   (Item => " KERNEL PANIC]");

      KC.Put_String (Item => "Vector: ");
      KC.Put_Byte   (Item => Byte (Context.Vector));
      KC.Put_String (Item => ", Error: ");
      KC.Put_Word64 (Item => Context.Error_Code);
      KC.New_Line;
      KC.New_Line;

      Dump_Registers (GPR => Context.GPR,
                      RIP => Context.RIP,
                      CS  => Context.CS,
                      RFL => Context.RFLAGS,
                      RSP => Context.RSP,
                      SS  => Context.SS,
                      CR0 => CPU.Get_CR0,
                      CR2 => CPU.Get_CR2,
                      CR3 => CPU.Get_CR3,
                      CR4 => CPU.Get_CR4);
      Locks.Unlock;

      CPU.Stop;
   end Print_State;

end SK.Dump;
