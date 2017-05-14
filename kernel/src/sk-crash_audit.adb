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

with System.Machine_Code;

with SK.Dump;
with SK.CPU;

package body SK.Crash_Audit
with
   Refined_State => (State => Next_Slot)
is

   Next_Slot : Positive := Positive'First
   with
      Volatile,
      Async_Readers,
      Async_Writers;

   -------------------------------------------------------------------------

   --  Atomically retrieve next slot index and increment global Next_Slot
   --  variable.
   procedure Get_And_Inc (Slot : out Positive)
   with
      Global => (In_Out => Next_Slot);

   procedure Get_And_Inc (Slot : out Positive)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "movq $1, %%rax; lock xadd %%eax, %0",
         Outputs  => (Positive'Asm_Output ("=m", Next_Slot),
                      Positive'Asm_Output ("=a", Slot)),
         Volatile => True,
         Clobber  => "cc");
   end Get_And_Inc;

   -------------------------------------------------------------------------

   procedure Allocate (Audit : out Entry_Type)
   is
      S : Positive;
   begin
      Audit.Slot := Dumpdata_Index'First;

      Get_And_Inc (Slot => S);
      if S > Max_Dumps then
         pragma Debug
           (Dump.Print_Message_8
              (Msg  => "Crash audit: Unable to allocate record, halting CPU "
               & "- slot count is",
               Item => Byte (S)));
         CPU.Stop;
      end if;

      Audit.Slot := Dumpdata_Index (S);
      pragma Debug (Dump.Print_Message_8
                    (Msg  => "Crash audit: Allocated record",
                     Item => Byte (Audit.Slot)));
   end Allocate;

end SK.Crash_Audit;
