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

with System.Machine_Code;

with Skp;

with SK.CPU;

package body SK.MP
with
   Refined_State => (Barrier => All_Barrier)
is

   type Sense_Barrier_Type is record
      Sense      : Boolean := False with Atomic;
      Wait_Count : SK.Byte := 0     with Atomic;
   end record
     with
       Volatile;

   All_Barrier : Sense_Barrier_Type
     with
       Async_Readers,
       Async_Writers;

   -------------------------------------------------------------------------

   --  Atomically get and increment the wait count. Returns the current number
   --  of CPUs blocked by the barrier.
   procedure Get_And_Increment
     (Sense_Barrier : in out Sense_Barrier_Type;
      Count         :    out SK.Byte)
   with
      Global  => null,
      Depends => ((Sense_Barrier, Count) => Sense_Barrier);

   procedure Get_And_Increment
     (Sense_Barrier : in out Sense_Barrier_Type;
      Count         :    out SK.Byte)
   with
      SPARK_Mode => Off
   is
   begin
      Count := 1;

      System.Machine_Code.Asm
        (Template => "lock xaddb %0, %1",
         Inputs   => (SK.Byte'Asm_Input ("a", Count),
                      SK.Byte'Asm_Input ("m", Sense_Barrier.Wait_Count)),
         Outputs  => (SK.Byte'Asm_Output ("=a", Count),
                      SK.Byte'Asm_Output ("=m", Sense_Barrier.Wait_Count)),
         Volatile => True);
   end Get_And_Increment;

   -------------------------------------------------------------------------

   procedure Wait_For_All
   with
      SPARK_Mode      => Off,
      Refined_Global  => (In_Out => All_Barrier),
      Refined_Depends => (All_Barrier =>+ null)
   is
      Count         : SK.Byte;
      Barrier_Sense : Boolean;
      CPU_Sense     : Boolean;
   begin
      Barrier_Sense := All_Barrier.Sense;
      CPU_Sense     := not Barrier_Sense;

      Get_And_Increment (All_Barrier,  --  Workaround for [NA10-010]
                         Count);       --  (no named arguments)

      if Count = SK.Byte (Skp.CPU_Range'Last) then
         All_Barrier.Wait_Count := 0;
         All_Barrier.Sense      := CPU_Sense;
      else
         loop
            pragma $Prove_Warnings (Off, "unused assignment",
                                    Reason => "Sense is switched by last CPU");
            Barrier_Sense := All_Barrier.Sense;
            pragma $Prove_Warnings (On, "unused assignment");

            pragma $Prove_Warnings (Off, "statement has no effect",
                                    Reason => "Passing time by busy-looping");
            exit when Barrier_Sense = CPU_Sense;
            CPU.Pause;
            pragma $Prove_Warnings (On, "statement has no effect");
         end loop;
      end if;
   end Wait_For_All;

end SK.MP;
