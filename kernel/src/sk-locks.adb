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

with SK.CPU;

package body SK.Locks
is

   -------------------------------------------------------------------------

   procedure Acquire (Lock : in out Spin_Lock_Type)
   with SPARK_Mode => Off
   is
      Previous_State : Lock_State_Type;
   begin
      loop
         System.Machine_Code.Asm
           (Template => "mov $1, %%eax; lock xchgl %%eax, (%%rdx)",
            Outputs  => (Lock_State_Type'Asm_Output ("=a", Previous_State)),
            Inputs   => (System.Address'Asm_Input
                         ("d", Lock.State'Address)));

         if Previous_State = Free then
            exit;
         end if;
         SK.CPU.Pause;
      end loop;
   end Acquire;

   -------------------------------------------------------------------------

   procedure Release (Lock : in out Spin_Lock_Type)
   with SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "movl $0, %0",
         Outputs  => (Lock_State_Type'Asm_Output ("=m", Lock.State)),
         Volatile => True);
   end Release;

end SK.Locks;
