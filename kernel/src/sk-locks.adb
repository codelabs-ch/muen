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
         Previous_State := Locked;
         System.Machine_Code.Asm
           (Template => "lock xchgl %0, %1",
            Outputs  => (Lock_State_Type'Asm_Output ("+r", Previous_State),
                         Lock_State_Type'Asm_Output ("+m", Lock.State)),
            Clobber  => "memory",
            Volatile => True);

         exit when Previous_State = Free;
         SK.CPU.Pause;
      end loop;
   end Acquire;

   -------------------------------------------------------------------------

   procedure Initialize (Lock : out Spin_Lock_Type)
   is
   begin
      Lock.State := Free;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Release (Lock : in out Spin_Lock_Type)
   with SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "movl $0, %0",
         Outputs  => (Lock_State_Type'Asm_Output ("=m", Lock.State)),
         Clobber  => "memory",
         Volatile => True);
   end Release;

end SK.Locks;
