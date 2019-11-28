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

with SK.Constants;

package body SK.Debug_Lock
with
   SPARK_Mode => Off
is

   type Spin_Lock_Type is record
      Locked : SK.Word32;
   end record;

   Global_Lock : Spin_Lock_Type := (Locked => 0)
   with
      Linker_Section => Constants.Global_Data_Section;

   -------------------------------------------------------------------------

   procedure Acquire
   is
      Result : SK.Word32;
   begin
      loop
         System.Machine_Code.Asm
           (Template => "mov $1, %%eax; lock xchgl %%eax, (%%rdx); pause",
            Outputs  => (SK.Word32'Asm_Output ("=a", Result)),
            Inputs   => (System.Address'Asm_Input
                         ("d", Global_Lock.Locked'Address)));

         if Result = 0 then
            exit;
         end if;
      end loop;
   end Acquire;

   -------------------------------------------------------------------------

   procedure Release
   is
   begin
      System.Machine_Code.Asm
        (Template => "movl $0, %0",
         Outputs  => (SK.Word32'Asm_Output ("=m", Global_Lock.Locked)),
         Volatile => True);
   end Release;

end SK.Debug_Lock;
