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

package body SK.Hypercall
is

   -------------------------------------------------------------------------

   procedure Trigger_Event (Number : SK.Byte)
   with
      SPARK_Mode => Off
   is
      ID : constant SK.Word64 := Word64 (Number);
   begin
      System.Machine_Code.Asm
        (Template => "vmcall",
         Inputs   => (Word64'Asm_Input ("a", ID)),
         Volatile => True);
   end Trigger_Event;

end SK.Hypercall;
