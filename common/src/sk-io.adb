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

package body SK.IO
is

   -------------------------------------------------------------------------

   procedure Inb
     (Port  :     SK.Word16;
      Value : out SK.Byte)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "inb %1, %0",
         Inputs   => (Word16'Asm_Input ("d", Port)),
         Outputs  => (Byte'Asm_Output ("=a", Value)),
         Volatile => True);
   end Inb;

   -------------------------------------------------------------------------

   procedure Inw
     (Port  :     SK.Word16;
      Value : out SK.Word16)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "inw %1, %0",
         Inputs   => (Word16'Asm_Input ("d", Port)),
         Outputs  => (Word16'Asm_Output ("=a", Value)),
         Volatile => True);
   end Inw;

   -------------------------------------------------------------------------

   procedure Outb
     (Port  : SK.Word16;
      Value : SK.Byte)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "outb %0, %1",
         Inputs   => (Byte'Asm_Input ("a", Value),
                      Word16'Asm_Input ("d", Port)),
         Volatile => True);
   end Outb;

   -------------------------------------------------------------------------

   procedure Outw
     (Port  : SK.Word16;
      Value : SK.Word16)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "outw %0, %1",
         Inputs   => (Word16'Asm_Input ("a", Value),
                      Word16'Asm_Input ("d", Port)),
         Volatile => True);
   end Outw;

end SK.IO;
