--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body SK.CPU.VMX
is

   -------------------------------------------------------------------------

   procedure INVVPID
     (VPID    :     Word16;
      Success : out Boolean)
   with
      SPARK_Mode => Off
   is
      type Unsigned_48 is mod 2 ** 48
      with
         Size => 48;

      type Descr_Type is record
         VPID     : Word16;
         Reserved : Unsigned_48;
         Address  : Word64;
      end record
      with
         Pack,
         Size => 128;

      Descr : constant Descr_Type := (VPID     => VPID,
                                      Reserved => 0,
                                      Address  => 0);
   begin
      System.Machine_Code.Asm
        (Template => "invvpid %1, %2; seta %0",
         Inputs   => (Descr_Type'Asm_Input ("m", Descr),
                      Word64'Asm_Input ("a", 1)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end INVVPID;

   -------------------------------------------------------------------------

   procedure VMCLEAR
     (Region  :     Word64;
      Success : out Boolean)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "vmclear %1; seta %0",
         Inputs   => (Word64'Asm_Input ("m", Region)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc,memory",
         Volatile => True);
   end VMCLEAR;

   -------------------------------------------------------------------------

   procedure VMPTRLD
     (Region  :     Word64;
      Success : out Boolean)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "vmptrld %1; seta %0",
         Inputs   => (Word64'Asm_Input ("m", Region)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc,memory",
         Volatile => True);
   end VMPTRLD;

   -------------------------------------------------------------------------

   procedure VMPTRST
     (Region  : out Word64;
      Success : out Boolean)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "vmptrst %0; seta %1",
         Outputs  => (Word64'Asm_Output ("=m", Region),
                      Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMPTRST;

   -------------------------------------------------------------------------

   procedure VMREAD
     (Field   :     Word64;
      Value   : out Word64;
      Success : out Boolean)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "vmread %2, %0; seta %1",
         Inputs   => (Word64'Asm_Input ("r", Field)),
         Outputs  => (Word64'Asm_Output ("=rm", Value),
                      Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMREAD;

   -------------------------------------------------------------------------

   procedure VMWRITE
     (Field   :     Word64;
      Value   :     Word64;
      Success : out Boolean)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "vmwrite %1, %2; seta %0",
         Inputs   => (Word64'Asm_Input ("rm", Value),
                      Word64'Asm_Input ("r", Field)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc",
         Volatile => True);
   end VMWRITE;

   -------------------------------------------------------------------------

   procedure VMXON
     (Region  :     Word64;
      Success : out Boolean)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "vmxon %1; seta %0",
         Inputs   => (Word64'Asm_Input ("m", Region)),
         Outputs  => (Boolean'Asm_Output ("=q", Success)),
         Clobber  => "cc,memory",
         Volatile => True);
   end VMXON;

end SK.CPU.VMX;
