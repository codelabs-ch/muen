--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  All rights reserved.
--
--  Redistribution and use in source and binary forms, with or without
--  modification, are permitted provided that the following conditions are met:
--
--    * Redistributions of source code must retain the above copyright notice,
--      this list of conditions and the following disclaimer.
--
--    * Redistributions in binary form must reproduce the above copyright
--      notice, this list of conditions and the following disclaimer in the
--      documentation and/or other materials provided with the distribution.
--
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
--  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
--  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
--  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
--  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
--  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
--  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
--  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
--  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
--  POSSIBILITY OF SUCH DAMAGE.
--

with System.Machine_Code;

with Interfaces;

with Libmucontrol_Component.Memory;

package body Init.Utils
is

   --  Return the current RSP value.
   function Get_Stack_Pointer return Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   function Get_Stack_Pointer return Interfaces.Unsigned_64
   with SPARK_Mode => Off
   is
      Result : Interfaces.Unsigned_64;
   begin
      System.Machine_Code.Asm
        (Template => "movq %%rsp, %0",
         Outputs  => (Interfaces.Unsigned_64'Asm_Output ("=r", Result)),
         Volatile => True);
      return Result;
   end Get_Stack_Pointer;

   -------------------------------------------------------------------------

   function Is_Control (Region : Musinfo.Memregion_Type) return Boolean
   is
      use type Interfaces.Unsigned_64;
   begin
      return Region.Address = Libmucontrol_Component.Memory.Control_Address;
   end Is_Control;

   -------------------------------------------------------------------------

   function Is_Stack (Region : Musinfo.Memregion_Type) return Boolean
   is
      use type Interfaces.Unsigned_64;

      End_Address : constant Interfaces.Unsigned_64
        := Region.Address + Region.Size - 1;
      RSP : constant Interfaces.Unsigned_64 := Get_Stack_Pointer;
   begin
      return RSP >= Region.Address and then RSP <= End_Address;
   end Is_Stack;

   -------------------------------------------------------------------------

   function Is_Status (Region : Musinfo.Memregion_Type) return Boolean
   is
      use type Interfaces.Unsigned_64;
   begin
      return Region.Address = Libmucontrol_Component.Memory.Status_Address;
   end Is_Status;

end Init.Utils;
