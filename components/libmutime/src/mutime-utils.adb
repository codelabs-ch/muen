--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Mutime.Utils
is

   -------------------------------------------------------------------------

   procedure Multiply_Divide
     (Value      :     Interfaces.Unsigned_64;
      Multiplier :     Interfaces.Unsigned_64;
      Divisor    :     Interfaces.Unsigned_64;
      Quotient   : out Interfaces.Unsigned_64)
   with
      SPARK_Mode => Off
   is
      use Interfaces;
   begin
      System.Machine_Code.Asm
        (Template => "mulq %2; divq %3",
         Outputs  => (Unsigned_64'Asm_Output ("=a", Quotient)),
         Inputs   =>
           (Unsigned_64'Asm_Input ("a", Value),
            Unsigned_64'Asm_Input ("rm", Multiplier),
            Unsigned_64'Asm_Input ("rm", Divisor)),
         Clobber  => "rdx");
   end Multiply_Divide;

   -------------------------------------------------------------------------

   function To_BCD
     (Value : Two_Digits_Type)
      return Interfaces.Unsigned_8
   is
      use type Interfaces.Unsigned_8;

      subtype Shift_Type is Natural range 0 .. 8;

      Shift  : Shift_Type            := 0;
      Result : Interfaces.Unsigned_8 := 0;
      Tmp    : Interfaces.Unsigned_8 := Value;
   begin
      loop
         Result := Result + (Tmp mod 10) * 2 ** Shift;
         Tmp    := Tmp / 10;
         exit when Shift + 4 > Shift_Type'Last;
         Shift  := Shift + 4;
      end loop;

      return Result;
   end To_BCD;

end Mutime.Utils;
