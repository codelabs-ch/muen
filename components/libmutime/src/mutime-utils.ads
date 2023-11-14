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

with Interfaces;

package Mutime.Utils
with
   SPARK_Mode
is

   subtype Two_Digits_Type is Interfaces.Unsigned_8 range 0 .. 99;

   --  Convert given byte from binary to BCD format.
   function To_BCD
     (Value : Two_Digits_Type)
      return Interfaces.Unsigned_8;

   use type Interfaces.Unsigned_128;

   --  The procedure multiplies a given value with a multiplier and then
   --  divides it by the specified divisor.
   --
   --  The procedure supports 128 bit arithmetic by using the processor's MUL
   --  and DIV instructions. It basically calculates the following efficiently:
   --
   --    Quotient := Unsigned_64'Mod (Unsigned_128 (Value) * Unsigned_128
   --      (Multiplier) / Unsigned_128 (Divisor))
   --
   --  The processor will raise an exception if the quotient is >= 2 ** 64. A
   --  precondition ensures that this does not happen.
   procedure Multiply_Divide
     (Value      :     Interfaces.Unsigned_64;
      Multiplier :     Interfaces.Unsigned_64;
      Divisor    :     Interfaces.Unsigned_64;
      Quotient   : out Interfaces.Unsigned_64)
   with
      Pre =>
         (Divisor /= 0 and then
            (Interfaces.Unsigned_128 (Value) *
             Interfaces.Unsigned_128 (Multiplier)) /
               Interfaces.Unsigned_128 (Divisor) <=
                 Interfaces.Unsigned_128 (Interfaces.Unsigned_64'Last));

end Mutime.Utils;
