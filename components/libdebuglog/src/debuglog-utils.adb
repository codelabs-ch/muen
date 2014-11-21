--
--  Copyright (C) 2014  secunet Security Networks AG
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

package body Debuglog.Utils
is

   -------------------------------------------------------------------------

   function Num_To_Char (Value : Nibble_Type) return Character
   is
      Result : Character;
   begin
      case Value is
         when 16#0# => Result := '0';
         when 16#1# => Result := '1';
         when 16#2# => Result := '2';
         when 16#3# => Result := '3';
         when 16#4# => Result := '4';
         when 16#5# => Result := '5';
         when 16#6# => Result := '6';
         when 16#7# => Result := '7';
         when 16#8# => Result := '8';
         when 16#9# => Result := '9';
         when 16#a# => Result := 'a';
         when 16#b# => Result := 'b';
         when 16#c# => Result := 'c';
         when 16#d# => Result := 'd';
         when 16#e# => Result := 'e';
         when 16#f# => Result := 'f';
      end case;
      return Result;
   end Num_To_Char;

end Debuglog.Utils;
