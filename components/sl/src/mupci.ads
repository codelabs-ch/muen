--
--  Copyright (C) 2025  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2025  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Musinfo;

--  Common types used by CSPECS and Mupci library.
package Mupci
is

   type BAR_Type is record
      Register_Value : Interfaces.Unsigned_32;
      Size           : Interfaces.Unsigned_32;
   end record;

   Null_BAR : constant BAR_Type;

   subtype BAR_Range is Natural range 0 .. 5;

   type BAR_Array is array (BAR_Range) of BAR_Type;

   --  Known reset methods, not all might be implemented.
   type Reset_Method_Type is
     (Reset_Method_None,
      Reset_Method_FLR,
      Reset_Method_AF_FLR,
      Reset_Method_PM,
      Reset_Method_Bus);

   type Device_Type is record
      SID       : Musinfo.SID_Type;
      Device_ID : Interfaces.Unsigned_16;
      Vendor_ID : Interfaces.Unsigned_16;
      BARs      : BAR_Array;
      Reset     : Reset_Method_Type;
   end record;

   type Device_Array is array (Positive range <>) of Device_Type;

private

   Null_BAR : constant BAR_Type := (others => 0);

end Mupci;
