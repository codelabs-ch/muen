--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Strings;

with Debug_Ops;

package body Mudm.Client
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   pragma $Release_Warnings (Off, "formal parameter * is not referenced",
                             Reason => "Only used for debug output");
   procedure Pciconf_Emulate_Read
     (SID    :     Musinfo.SID_Type;
      Offset :     Offset_Type;
      Result : out Interfaces.Unsigned_32)
   is
      pragma $Release_Warnings (On, "formal parameter * is not referenced",
                                Reason => "Only used for debug output");
   begin
      pragma Debug
        (Debug_Ops.Put_Line
           (Item => "Error: Pciconf emulation read request for SID "
            & SK.Strings.Img (SID) & " @ offset "
            & SK.Strings.Img (SK.Byte (Offset)) & " but "
            & "pciconf emulation is disabled"));
      Result := 0;
   end Pciconf_Emulate_Read;

   -------------------------------------------------------------------------

   pragma $Release_Warnings (Off, "formal parameter * is not referenced",
                             Reason => "Only used for debug output");
   procedure Pciconf_Emulate_Write
     (SID    : Musinfo.SID_Type;
      Offset : Offset_Type;
      Value  : Interfaces.Unsigned_32)
   is
      pragma $Release_Warnings (On, "formal parameter * is not referenced",
                                Reason => "Only used for debug output");
   begin
      pragma Debug
        (Debug_Ops.Put_Line
           (Item => "Error: Pciconf emulation write request for SID "
            & SK.Strings.Img (SID) & " @ offset "
            & SK.Strings.Img (SK.Byte (Offset)) & ", value "
            & SK.Strings.Img (Value)
            & " but pciconf emulation is disabled"));
   end Pciconf_Emulate_Write;

end Mudm.Client;
