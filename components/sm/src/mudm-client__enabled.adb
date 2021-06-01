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

with System;

with SK.Hypercall;

with Libmudm_Component.Channels;

package body Mudm.Client
with
   Refined_State => (State => (Request, Response))
is

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Request : Mudm.Emul_Message_Type := Null_Emul_Message
   with
      Volatile,
      Async_Readers,
      Address => System'To_Address
        (Libmudm_Component.Channels.Dm_Pciconf_Req_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "This global variable is effectively read-only.");
   Response : Mudm.Emul_Message_Type
   with
      Volatile,
      Async_Writers,
      Address => System'To_Address
        (Libmudm_Component.Channels.Dm_Pciconf_Res_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure Pciconf_Emulate_Read
     (SID    :     Musinfo.SID_Type;
      Offset :     Offset_Type;
      Result : out Interfaces.Unsigned_32)
   is
   begin
      Request := (SID    => SID,
                  Offset => Offset,
                  Op     => Mudm.Emul_Req_Read,
                  Value  => 0,
                  Result => 0);
      SK.Hypercall.Trigger_Event
        (Number => Libmudm_Component.Channels.Dm_Pciconf_Req_Event);

      Result := Response.Result;
   end Pciconf_Emulate_Read;

   -------------------------------------------------------------------------

   procedure Pciconf_Emulate_Write
     (SID    : Musinfo.SID_Type;
      Offset : Offset_Type;
      Value  : Interfaces.Unsigned_32)
   is
   begin
      Request := (SID    => SID,
                  Offset => Offset,
                  Op     => Mudm.Emul_Req_Write,
                  Value  => Value,
                  Result => 0);
      SK.Hypercall.Trigger_Event
        (Number => Libmudm_Component.Channels.Dm_Pciconf_Req_Event);
   end Pciconf_Emulate_Write;

end Mudm.Client;
