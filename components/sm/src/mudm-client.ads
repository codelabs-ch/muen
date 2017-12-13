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

with X86_64;

with Interfaces;

--  TODO: Move this to Mudm
--  * Requires SK.Hypercall in a library
--  * Event constants would be a good thing
package Mudm.Client
with
   Abstract_State => (State with External => (Async_Readers, Async_Writers))
is

   --  Emulate PCI config space read operation for device given by SID.
   procedure Pciconf_Emulate_Read
     (SID    :     Musinfo.SID_Type;
      Offset :     Offset_Type;
      Result : out Interfaces.Unsigned_32)
   with
      Global => (In_Out => (State, X86_64.State));

   --  Emulate PCI config space write operation for device given by SID.
   procedure Pciconf_Emulate_Write
     (SID    : Musinfo.SID_Type;
      Offset : Offset_Type;
      Value  : Interfaces.Unsigned_32)
   with
      Global => (In_Out => (State, X86_64.State));

end Mudm.Client;
