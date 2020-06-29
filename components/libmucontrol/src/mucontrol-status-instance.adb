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

with System;

with Libmucontrol_Component.Memory;

package body Mucontrol.Status.Instance
is

   package Cspec renames Libmucontrol_Component.Memory;

   Status_Page : Status_Page_Type
   with
      Async_Readers,
      Size    => Cspec.Status_Size * 8,
      Address => System'To_Address (Cspec.Status_Address);

   -------------------------------------------------------------------------

   procedure Error (Diagnostic : Diagnostics_Type)
   is
   begin
      Status_Page.Diagnostics := Diagnostic;
      Status_Page.Status := STATE_ERROR;
   end Error;

   -------------------------------------------------------------------------

   function Get return Status_Type
   is (Status_Page.Status);

   -------------------------------------------------------------------------

   function Get_Watchdog return Interfaces.Unsigned_64
   is (Status_Page.Watchdog);

   -------------------------------------------------------------------------

   procedure Initialize
   is
   begin
      Status_Page := (Status      => STATE_INITIAL,
                      Watchdog    => 0,
                      Diagnostics => DIAG_OK,
                      Reserved    => (others => 0));
   end Initialize;

   -------------------------------------------------------------------------

   procedure Set (New_Status : Status_Type)
   is
   begin
      Status_Page.Status := New_Status;
   end Set;

   -------------------------------------------------------------------------

   procedure Set_Watchdog (Value : Interfaces.Unsigned_64)
   is
   begin
      Status_Page.Watchdog := Value;
   end Set_Watchdog;

end Mucontrol.Status.Instance;
