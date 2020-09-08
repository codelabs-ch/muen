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

with Interfaces;

private with System;

private with Libmucontrol_Component.Memory;

package Mucontrol.Status.Instance
with
   Abstract_State => (State with External => Async_Readers),
   Initializes    => State
is

   --  Initialize subject status.
   procedure Initialize
   with
      Global  => (Output => State),
      Depends => (State => null);

   --  Get current subject state.
   function Get return State_Type
   with
      Global => (Input => State),
      Volatile_Function;

   --  Get current diagnostics value.
   function Get_Diagnostics return Diagnostics_Type
   with
      Global => (Input => State),
      Volatile_Function;

   --  Get current watchdog value.
   function Get_Watchdog return Interfaces.Unsigned_64
   with
      Global => (Input => State),
      Volatile_Function;

   --  Set subject state to given value.
   procedure Set (New_State : State_Type)
   with
      Global  => (In_Out => State),
      Depends => (State  =>+ New_State);

   --  Set error in subject status.
   procedure Error
   with
      Global  => (In_Out => State),
      Depends => (State  => State);

   --  Set diagnostic to given value.
   procedure Set_Diagnostics (Value : Diagnostics_Type)
   with
      Global  => (In_Out => State),
      Depends => (State  =>+ Value);

   --  Set watchdog value.
   procedure Set_Watchdog (Value : Interfaces.Unsigned_64)
   with
      Global  => (In_Out => State),
      Depends => (State  =>+ Value);

private

   package Cspec renames Libmucontrol_Component.Memory;

   Status_Page : Status_Interface_Type
   with
      Import,
      Async_Readers,
      Part_Of => State,
      Size    => Cspec.Status_Size * 8,
      Address => System'To_Address (Cspec.Status_Address);

end Mucontrol.Status.Instance;
