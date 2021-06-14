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

private with System;

private with Libmucontrol_Component.Memory;

package Mucontrol.Command.Instance
with
   Abstract_State => (State with External => Async_Writers)
is

   --  Returns the current command.
   function Get return Command_Type
   with
      Global => (Input => State),
      Volatile_Function;

   --  Returns the current epoch.
   function Get_Epoch return Interfaces.Unsigned_64
   with
      Global => (Input => State),
      Volatile_Function;

   --  Returns the current watchdog interval.
   function Get_Watchdog_Interval return Interfaces.Unsigned_64
   with
      Global => (Input => State),
      Volatile_Function;

   --  Returns True if the subject is self-governed.
   function Is_Self_Governed return Boolean
   with
      Global => (Input => State),
      Volatile_Function;

   --  Wait for next command that is different from the given Cmd value. Cmd is
   --  set to the new command value.
   procedure Wait_For_Next (Cmd : in out Command_Type)
   with
      Global  => (Input => State),
      Depends => (Cmd   =>+ State);

private

   package Cspec renames Libmucontrol_Component.Memory;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "Instance is exclusively mapped to external interface.");
   Command_Page : Command_Interface_Type
   with
      Import,
      Async_Writers,
      Part_Of => State,
      Size    => Cspec.Control_Size * 8,
      Address => System'To_Address (Cspec.Control_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end Mucontrol.Command.Instance;
