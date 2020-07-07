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

package body Mucontrol.Command.Instance
with
   Refined_State => (State => Command_Page)
is

   -------------------------------------------------------------------------

   function Get return Command_Type
   is
   begin
      return Command_Page.Command;
   end Get;

   -------------------------------------------------------------------------

   function Get_Epoch return Interfaces.Unsigned_64
   is
   begin
      return Command_Page.Epoch;
   end Get_Epoch;

   -------------------------------------------------------------------------

   function Get_Watchdog_Interval return Interfaces.Unsigned_64
   is
   begin
      return Command_Page.Watchdog_Interval;
   end Get_Watchdog_Interval;

   -------------------------------------------------------------------------

   function Is_Self_Governed return Boolean
   is
      Current_Cmd : constant Command_Type := Command_Page.Command;
   begin
      return Current_Cmd = CMD_SELF_CTRL;
   end Is_Self_Governed;

   -------------------------------------------------------------------------

   procedure Wait_For_Next (Cmd : in out Command_Type)
   is
      Cur_Cmd : constant Command_Type := Cmd;
   begin
      loop
         Cmd := Command_Page.Command;
         exit when Cmd /= Cur_Cmd;
      end loop;
   end Wait_For_Next;

end Mucontrol.Command.Instance;
