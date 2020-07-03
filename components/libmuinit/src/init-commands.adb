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

with Mucontrol.Command.Instance;

package body Init.Commands
is

   use type Mucontrol.Command.Command_Type;

   -------------------------------------------------------------------------

   procedure Wait_For_Erase_Or_Prepare
     (Success : out Boolean;
      Erase   : out Boolean)
   is
      Cmd : Mucontrol.Command.Command_Type := Mucontrol.Command.CMD_SYNC;
   begin
      Mucontrol.Command.Instance.Wait_For_Next (Cmd => Cmd);
      Erase := Cmd = Mucontrol.Command.CMD_ERASE;
      Success := Erase or Cmd = Mucontrol.Command.CMD_PREPARE;
   end Wait_For_Erase_Or_Prepare;

   -------------------------------------------------------------------------

   procedure Wait_For_Prepare (Success : out Boolean)
   is
      Cmd : Mucontrol.Command.Command_Type := Mucontrol.Command.CMD_ERASE;
   begin
      Mucontrol.Command.Instance.Wait_For_Next (Cmd => Cmd);
      Success := Cmd = Mucontrol.Command.CMD_PREPARE;
   end Wait_For_Prepare;

   -------------------------------------------------------------------------

   procedure Wait_For_Run (Success : out Boolean)
   is
      Cmd : Mucontrol.Command.Command_Type := Mucontrol.Command.CMD_VALIDATE;
   begin
      Mucontrol.Command.Instance.Wait_For_Next (Cmd => Cmd);
      Success := Cmd = Mucontrol.Command.CMD_RUN;
   end Wait_For_Run;

   -------------------------------------------------------------------------

   procedure Wait_For_Sync (Success : out Boolean)
   is
      Cmd : Mucontrol.Command.Command_Type := Mucontrol.Command.CMD_NOP;
   begin
      Mucontrol.Command.Instance.Wait_For_Next (Cmd => Cmd);
      Success := Cmd = Mucontrol.Command.CMD_SYNC;
   end Wait_For_Sync;

   -------------------------------------------------------------------------

   procedure Wait_For_Validate (Success : out Boolean)
   is
      Cmd : Mucontrol.Command.Command_Type := Mucontrol.Command.CMD_PREPARE;
   begin
      Mucontrol.Command.Instance.Wait_For_Next (Cmd => Cmd);
      Success := Cmd = Mucontrol.Command.CMD_VALIDATE;
   end Wait_For_Validate;

end Init.Commands;
