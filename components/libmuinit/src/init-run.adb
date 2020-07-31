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

with Mucontrol.Status;

with Libmucontrol_Component.Memory;

with Init.Commands;
with Init.Memory;
with Init.Stack;
with Init.Status;

package body Init.Run
is

   -------------------------------------------------------------------------

   procedure Initialize
   is
      Success, Do_Erase : Boolean;
   begin
      Status.Initialize;
      Commands.Wait_For_Sync (Success => Success);
      if Success then
         Status.Set (New_Status => Mucontrol.Status.STATE_SYNCED);
         Commands.Wait_For_Erase_Or_Prepare (Success => Success,
                                             Erase   => Do_Erase);
         if Success then
            if Do_Erase then
               Status.Set (New_Status => Mucontrol.Status.STATE_ERASING);

               --  Erase writable regions.

               Memory.Clear_Writable;
               Status.Set (New_Status => Mucontrol.Status.STATE_ERASED);
               Commands.Wait_For_Prepare (Success => Success);
               if not Success then
                  Status.Error
                    (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
               end if;
            end if;
            Status.Set (New_Status => Mucontrol.Status.STATE_PREPARING);

            --  Set up memory region content.

            Memory.Setup_Writable (Success => Success);
            if not Success then
               Status.Error;
            end if;
            Status.Set (New_Status => Mucontrol.Status.STATE_PREPARED);
            Commands.Wait_For_Validate (Success => Success);
            if Success then
               Status.Set (New_Status => Mucontrol.Status.STATE_VALIDATING);

               --  Verify hashes of all memory regions.

               Memory.Check_Hashes (Success => Success);
               if not Success then
                  Status.Error;
               end if;
               Status.Set (New_Status => Mucontrol.Status.STATE_VALIDATED);
               Commands.Wait_For_Run (Success => Success);
               if not Success then
                  Status.Error
                    (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
               else
                  Status.Set
                    (New_Status => Mucontrol.Status.STATE_INITIALIZING);
               end if;
            end if;
         else
            Status.Error (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
         end if;
      else
         Status.Error (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
      end if;
   end Initialize;

   -------------------------------------------------------------------------

   procedure Main (Run_Info : out Run_Info_Type)
   is
   begin
      Initialize;
      Run_Info.Entry_Point := Memory.Get_Text_Base;
      Run_Info.Status_Address := Libmucontrol_Component.Memory.Status_Address;
      Run_Info.Status_Value   := Interfaces.Unsigned_64
        (Mucontrol.Status.STATE_RUNNING);

      Stack.Clear (Stack_Start => Memory.Get_Stack_Base);
   end Main;

end Init.Run;
