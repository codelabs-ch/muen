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

with Init.Commands;
with Init.Memory;
with Init.Stack;
with Init.Status;

procedure Libmuinit
is
   Success, Do_Erase : Boolean;
begin
   Init.Status.Initialize;
   Init.Commands.Wait_For_Sync (Success => Success);
   if Success then
      Init.Status.Set (New_Status => Mucontrol.Status.STATE_SYNCED);
      Init.Commands.Wait_For_Erase_Or_Prepare (Success => Success,
                                               Erase   => Do_Erase);
      if Success then
         if Do_Erase then
            Init.Status.Set (New_Status => Mucontrol.Status.STATE_ERASING);

            --  Erase writable regions

            Init.Memory.Clear_Writable;
            Init.Status.Set (New_Status => Mucontrol.Status.STATE_ERASED);
            Init.Commands.Wait_For_Prepare (Success => Success);
            if not Success then
               Init.Status.Error (Diagnostic => 3);
               return;
            end if;
         end if;
         Init.Status.Set (New_Status => Mucontrol.Status.STATE_PREPARING);

         --  Set up memory region content.
         Init.Memory.Setup_Writable (Success => Success);
         if not Success then
            Init.Status.Error (Diagnostic => 5);
         end if;
         Init.Status.Set (New_Status => Mucontrol.Status.STATE_PREPARED);
         Init.Commands.Wait_For_Validate (Success => Success);
         if Success then
            Init.Status.Set
              (New_Status => Mucontrol.Status.STATE_VALIDATING);
            --  Verify hashes of all memory regions.
            Init.Status.Set (New_Status => Mucontrol.Status.STATE_VALIDATED);
            Init.Commands.Wait_For_Run (Success => Success);
            Init.Status.Set
              (New_Status => Mucontrol.Status.STATE_INITIALIZING);
         else
            Init.Status.Error (Diagnostic => 4);
         end if;
      else
         Init.Status.Error (Diagnostic => 2);
      end if;
   else
      Init.Status.Error (Diagnostic => 1);
   end if;

   Init.Stack.Clear (Stack_Start => Init.Memory.Get_Stack_Base);
end Libmuinit;
