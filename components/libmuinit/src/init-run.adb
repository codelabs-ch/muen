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

with Musinfo.Instance;

with Mucontrol.Status;

with Init.Commands;
with Init.Memory;
with Init.Status;

package body Init.Run
is

   -------------------------------------------------------------------------

   --D @Section Id => subject-lifecycle-operation-init, Label => Initialization, Parent => subject-lifecycle-operation, Priority => 10
   --D @Text Section => subject-lifecycle-operation-init, Priority => 0
   --D Subject initialization consists of the following steps:
   --D @OL Id => subject-lifecycle-operation-init-steps, Section => subject-lifecycle-operation-init, Priority => 0
   procedure Initialize (Success : out Boolean)
   is
      Do_Erase : Boolean;
   begin
      --D @Item List => subject-lifecycle-operation-init-steps, Priority => 0
      --D Initialize the status interface by clearing all data and setting the
      --D state to \verb!STATE_INITIAL!.
      Status.Initialize;

      --D @Item List => subject-lifecycle-operation-init-steps, Priority => 0
      --D Check that Subject Info is valid. If it is not valid, set error on the
      --D status interface and return with \emph{Success} flag set to
      --D \emph{False}.
      if not Musinfo.Instance.Is_Valid then
         Success := False;
         Status.Error;
         return;
      end if;

      --D @Item List => subject-lifecycle-operation-init-steps, Priority => 10
      --D Wait for synchronization command.
      Commands.Wait_For_Sync (Success => Success);
      if Success then
         --D @Item List => subject-lifecycle-operation-init-steps, Priority => 15
         --D On Success, set status to \verb!STATE_SYNCED! and wait for either the
         --D \emph{Erase} or \emph{Prepare} command.
         Status.Set (New_State => Mucontrol.Status.STATE_SYNCED);
         Commands.Wait_For_Erase_Or_Prepare (Success => Success,
                                             Erase   => Do_Erase);
         if Success then
            if Do_Erase then
               --D @Item List => subject-lifecycle-operation-init-steps, Priority => 25
               --D On Success and erase command, set status to
               --D \verb!STATE_ERASING!.
               Status.Set (New_State => Mucontrol.Status.STATE_ERASING);

               --D @Item List => subject-lifecycle-operation-init-steps, Priority => 25
               --D Erase all writable memory regions by clearing them with
               --D zeros. Then, set the status to \verb!STATE_ERASED! and wait for
               --D the \emph{Prepare} command.
               Memory.Clear_Writable;
               Status.Set (New_State => Mucontrol.Status.STATE_ERASED);
               Commands.Wait_For_Prepare (Success => Success);
               if not Success then
                  --D @Item List => subject-lifecycle-operation-init-steps, Priority => 25
                  --D On Failure, signal error by setting status to
                  --D \verb!DIAG_UNEXPECTED_CMD! and return.
                  Status.Error
                    (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
                  return;
               end if;
            end if;

            --D @Item List => subject-lifecycle-operation-init-steps, Priority => 30
            --D On Success, set status to \verb!STATE_PREPARING!.
            Status.Set (New_State => Mucontrol.Status.STATE_PREPARING);

            --D @Item List => subject-lifecycle-operation-init-steps, Priority => 30
            --D Set up writable memory regions that have initial content by
            --D either copying the content from a read-only source region or
            --D filling them with a pattern.
            Memory.Setup_Writable (Success => Success);
            if not Success then
               --D @Item List => subject-lifecycle-operation-init-steps, Priority => 30
               --D On Failure, signal error and return. The diagnostics field
               --D contains the Sinfo index of the memory region that was being
               --D processed.
               Status.Error;
               return;
            end if;

            --D @Item List => subject-lifecycle-operation-init-steps, Priority => 30
            --D On Success, set status to \verb!STATE_PREPARED! and wait for the
            --D Validate command.
            Status.Set (New_State => Mucontrol.Status.STATE_PREPARED);
            Commands.Wait_For_Validate (Success => Success);
            if Success then

               --D @Item List => subject-lifecycle-operation-init-steps, Priority => 40
               --D On Success, set status to \verb!STATE_VALIDATING!.
               Status.Set (New_State => Mucontrol.Status.STATE_VALIDATING);

               --D @Item List => subject-lifecycle-operation-init-steps, Priority => 40
               --D Verify hashes of \emph{all}\footnote{This also includes
               --D read-only memory regions.} memory regions. The entire
               --D content of each memory region is hashed and the resulting
               --D value is compared to the reference hash contained in the
               --D corresponding Sinfo entry.
               Memory.Check_Hashes (Success => Success);
               if not Success then
                  --D @Item List => subject-lifecycle-operation-init-steps, Priority => 40
                  --D On Failure, signal error and return. The diagnostics field
                  --D contains the Sinfo index of the memory region that was
                  --D being processed.
                  Status.Error;
                  return;
               end if;
               --D @Item List => subject-lifecycle-operation-init-steps, Priority => 40
               --D Otherwise, set status to \verb!STATE_VALIDATED! and wait for
               --D the \emph{Run} command.
               Status.Set (New_State => Mucontrol.Status.STATE_VALIDATED);
               Commands.Wait_For_Run (Success => Success);
               if not Success then
                  --D @Item List => subject-lifecycle-operation-init-steps, Priority => 40
                  --D On Failure, signal error by setting status to
                  --D \verb!DIAG_UNEXPECTED_CMD!.
                  Status.Error
                    (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
               else
                  --D @Item List => subject-lifecycle-operation-init-steps, Priority => 40
                  --D On Success, set status to \verb!STATE_INITIALIZING!.
                  --D The final transition to \verb!STATE_RUNNING! state is done
                  --D just prior to jumping to the code of the component that
                  --D has just been initialized/reset.
                  Status.Set
                    (New_State => Mucontrol.Status.STATE_INITIALIZING);
               end if;
            else
               --D @Item List => subject-lifecycle-operation-init-steps, Priority => 31
               --D On Failure, signal error by setting status to
               --D \verb!DIAG_UNEXPECTED_CMD! and return.
               Status.Error (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
            end if;
         else
            --D @Item List => subject-lifecycle-operation-init-steps, Priority => 21
            --D On Failure, signal error by setting status to
            --D \verb!DIAG_UNEXPECTED_CMD!.
            Status.Error (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
         end if;
      else
         --D @Item List => subject-lifecycle-operation-init-steps, Priority => 11
         --D On Failure, signal error by setting status to
         --D \verb!DIAG_UNEXPECTED_CMD!.
         Status.Error (Diagnostic => Mucontrol.Status.DIAG_UNEXPECTED_CMD);
      end if;
   end Initialize;

end Init.Run;
