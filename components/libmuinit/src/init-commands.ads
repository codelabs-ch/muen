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

package Init.Commands
is

   --  Wait for erase, prepare or self-control command. Success is True if the
   --  received command was either erase, prepare or self-control. Erase is set
   --  to True if the command was erase.
   procedure Wait_For_Erase_Or_Prepare
     (Success : out Boolean;
      Erase   : out Boolean);

   --  Wait for prepare or self-control command. Success is True if the
   --  received command was either prepare or self-control.
   procedure Wait_For_Prepare (Success : out Boolean);

   --  Wait for run or self-control command. Success is True if the received
   --  command was either run or self-control.
   procedure Wait_For_Run (Success : out Boolean);

   --  Wait for synchronization or self-control command. Success is True if the
   --  received command was either sync or self-control.
   procedure Wait_For_Sync (Success : out Boolean);

   --  Wait for validation or self-control command. Success is True if the
   --  received command was either validate or self-control.
   procedure Wait_For_Validate (Success : out Boolean);

end Init.Commands;
