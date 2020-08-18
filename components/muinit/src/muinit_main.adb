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

with System.Machine_Code;

with Musinfo.Instance;

with Mucontrol.Status;

with Libmucontrol_Component.Memory;

with Init.Memory;
with Init.Run;
with Init.Stack;
with Init.Status;

package body Muinit_Main
is

   -------------------------------------------------------------------------

   procedure Halt
     with No_Return
   is
   begin
      loop
         System.Machine_Code.Asm (Template => "hlt",
                                  Volatile => True);
      end loop;
   end Halt;

   -------------------------------------------------------------------------

   procedure Run (Run_Info : out Run_Info_Type)
   is
      DIAG_BASE_ADDR : constant Mucontrol.Status.Diagnostics_Type := 16#f00#;
      Stack_Base     : Interfaces.Unsigned_64;
      Success        : Boolean;
   begin
      if not Musinfo.Instance.Is_Valid then
         Init.Status.Error;
         Halt;
      end if;

      Init.Run.Initialize (Success => Success);
      if not Success then
         Halt;
      end if;

      Run_Info.Status_Address := Libmucontrol_Component.Memory.Status_Address;
      Run_Info.Status_Value   := Interfaces.Unsigned_64
        (Mucontrol.Status.STATE_RUNNING);
      Init.Memory.Get_Base_Addresses (Text_Base  => Run_Info.Entry_Point,
                                      Stack_Base => Stack_Base,
                                      Success    => Success);
      if not Success then
         Init.Status.Error (Diagnostic => DIAG_BASE_ADDR);
         Halt;
      end if;

      Init.Stack.Clear (Stack_Start => Stack_Base);
   end Run;

end Muinit_Main;
