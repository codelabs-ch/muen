--
--  Copyright (C) 2025  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2025  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Mupci.Config_Space
is

   FLR_Initiate : constant := 16#8000#;

   -------------------------------------------------------------------------

   procedure Reset
     (Dev     :     Device_Type;
      Success : out Boolean)
   is
      use type Interfaces.Unsigned_16;
      pragma Unreferenced (Dev);

      --  TODO: From base address + offset in device info.
      Caps : PCIe_Cap_Struct_Type
      with
         Address => System'To_Address (16#f800_8080#);

      Ctrl_Val : constant Interfaces.Unsigned_16 := Caps.Device_Control;
   begin
      -- PCI Express Base Specification 6.2, 6.6.2 Function Level Reset (FLR).
      Caps.Device_Control := Ctrl_Val or FLR_Initiate;
      --  TODO: real delay
      for I in 0 .. 300000 loop
         null;
      end loop;

      Success := True;
   end Reset;

end Mupci.Config_Space;
