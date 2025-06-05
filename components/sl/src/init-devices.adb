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

with SK.Strings;

with Mupci.Config_Space.Debug;

with Debuglog.Client;

with Init.Cspecs;

package body Init.Devices
is

   -------------------------------------------------------------------------

   procedure Reset
   is
      Success : Boolean;
   begin
      for D of Cspecs.Devices loop
         --  TODO: check capabilities list bit before accessing caps.
         Mupci.Config_Space.Debug.Print_PCI_Device_Info (SID => D.SID);
         Mupci.Config_Space.Debug.Print_PCI_Capabilities (SID => D.SID);
         Mupci.Config_Space.Debug.Print_PCIe_Capability_Structure (SID => D.SID);

         Success := Mupci.Config_Space.Check_Vendor_Device (Device => D);

         if Success then
            Mupci.Config_Space.Reset
               (Device  => D,
               Success => Success);
            if Success then
               Mupci.Config_Space.Debug.Print_PCI_Device_Info (SID => D.SID);
               Mupci.Config_Space.Debug.Print_PCI_Capabilities (SID => D.SID);
               Mupci.Config_Space.Debug.Print_PCIe_Capability_Structure (SID => D.SID);
            else
               Debuglog.Client.Put_Line (Item => "ERROR: Reset failed");
            end if;
         else
            Debuglog.Client.Put_Line
              (Item => "ERROR: Unexpected device at "
                       & SK.Strings.Img (Mupci.Config_Space.Mmconf_Address
                          (SID => D.SID)));
         end if;
      end loop;
   end Reset;

end Init.Devices;
