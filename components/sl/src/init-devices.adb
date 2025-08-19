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

with Pciconf;

package body Init.Devices
is

   -------------------------------------------------------------------------

   procedure Reset
   is
      use type Mupci.Reset_Method_Type;

      Success : Boolean;
   begin
      for D of Pciconf.Devices loop
         declare
            PCIe_Cap : Mupci.Capability_Ptr_Type;
         begin
            --  TODO: check capabilities list bit before accessing caps.
            Mupci.Config_Space.Debug.Print_PCI_Device_Info (SID => D.SID);
            Mupci.Config_Space.Debug.Print_PCI_Capabilities (SID => D.SID);

            Mupci.Config_Space.Get_PCI_Capability
              (SID     => D.SID,
               ID      => Mupci.PCI_Express_Capability,
               Offset  => PCIe_Cap,
               Success => Success);
            if not Success then
               Debuglog.Client.Put_Line
                 (Item => "ERROR: PCIe capability not found for device SID "
                  & SK.Strings.Img (D.SID));
               return;
            end if;

            Mupci.Config_Space.Debug.Print_PCIe_Capability_Structure
              (Address => Mupci.Config_Space.Debug.Mmconf_Register
                 (SID    => D.SID,
                  Offset => PCIe_Cap));

            Mupci.Config_Space.Check_Device_Identity
              (Device  => D,
               Success => Success);
            if not Success then
               Debuglog.Client.Put_Line
                  (Item => "ERROR: Unexpected device at "
                   & SK.Strings.Img (Mupci.Config_Space.Debug.Mmconf_Address
                     (SID => D.SID)));
               return;
            end if;

            Mupci.Config_Space.Check_BARs
              (Device  => D,
               Success => Success);
            if not Success then
               Debuglog.Client.Put_Line
                 (Item => "WARNING: Unexpected BAR for device SID "
                  & SK.Strings.Img (D.SID) & ", continue");
            end if;

            if D.Reset /= Mupci.Reset_Method_None then
               Mupci.Config_Space.Reset
                 (Device  => D,
                  Success => Success);
               if not Success then
                  Debuglog.Client.Put_Line
                    (Item => "ERROR: Reset failed, SID "
                     & SK.Strings.Img (D.SID));
               end if;
               Mupci.Config_Space.Debug.Print_PCI_Device_Info (SID => D.SID);
               Mupci.Config_Space.Debug.Print_PCI_Capabilities (SID => D.SID);
               Mupci.Config_Space.Debug.Print_PCIe_Capability_Structure
                (Address => Mupci.Config_Space.Debug.Mmconf_Register
                   (SID    => D.SID,
                    Offset => PCIe_Cap));

               Mupci.Config_Space.Setup_BARs
                 (Device  => D,
                  Success => Success);
               if not Success then
                  Debuglog.Client.Put_Line
                    (Item => "ERROR: Programming BARs failed, SID "
                     & SK.Strings.Img (D.SID));
               end if;
               Mupci.Config_Space.Debug.Print_PCI_Device_Info (SID => D.SID);
            end if;
         end;
      end loop;
   end Reset;

end Init.Devices;
