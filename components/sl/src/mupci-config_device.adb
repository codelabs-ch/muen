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

package body Mupci.Config_Device
is

   PCI_Cmd_Spaces_Enable_Bits : constant := 16#0003#;

   -------------------------------------------------------------------------

   procedure Check_Vendor_Device
     (Device    : aliased     Config_Space_Type;
      Vendor_ID :             Vendor_ID_Type;
      Device_ID :             Device_ID_Type;
      Success   :         out Boolean)
   is
      V_ID : constant Vendor_ID_Type
        := Vendor_ID_Type (Device.Header.Vendor_ID);
      D_ID : constant Device_ID_Type
        := Device_ID_Type (Device.Header.Device_ID);
   begin
      Success := V_ID = Vendor_ID and D_ID = Device_ID;
   end Check_Vendor_Device;

   -------------------------------------------------------------------------

   procedure Decode_Disable (Device : aliased in out Config_Space_Type)
   is
      Cmd_Register : Interfaces.Unsigned_16;
   begin
      Cmd_Register := Device.Header.Command;
      Cmd_Register := Cmd_Register and not PCI_Cmd_Spaces_Enable_Bits;
      Device.Header.Command := Cmd_Register;
   end Decode_Disable;

   -------------------------------------------------------------------------

   procedure Decode_Enable (Device : aliased in out Config_Space_Type)
   is
      Cmd_Register : Interfaces.Unsigned_16;
   begin
      Cmd_Register := Device.Header.Command;
      Cmd_Register := Cmd_Register or PCI_Cmd_Spaces_Enable_Bits;
      Device.Header.Command := Cmd_Register;
   end Decode_Enable;

end Mupci.Config_Device;
