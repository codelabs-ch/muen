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

with Mupci.Config_Device;

package body Mupci.Config_Space
with
   Refined_State => (State => Space)
is

   -------------------------------------------------------------------------

   procedure Check_BARs
     (Device  :     Device_Type;
      Success : out Boolean)
   is
   begin
      Config_Device.Check_BARs
        (Device  => Space (Device.SID),
         BARs    => Device.BARs,
         Success => Success);
   end Check_BARs;

   -------------------------------------------------------------------------

   procedure Check_Vendor_Device
     (Device  :     Device_Type;
      Success : out Boolean)
   is
   begin
      Config_Device.Check_Vendor_Device
        (Device    => Space (Device.SID),
         Vendor_ID => Device.Vendor_ID,
         Device_ID => Device.Device_ID,
         Success   => Success);
   end Check_Vendor_Device;

   -------------------------------------------------------------------------

   procedure Decode_Enable (SID : Musinfo.SID_Type)
   is
   begin
      Config_Device.Decode_Enable (Device => Space (SID));
   end Decode_Enable;

   -------------------------------------------------------------------------

   procedure Decode_Disable (SID : Musinfo.SID_Type)
   is
   begin
      Config_Device.Decode_Disable (Device => Space (SID));
   end Decode_Disable;

   -------------------------------------------------------------------------

   procedure Get_PCI_Capability
     (SID     :     Musinfo.SID_Type;
      ID      :     Capability_ID_Type;
      Offset  : out Capability_Ptr_Type;
      Success : out Boolean)
   is
   begin
      Config_Device.Get_PCI_Capability
        (Device  => Space (SID),
         ID      => ID,
         Offset  => Offset,
         Success => Success);
   end Get_PCI_Capability;

   -------------------------------------------------------------------------

   procedure Reset
     (Device  :     Device_Type;
      Success : out Boolean)
   is
   begin
      Config_Device.Reset
        (Device  => Space (Device.SID),
         Method  => Device.Reset,
         Success => Success);
   end Reset;

   -------------------------------------------------------------------------

   procedure Setup_BARs
     (Device  :     Device_Type;
      Success : out Boolean)
   is
   begin
      Config_Device.Check_BARs
        (Device  => Space (Device.SID),
         BARs    => Device.BARs,
         Success => Success);
   end Setup_BARs;

   -------------------------------------------------------------------------

   procedure Wait_For_Device
     (SID        :     Musinfo.SID_Type;
      Timeout_MS :     Positive;
      Divider    :     Positive := 5;
      Success    : out Boolean)
   is
   begin
      Config_Device.Wait_For_Device
        (Device     => Space (SID),
         Timeout_MS => Timeout_MS,
         Divider    => Divider,
         Success    => Success);
   end Wait_For_Device;

end Mupci.Config_Space;
