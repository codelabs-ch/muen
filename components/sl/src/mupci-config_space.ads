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

with SK;

private with System;

with Musinfo;

--D @Interface
--D This package defines an API to the PCI configuration space of devices
--D assigned to a subject.
package Mupci.Config_Space
with
   Abstract_State => (State with External => (Async_Readers, Async_Writers))
is

   --  Check that the PCI configuration space of given device
   --  matches the expected Vendor/Device ID.
   procedure Check_Vendor_Device
     (Device  :     Device_Type;
      Success : out Boolean);

   --  Check that the PCI configuration space of given device
   --  matches the expected BAR addresses and sizes.
   procedure Check_BARs
     (Device  :     Device_Type;
      Success : out Boolean);

   --  Setup BARs of specified device according to policy.
   procedure Setup_BARs
     (Device  :     Device_Type;
      Success : out Boolean);

   --  Disable decode of I/O and mem spaces for specified device.
   procedure Decode_Disable (SID : Musinfo.SID_Type);

   --  Enable decode of I/O and mem spaces for specified device.
   procedure Decode_Enable (SID : Musinfo.SID_Type);

   --  Reset given device.
   procedure Reset
     (Device  :     Device_Type;
      Success : out Boolean)
   with
      Pre => Device.Reset /= Reset_Method_None;

   --  Wait for device ready within the given timeout in MS. Returns False if
   --  the device is not ready after the specified time period.
   procedure Wait_For_Device
     (SID        :     Musinfo.SID_Type;
      Timeout_MS :     Positive;
      Success    : out Boolean);

   --  Return address of device PCI configuration space.
   function Mmconf_Address
     (SID : Musinfo.SID_Type)
      return Interfaces.Unsigned_64;

   --  Return address of device PCI configuration space register at
   --  given offset.
   function Mmconf_Register
     (SID    : Musinfo.SID_Type;
      Offset : Dev_Specific_Range)
      return Interfaces.Unsigned_64;

   --  Return offset of PCI standard capability in given device PCI
   --  configuration space. If device does not have such a capability, Success
   --  is False and offset is set to null offset.
   procedure Get_PCI_Capability
     (SID     :     Musinfo.SID_Type;
      ID      :     Capability_ID_Type;
      Offset  : out Capability_Ptr_Type;
      Success : out Boolean);

private

   use type Interfaces.Unsigned_64;

   type Addrspace_Type is array (Musinfo.SID_Type) of Config_Space_Type
   with
      Component_Size => SK.Page_Size * 8,
      Object_Size    => (Interfaces.Unsigned_64 (Musinfo.SID_Type'Last) + 1)
         * SK.Page_Size * 8,
      Pack;

   --  TODO: We need XML Unsigned_32 <integer> support to define this in config.
   --        or generate it in CSPECS since it is static.
   Mmconf_Base : constant := 16#f800_0000#;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Space : Addrspace_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Part_Of => State,
      Address => System'To_Address (Mmconf_Base);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   function Mmconf_Address
     (SID : Musinfo.SID_Type)
      return Interfaces.Unsigned_64
   is (Mmconf_Base + Interfaces.Unsigned_64 (SID * SK.Page_Size));

   function Mmconf_Register
     (SID    : Musinfo.SID_Type;
      Offset : Dev_Specific_Range)
      return Interfaces.Unsigned_64
   is (Mmconf_Address (SID) + Interfaces.Unsigned_64 (Offset));

end Mupci.Config_Space;
