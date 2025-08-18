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

--D @Interface
--D This package provides an interface for accessing and manipulating the PCI
--D configuration space of a single PCI device.
package Mupci.Config_Device
is

   --  The following relies on the Ada 2012 'aliased' semantic descibed in:
   --  http://www.ada-auth.org/standards/12rat/html/Rat12-4-2.html
   --
   --  The 'aliased' keyword requires the compiler to enforce pass-by-reference,
   --  also for non-tagged records.
   --
   --  Rationale: Access types cannot be used as SPARK enforces them
   --  to be declared at library level.

   --  Check that the given device PCI configuration space matches the expected
   --  Vendor/Device ID.
   procedure Check_Vendor_Device
     (Device    : aliased     Config_Space_Type;
      Vendor_ID :             Vendor_ID_Type;
      Device_ID :             Device_ID_Type;
      Success   :         out Boolean);

   --  Check that the PCI configuration space of given device
   --  matches the expected BAR addresses and sizes.
   procedure Check_BARs
     (Device  : aliased in out Config_Space_Type;
      BARs    :                BAR_Array;
      Success :            out Boolean);

   --  Setup BARs of specified device according to policy.
   procedure Setup_BARs
     (Device  : aliased in out Config_Space_Type;
      BARs    :                BAR_Array;
      Success :            out Boolean);

   --  Disable decode of I/O and mem spaces for specified device.
   procedure Decode_Disable (Device : aliased in out Config_Space_Type);

   --  Enable decode of I/O and mem spaces for specified device.
   procedure Decode_Enable (Device : aliased in out Config_Space_Type);

   --  Reset given device.
   procedure Reset
     (Device  : aliased in out Config_Space_Type;
      Method  :                Reset_Method_Type;
      Success :            out Boolean)
   with
      Pre => Method /= Reset_Method_None;

   --  Wait for device ready within the given timeout in MS. Returns False if
   --  the device is not ready after the specified time period.
   --
   --  The Divider argument's purpose is to split the total timeout duration
   --  into smaller, evenly spaced time slices. These slices are used to perform
   --  repeated wait operations instead of waiting for the full timeout in one
   --  go.
   procedure Wait_For_Device
     (Device     : aliased     Config_Space_Type;
      Timeout_MS :             Positive;
      Divider    :             Positive := 5;
      Success    :         out Boolean)
   with
      Pre => Timeout_MS mod Divider = 0;

   --  Return offset of PCI standard capability in given device PCI
   --  configuration space. If device does not have such a capability, Success
   --  is False and offset is set to null offset.
   procedure Get_PCI_Capability
     (Device  : aliased     Config_Space_Type;
      ID      :             Capability_ID_Type;
      Offset  :         out Capability_Ptr_Type;
      Success :         out Boolean);

end Mupci.Config_Device;
