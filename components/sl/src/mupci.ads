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

with Interfaces;

with SK;

with Musinfo;

--D @Interface
--D This package defines types used in the Muen PCI library API.
package Mupci
is

   --  Device database types.

   type BAR_Type is record
      Register_Value : Interfaces.Unsigned_32;
      Size           : Interfaces.Unsigned_32;
   end record;

   Null_BAR : constant BAR_Type;

   subtype BAR_Range is Natural range 0 .. 5;

   type BAR_Array is array (BAR_Range) of BAR_Type;

   --  Known reset methods, not all might be implemented.
   type Reset_Method_Type is
     (Reset_Method_None,
      Reset_Method_FLR,
      Reset_Method_AF_FLR,
      Reset_Method_PM,
      Reset_Method_Bus);

   type Vendor_ID_Type is new Interfaces.Unsigned_16;
   type Device_ID_Type is new Interfaces.Unsigned_16;

   type Device_Type is record
      SID       : Musinfo.SID_Type;
      Vendor_ID : Vendor_ID_Type;
      Device_ID : Device_ID_Type;
      BARs      : BAR_Array;
      Reset     : Reset_Method_Type;
   end record;

   type Device_Array is array (Positive range <>) of Device_Type;

   --  Types to access PCI configuration space of a device.

   --  PCIe configuration space range.

   subtype Dev_Specific_Range is
     Interfaces.Unsigned_16 range 16#40# .. 16#fff#;

   Null_Dev_Specific_Offset : constant Dev_Specific_Range;

   --  PCI capability pointer.
   --  See PCI Local Bus Specification, "Capabilities List".
   --  The last valid pointer must hold enough space for the ID and next
   --  pointer fields, and at least one data byte.

   use type Dev_Specific_Range;

   subtype Capability_Ptr_Type is
     Dev_Specific_Range
       range Dev_Specific_Range'First .. Dev_Specific_Range'Last - 3;

   --  Currently supported capabilites.
   type Capability_ID_Type is
     (PCI_Power_Management_Capability, PCI_Express_Capability);

   --  PCI configuration space of a single device, see PCI Express Base
   --  Specification 6.2, 7.5.1 PCI-Compatible Configuration Registers.
   type Config_Space_Type is private;

private

   Null_BAR : constant BAR_Type := (others => 0);

   Null_Dev_Specific_Offset : constant Dev_Specific_Range
     := Dev_Specific_Range'Last;

   for Capability_ID_Type use
     (PCI_Power_Management_Capability => 16#01#,
      PCI_Express_Capability          => 16#10#);

   type Info_Record_Type is record
      Revision_ID : Interfaces.Unsigned_8;
      Class_Code  : Interfaces.Unsigned_24;
   end record
   with
      Pack,
      Size => 32;

   Header_Size : constant := 64;

   type Device_BAR_Array is array (BAR_Range) of Interfaces.Unsigned_32
   with
      Size => 6 * 4 * 8;

   type Header_Type is record
      Vendor_ID               : Interfaces.Unsigned_16;
      Device_ID               : Interfaces.Unsigned_16;
      Command                 : Interfaces.Unsigned_16;
      Status                  : Interfaces.Unsigned_16;
      Info                    : Info_Record_Type;
      Cache_Line_Size         : Interfaces.Unsigned_8;
      Master_Latency_Timer    : Interfaces.Unsigned_8;
      Header_Type             : Interfaces.Unsigned_8;
      Buitin_Self_Test        : Interfaces.Unsigned_8;
      Base_Address_Registers  : Device_BAR_Array;
      Cardbus_CIS_Pointer     : Interfaces.Unsigned_32;
      Subsystem_Vendor_ID     : Interfaces.Unsigned_16;
      Subsystem_ID            : Interfaces.Unsigned_16;
      Expansion_ROM_Base_Addr : Interfaces.Unsigned_32;
      Capabilities_Pointer    : Interfaces.Unsigned_8;
      Reserved_1              : Interfaces.Unsigned_24;
      Reserved_2              : Interfaces.Unsigned_32;
      Interrupt_Line          : Interfaces.Unsigned_8;
      Interrupt_Pin           : Interfaces.Unsigned_8;
      Min_Grant               : Interfaces.Unsigned_8;
      Max_Latency             : Interfaces.Unsigned_8;
   end record
   with
      Size => Header_Size * 8;

   for Header_Type use record
      Vendor_ID               at 16#00# range  0 ..  15;
      Device_ID               at 16#00# range 16 ..  31;
      Command                 at 16#04# range  0 ..  15;
      Status                  at 16#04# range 16 ..  31;
      Info                    at 16#08# range  0 ..  31;
      Cache_Line_Size         at 16#0c# range  0 ..   7;
      Master_Latency_Timer    at 16#0c# range  8 ..  15;
      Header_Type             at 16#0c# range 16 ..  23;
      Buitin_Self_Test        at 16#0c# range 24 ..  31;
      Base_Address_Registers  at 16#10# range  0 .. 191;
      Cardbus_CIS_Pointer     at 16#28# range  0 ..  31;
      Subsystem_Vendor_ID     at 16#2c# range  0 ..  15;
      Subsystem_ID            at 16#2c# range 16 ..  31;
      Expansion_ROM_Base_Addr at 16#30# range  0 ..  31;
      Capabilities_Pointer    at 16#34# range  0 ..   7;
      Reserved_1              at 16#34# range  8 ..  31;
      Reserved_2              at 16#38# range  0 ..  31;
      Interrupt_Line          at 16#3c# range  0 ..   7;
      Interrupt_Pin           at 16#3c# range  8 ..  15;
      Min_Grant               at 16#3c# range 16 ..  23;
      Max_Latency             at 16#3c# range 24 ..  31;
   end record;

   type Dev_Specific_Array is array (Dev_Specific_Range) of Interfaces.Unsigned_8
   with
      Size => (Dev_Specific_Range'Last - Dev_Specific_Range'First + 1) * 8;

   --D @Interface
   --D PCI (Express) device configuration space.
   type Config_Space_Type is record
      --D @Interface
      --D Standard PCI configuration space header, see PCI Local Bus
      --D Specification, section "Configuration Space Organization".
      Header       : Header_Type;
      --D @Interface
      --D Device specific data above PCI 3.0 compatible configuration space
      --D header (> 0x3f) in PCI and PCI Express (PCIe) configuration
      --D space, including PCI and PCIe capability structures.
      Dev_Specific : Dev_Specific_Array;
   end record
   with
      Object_Size => SK.Page_Size * 8,
      Size        => SK.Page_Size * 8;

   --  PCI Express Base Specification 6.2, 7.5.3 PCI Express Capability
   --  Structure.
   type PCIe_Cap_Struct_Type is record
      Cap_ID         : Interfaces.Unsigned_8;
      Next_Cap_Ptr   : Interfaces.Unsigned_8;
      Caps_Register  : Interfaces.Unsigned_16;
      Device_Caps    : Interfaces.Unsigned_32;
      Device_Control : Interfaces.Unsigned_16;
      Device_Status  : Interfaces.Unsigned_16;
   end record
   with
      Size => 3 * 32;

   for PCIe_Cap_Struct_Type use record
      Cap_ID         at 16#00# range  0 ..  7;
      Next_Cap_Ptr   at 16#00# range  8 .. 15;
      Caps_Register  at 16#00# range 16 .. 31;
      Device_Caps    at 16#04# range  0 .. 31;
      Device_Control at 16#08# range  0 .. 15;
      Device_Status  at 16#08# range 16 .. 31;
   end record;

end Mupci;
