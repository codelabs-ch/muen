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

with System;

with Interfaces;

--  TODO: Remove me.
with Debuglog.Client;
with SK.Strings;

--  TODO: Remove
pragma Warnings (Off);
package body Init.Devices
is
   --  TODO: Move to proper library and use it in ahci_drv
   --  as well

   type Info_Record_Type is record
      Revision_ID : Interfaces.Unsigned_8;
      Class_Code  : Interfaces.Unsigned_24;
   end record
   with
      Pack,
      Size => 32;

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
      Base_Address_Register_0 : Interfaces.Unsigned_32;
      Base_Address_Register_1 : Interfaces.Unsigned_32;
      Base_Address_Register_2 : Interfaces.Unsigned_32;
      Base_Address_Register_3 : Interfaces.Unsigned_32;
      Base_Address_Register_4 : Interfaces.Unsigned_32;
      Base_Address_Register_5 : Interfaces.Unsigned_32;
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
      Size => 64 * 8;

   for Header_Type use record
      Vendor_ID               at 16#00# range  0 .. 15;
      Device_ID               at 16#00# range 16 .. 31;
      Command                 at 16#04# range  0 .. 15;
      Status                  at 16#04# range 16 .. 31;
      Info                    at 16#08# range  0 .. 31;
      Cache_Line_Size         at 16#0c# range  0 ..  7;
      Master_Latency_Timer    at 16#0c# range  8 .. 15;
      Header_Type             at 16#0c# range 16 .. 23;
      Buitin_Self_Test        at 16#0c# range 24 .. 31;
      Base_Address_Register_0 at 16#10# range  0 .. 31;
      Base_Address_Register_1 at 16#14# range  0 .. 31;
      Base_Address_Register_2 at 16#18# range  0 .. 31;
      Base_Address_Register_3 at 16#1c# range  0 .. 31;
      Base_Address_Register_4 at 16#20# range  0 .. 31;
      Base_Address_Register_5 at 16#24# range  0 .. 31;
      Cardbus_CIS_Pointer     at 16#28# range  0 .. 31;
      Subsystem_Vendor_ID     at 16#2c# range  0 .. 15;
      Subsystem_ID            at 16#2c# range 16 .. 31;
      Expansion_ROM_Base_Addr at 16#30# range  0 .. 31;
      Capabilities_Pointer    at 16#34# range  0 ..  7;
      Reserved_1              at 16#34# range  8 .. 31;
      Reserved_2              at 16#38# range  0 .. 31;
      Interrupt_Line          at 16#3c# range  0 ..  7;
      Interrupt_Pin           at 16#3c# range  8 .. 15;
      Min_Grant               at 16#3c# range 16 .. 23;
      Max_Latency             at 16#3c# range 24 .. 31;
   end record;

   subtype Capability_Range is Interfaces.Unsigned_8 range 16#40# .. 16#ff#;

   type Caps_Array is array (Capability_Range) of Interfaces.Unsigned_8
   with
      Pack;

   type Config_Space_Type is record
      Header       : Header_Type;
      Capabilities : Caps_Array;
   end record
   with
      Pack,
      Object_Size => 256 * 8,
      Size        => 256 * 8;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Instance : Config_Space_Type
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (16#f800_8000#);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

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

   --  FLR_Cap_Bit  : constant := 16#10000000#;
   FLR_Initiate : constant := 16#8000#;

   -------------------------------------------------------------------------

   procedure Do_Reset
   is
      use Debuglog.Client;
      use type Interfaces.Unsigned_16;

      Caps : PCIe_Cap_Struct_Type
      with
         Address => System'To_Address (16#f800_8080#);

      Ctrl_Val : Interfaces.Unsigned_16 := Caps.Device_Control;
   begin
      Put_Line (Item => "Reset()");

      -- PCI Express Base Specification 6.2, 6.6.2 Function Level Reset (FLR).
      Caps.Device_Control := Ctrl_Val or FLR_Initiate;
      --  TODO: real delay
      for I in 0 .. 300000 loop
         null;
      end loop;

      Ctrl_Val := Caps.Device_Control;
      Put_Line (Item => " Device_Control after reset : " & SK.Strings.Img (Ctrl_Val));
   end Do_Reset;

   -------------------------------------------------------------------------

   procedure Print_PCI_Capabilities
   is
      use Debuglog.Client;
      use type Interfaces.Unsigned_8;

      Cap_ID : Interfaces.Unsigned_8;
      Index  : Interfaces.Unsigned_8 := Instance.Header.Capabilities_Pointer;
   begin
      loop
         exit when Index = 0 or not (Index in Capability_Range);
         Cap_ID := Instance.Capabilities (Index);
         Put_Line (Item => " Capability : " & SK.Strings.Img (Cap_ID) & " @ "
                   & SK.Strings.Img (Index));
         Index := Instance.Capabilities (Index + 1);
      end loop;
   end Print_PCI_Capabilities;

   -------------------------------------------------------------------------

   procedure Print_PCI_Device_Info
   is
      use Debuglog.Client;

      Dummy8  : Interfaces.Unsigned_8;
      Dummy16 : Interfaces.Unsigned_16;
      Dummy32 : Interfaces.Unsigned_32;
   begin
      Put_Line (Item => "== PCI config space");
      Dummy16 := Instance.Header.Vendor_ID;
      Put_Line (Item => " Vendor ID : " & SK.Strings.Img (Dummy16));
      Dummy16 := Instance.Header.Device_ID;
      Put_Line (Item => " Device ID : " & SK.Strings.Img (Dummy16));
      Dummy8 := Instance.Header.Info.Revision_ID;
      Put_Line (Item => " Revision  : " & SK.Strings.Img (Dummy8));
      Dummy32 := Interfaces.Unsigned_32 (Instance.Header.Info.Class_Code);
      Put_Line (Item => " Class     : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_5;
      Put_Line (Item => " ABAR      : " & SK.Strings.Img (Dummy32));
      Dummy16 := Instance.Header.Command;
      Put_Line (Item => " CMD       : " & SK.Strings.Img (Dummy16));
      Dummy32 := Instance.Header.Base_Address_Register_0;
      Put_Line (Item => " BAR0      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_1;
      Put_Line (Item => " BAR1      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_2;
      Put_Line (Item => " BAR2      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_3;
      Put_Line (Item => " BAR3      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_4;
      Put_Line (Item => " BAR4      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Instance.Header.Base_Address_Register_5;
      Put_Line (Item => " BAR6      : " & SK.Strings.Img (Dummy32));
   end Print_PCI_Device_Info;

   -------------------------------------------------------------------------

   procedure Print_PCIe_Capability_Structure
   is
      use Debuglog.Client;

      Caps : PCIe_Cap_Struct_Type
      with
         Address => System'To_Address (16#f800_8080#);

      Dummy8  : Interfaces.Unsigned_8;
      Dummy16 : Interfaces.Unsigned_16;
      Dummy32 : Interfaces.Unsigned_32;
   begin
      Put_Line (Item => "== PCIe Capability Structure");
      Dummy8 := Caps.Cap_ID;
      Put_Line (Item => " Cap ID              : " & SK.Strings.Img (Dummy8));
      Dummy8 := Caps.Next_Cap_Ptr;
      Put_Line (Item => " Next Cap Ptr        : " & SK.Strings.Img (Dummy8));
      Dummy16 := Caps.Caps_Register;
      Put_Line (Item => " Caps Register       : " & SK.Strings.Img (Dummy16));
      Dummy32 := Caps.Device_Caps;
      Put_Line (Item => " Device Capabilities : " & SK.Strings.Img (Dummy32));
      Dummy16 := Caps.Device_Control;
      Put_Line (Item => " Device Control      : " & SK.Strings.Img (Dummy16));
      Dummy16 := Caps.Device_Status;
      Put_Line (Item => " Device Status       : " & SK.Strings.Img (Dummy16));
   end Print_PCIe_Capability_Structure;

   -------------------------------------------------------------------------

   procedure Reset
   is
   begin
      --  TODO: check capabilities list bit before accessing caps.
      Print_PCI_Device_Info;
      Print_PCI_Capabilities;
      Print_PCIe_Capability_Structure;

      Do_Reset;

      Print_PCI_Device_Info;
      Print_PCI_Capabilities;
      Print_PCIe_Capability_Structure;
   end Reset;

end Init.Devices;
pragma Warnings (On);
