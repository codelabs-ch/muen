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

with SK.Strings;

with Debuglog.Client;

package body Mupci.Config_Space.Debug
is

   use Debuglog.Client;

   -------------------------------------------------------------------------

   procedure Print_PCI_Capabilities (SID : Musinfo.SID_Type)
   is
      use type Interfaces.Unsigned_8;

      Cap_ID : Interfaces.Unsigned_8;
      Ptr    : constant Interfaces.Unsigned_8
        := Space (SID).Header.Capabilities_Pointer;

      --  PCI Express Base Specification 6.2, 7.5.1.1.11 Capabilities Pointer.
      --  Lowest two bits must be masked.
      Index : Interfaces.Unsigned_16 := Interfaces.Unsigned_16 (Ptr and 16#fc#);
   begin
      loop
         exit when Index = 0 or not (Index in Legacy_Range);
         Cap_ID := Space (SID).Dev_Specific (Index);
         Put_Line (Item => " Capability : " & SK.Strings.Img (Cap_ID) & " @ "
                   & SK.Strings.Img (Index));
         Index := Interfaces.Unsigned_16 (Space (SID).Dev_Specific (Index + 1));
      end loop;
   end Print_PCI_Capabilities;

   -------------------------------------------------------------------------

   procedure Print_PCI_Device_Info (SID : Musinfo.SID_Type)
   is
      Dummy8  : Interfaces.Unsigned_8;
      Dummy16 : Interfaces.Unsigned_16;
      Dummy32 : Interfaces.Unsigned_32;
   begin
      Put_Line (Item => "== PCI config space");
      Dummy16 := Space (SID).Header.Vendor_ID;
      Put_Line (Item => " Vendor ID : " & SK.Strings.Img (Dummy16));
      Dummy16 := Space (SID).Header.Device_ID;
      Put_Line (Item => " Device ID : " & SK.Strings.Img (Dummy16));
      Dummy8 := Space (SID).Header.Info.Revision_ID;
      Put_Line (Item => " Revision  : " & SK.Strings.Img (Dummy8));
      Dummy32 := Interfaces.Unsigned_32 (Space (SID).Header.Info.Class_Code);
      Put_Line (Item => " Class     : " & SK.Strings.Img (Dummy32));
      Dummy32 := Space (SID).Header.Base_Address_Register_5;
      Put_Line (Item => " ABAR      : " & SK.Strings.Img (Dummy32));
      Dummy16 := Space (SID).Header.Command;
      Put_Line (Item => " CMD       : " & SK.Strings.Img (Dummy16));
      Dummy32 := Space (SID).Header.Base_Address_Register_0;
      Put_Line (Item => " BAR0      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Space (SID).Header.Base_Address_Register_1;
      Put_Line (Item => " BAR1      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Space (SID).Header.Base_Address_Register_2;
      Put_Line (Item => " BAR2      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Space (SID).Header.Base_Address_Register_3;
      Put_Line (Item => " BAR3      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Space (SID).Header.Base_Address_Register_4;
      Put_Line (Item => " BAR4      : " & SK.Strings.Img (Dummy32));
      Dummy32 := Space (SID).Header.Base_Address_Register_5;
      Put_Line (Item => " BAR6      : " & SK.Strings.Img (Dummy32));
   end Print_PCI_Device_Info;

   -------------------------------------------------------------------------

   procedure Print_PCIe_Capability_Structure (SID : Musinfo.SID_Type)
   is
      pragma Unreferenced (SID);

      use type Interfaces.Unsigned_32;

      FLR_Cap_Bit  : constant := 16#10000000#;

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

      if (Dummy32 and FLR_Cap_Bit) = FLR_Cap_Bit then
         Put_Line (Item => " FLR capability      : True");
      else
         Put_Line (Item => " FLR capability      : False");
      end if;

      Dummy16 := Caps.Device_Control;
      Put_Line (Item => " Device Control      : " & SK.Strings.Img (Dummy16));
      Dummy16 := Caps.Device_Status;
      Put_Line (Item => " Device Status       : " & SK.Strings.Img (Dummy16));
   end Print_PCIe_Capability_Structure;

end Mupci.Config_Space.Debug;
