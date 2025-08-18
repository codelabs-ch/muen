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
   PCI_Base_Address_Mem_Mask : constant := 16#f#;
   PCI_Base_Address_IO_Mask  : constant := 16#3#;

   PCI_Cmd_Spaces_Enable_Bits : constant := 16#0003#;

   --  Busyloop given milliseconds.
   procedure Wait (Milliseconds : Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   procedure Check_BARs
     (Device  : aliased in out Config_Space_Type;
      BARs    :                BAR_Array;
      Success :            out Boolean)
   is
      use type Interfaces.Unsigned_32;

      Regvalue, Size : Interfaces.Unsigned_32;
   begin
      Success := True;

      for I in BARs'Range loop
         Regvalue := Device.Header.Base_Address_Registers (I);

         if BARs (I).Register_Value /= Regvalue then
            Success := False;
            return;
         end if;

         if BARs (I) /= Null_BAR then

            --  Check size, see PCI Express Base Specification 6.2,
            --  7.5.1.2.1 Base Address Registers,
            --  Implementation note about sizing base address registers.

            Decode_Disable (Device => Device);

            Device.Header.Base_Address_Registers (I)
              := Interfaces.Unsigned_32'Last;
            Size := Device.Header.Base_Address_Registers (I);

            if (Regvalue and 1) = 1 then

               --  PIO, clear first two bits.

               Size := Size and not PCI_Base_Address_IO_Mask;
            else

               --  Memory, clear first 4 bits.

               Size := Size and not PCI_Base_Address_Mem_Mask;
            end if;

            Size := not Size + 1;

            if BARs (I).Size /= Size then
               Success := False;
               return;
            end if;

            --  Restore register value.

            Device.Header.Base_Address_Registers (I) := Regvalue;

            Decode_Enable (Device => Device);
         end if;
      end loop;
   end Check_BARs;

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

   -------------------------------------------------------------------------

   procedure Get_PCI_Capability
     (Device  : aliased     Config_Space_Type;
      ID      :             Capability_ID_Type;
      Offset  :         out Capability_Ptr_Type;
      Success :         out Boolean)
   is
      use type Interfaces.Unsigned_8;

      ID_Value : constant Interfaces.Unsigned_8 :=
        Capability_ID_Type'Enum_Rep (ID);
      Cap_ID   : Interfaces.Unsigned_8;
      Index    : Interfaces.Unsigned_16         :=
        Interfaces.Unsigned_16 (Device.Header.Capabilities_Pointer);
   begin
      Success := False;
      Offset  := Capability_Ptr_Type'First;

      loop
         exit when Index = 0 or not (Index in Capability_Ptr_Type);
         Cap_ID := Device.Dev_Specific (Index);
         if Cap_ID = ID_Value then
            Offset  := Index;
            Success := True;
            return;
         end if;
         Index := Interfaces.Unsigned_16 (Device.Dev_Specific (Index + 1));
      end loop;
   end Get_PCI_Capability;

   -------------------------------------------------------------------------

   procedure Setup_BARs
     (Device  : aliased in out Config_Space_Type;
      BARs    :                BAR_Array;
      Success :            out Boolean)
   is
      use type Interfaces.Unsigned_32;

      Regvalue : Interfaces.Unsigned_32;
   begin
      Success := True;

      Decode_Disable (Device => Device);

      for I in BARs'Range loop
         if BARs (I) /= Null_BAR then
            Device.Header.Base_Address_Registers (I)
              := BARs (I).Register_Value;

            Regvalue := Device.Header.Base_Address_Registers (I);
            if BARs (I).Register_Value /= Regvalue then
               Success := False;
               return;
            end if;
         end if;
      end loop;

      Decode_Enable (Device => Device);
   end Setup_BARs;

   --------------------------------------------------------------------------

   --  TODO: Implement real wait.
   procedure Wait (Milliseconds : Interfaces.Unsigned_64)
   with
      SPARK_Mode => Off
   is
      pragma Unreferenced (Milliseconds);
   begin
      for I in 1 .. 2000000 loop
         null;
      end loop;
   end Wait;

   -------------------------------------------------------------------------

   procedure Wait_For_Device
     (Device     : aliased     Config_Space_Type;
      Timeout_MS :             Positive;
      Divider    :             Positive := 5;
      Success    :         out Boolean)
   is
      use type Interfaces.Unsigned_64;

      Cmd_Val   : Interfaces.Unsigned_16          := 0;
      Not_Ready : constant Interfaces.Unsigned_16 := 16#ffff#;
   begin

      --  TODO: Handle Request Retry Status completions if supported by device.

      --  Use command register instead of vendor ID to ignore CRS
      --  completions for now. See also Linux pci_dev_wait() function.

      for I in 1 .. Divider loop
         Cmd_Val := Device.Header.Command;
         exit when Cmd_Val /= Not_Ready;
         Wait
           (Milliseconds =>
              Interfaces.Unsigned_64 (Timeout_MS) /
              Interfaces.Unsigned_64 (Divider));
      end loop;

      Success := Cmd_Val /= Not_Ready;
   end Wait_For_Device;

end Mupci.Config_Device;
