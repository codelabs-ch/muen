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

package body Mupci.Config_Space
is

   FLR_Initiate : constant := 16#8000#;

   --  Access PCIe capability structure at given address and reset device.
   procedure Reset_Device_FLR (Address : Interfaces.Unsigned_64);

   -------------------------------------------------------------------------

   procedure Check_BARs
     (Device  :     Device_Type;
      Success : out Boolean)
   is
      use type Interfaces.Unsigned_32;

      Cmd_Register   : Interfaces.Unsigned_16;
      Regvalue, Size : Interfaces.Unsigned_32;
   begin
      Success := True;

      for I in Device.BARs'Range loop
         Regvalue := Space (Device.SID).Header.Base_Address_Registers (I);

         if Device.BARs (I).Register_Value /= Regvalue then
            Success := False;
            return;
         end if;

         if Device.BARs (I) /= Null_BAR then

            --  Check size, see PCI Express Base Specification 6.2,
            --  7.5.1.2.1 Base Address Registers,
            --  Implementation note about sizing base address registers.

            --  Disable decode.

            Cmd_Register := Space (Device.SID).Header.Command;
            Cmd_Register := Cmd_Register and not 16#3#;
            Space (Device.SID).Header.Command := Cmd_Register;

            Space (Device.SID).Header.Base_Address_Registers (I)
            := Interfaces.Unsigned_32'Last;
            Size := Space (Device.SID).Header.Base_Address_Registers (I);

            if (Regvalue and 1) = 1 then

               --  PIO, clear first two bits.

               Size := Size and not 16#3#;
            else

               --  Memory, clear first 4 bits.

               Size := Size and not 16#f#;
            end if;

            Size := not Size + 1;

            if Device.BARs (I).Size /= Size then
               Success := False;
               return;
            end if;

            --  Restore register value.

            Space (Device.SID).Header.Base_Address_Registers (I) := Regvalue;

            --  Re-enable decode.

            Cmd_Register := Space (Device.SID).Header.Command;
            Cmd_Register := Cmd_Register or 16#3#;
            Space (Device.SID).Header.Command := Cmd_Register;
         end if;
      end loop;
   end Check_BARs;

   -------------------------------------------------------------------------

   procedure Check_Vendor_Device
     (Device  :     Device_Type;
      Success : out Boolean)
   is
      Vendor_ID : constant Interfaces.Unsigned_16
        := Space (Device.SID).Header.Vendor_ID;
      Device_ID : constant Interfaces.Unsigned_16
        := Space (Device.SID).Header.Device_ID;
   begin
      Success := Vendor_ID = Device.Vendor_ID and Device_ID = Device.Device_ID;
   end Check_Vendor_Device;

   -------------------------------------------------------------------------

   procedure Reset
     (Device  :     Device_Type;
      Success : out Boolean)
   is
      Offset : Dev_Specific_Range;
   begin
      Get_PCIe_Capability
        (Device  => Device,
         ID      => PCI_Express_Capability,
         Offset  => Offset,
         Success => Success);
      if Success then
         Reset_Device_FLR (Address => Mmconf_Register
           (SID    => Device.SID,
            Offset => Offset));
      end if;
   end Reset;

   -------------------------------------------------------------------------

   procedure Reset_Device_FLR (Address : Interfaces.Unsigned_64)
   with
      SPARK_Mode => Off
   is
      Caps : PCIe_Cap_Struct_Type
      with
         Import,
         Address => System'To_Address (Address);

      Ctrl_Val : constant Interfaces.Unsigned_16 := Caps.Device_Control;
   begin
      -- PCI Express Base Specification 6.2, 6.6.2 Function Level Reset (FLR).
      Caps.Device_Control := Ctrl_Val or FLR_Initiate;
      --  TODO: real delay
      for I in 0 .. 300000 loop
         null;
      end loop;

      --  TODO: Check if reset is successful
   end Reset_Device_FLR;

end Mupci.Config_Space;
