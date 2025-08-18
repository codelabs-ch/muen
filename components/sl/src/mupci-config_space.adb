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
   PCI_Cmd_Intx_Disable : constant := 16#0400#;

   PCIe_Cap_Dev_Status_Transaction_Pending : constant := 16#0020#;
   PCIe_Cap_FLR_Initiate                   : constant := 16#8000#;

   --  Access PCIe capability structure at given address and reset device.
   procedure Reset_Device_FLR (Address : Interfaces.Unsigned_64);
   --  TODO: Add pre that access is within Capabiltiy_Range of device
   --        with given SID.

   --  Busyloop given milliseconds.
   procedure Wait (Milliseconds : Interfaces.Unsigned_64);

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
      use type Interfaces.Unsigned_8;

      ID_Value : constant Interfaces.Unsigned_8
        := Capability_ID_Type'Enum_Rep (ID);
      Cap_ID   : Interfaces.Unsigned_8;
      Index    : Interfaces.Unsigned_16
        := Interfaces.Unsigned_16 (Space (SID).Header.Capabilities_Pointer);
   begin
      Success := False;
      Offset  := Capability_Ptr_Type'First;

      loop
         exit when Index = 0 or not (Index in Capability_Ptr_Type);
         Cap_ID := Space (SID).Dev_Specific (Index);
         if Cap_ID = ID_Value then
            Offset  := Index;
            Success := True;
            return;
         end if;
         Index := Interfaces.Unsigned_16 (Space (SID).Dev_Specific (Index + 1));
      end loop;
   end Get_PCI_Capability;

   -------------------------------------------------------------------------

   procedure Reset
     (Device  :     Device_Type;
      Success : out Boolean)
   is
      Offset       : Capability_Ptr_Type;
      Cmd_Register : Interfaces.Unsigned_16;
   begin

      Cmd_Register := Space (Device.SID).Header.Command;

      --  PCI Express Base Specification 6.2, 6.6.2 Function Level Reset (FLR).
      --  Impl. Note "Avoiding data corruption from stale completions",
      --  Item 2: Software clears the command register.

      --  Explanation taken from Linux kernel, pci_dev_save_and_disable.
      --
      --  Disable the device by clearing the Command register, except for
      --  INTx-disable which is set.  This not only disables MMIO and I/O port
      --  BARs, but also prevents the device from being Bus Master, preventing
      --  DMA from the device including MSI/MSI-X interrupts.  For PCI 2.3
      --  compliant devices, INTx-disable prevents legacy interrupts.

      Space (Device.SID).Header.Command := PCI_Cmd_Intx_Disable;

      case Device.Reset is
         when Reset_Method_FLR =>
            Get_PCI_Capability
              (SID     => Device.SID,
               ID      => PCI_Express_Capability,
               Offset  => Offset,
               Success => Success);
            if not Success then
               return;
            end if;

            Reset_Device_FLR (Address => Mmconf_Register
              (SID    => Device.SID,
               Offset => Offset));

            --  Wait for device ready: wait for an additional 1 sec as per
            --  conventional device reset.

            Wait_For_Device
              (SID        => Device.SID,
               Timeout_MS => 1000,
               Success    => Success);
         when others =>
            Success := False;
      end case;

      Space (Device.SID).Header.Command := Cmd_Register;
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

      --  PCI Express Base Specification 6.2, 6.6.2 Function Level Reset (FLR).
      --  Impl. Note "Avoiding data corruption from stale completions",
      --  Item 3: Wait for pending transactions.

      for I in 1 .. 10 loop
         exit when
           (Caps.Device_Status and PCIe_Cap_Dev_Status_Transaction_Pending) = 0;
         Wait (Milliseconds => 100);
      end loop;

      --  PCI Express Base Specification 6.2, 6.6.2 Function Level Reset (FLR).
      --  Impl. Note "Avoiding data corruption from stale completions",
      --  Item 4: Software initiates FLR.

      Caps.Device_Control := Ctrl_Val or PCIe_Cap_FLR_Initiate;

      --  TODO: Implement Immediate Ready support

      --  PCI Express Base Specification 6.2, 6.6.2 Function Level Reset (FLR).
      --  Impl. Note "Avoiding data corruption from stale completions",
      --  Item 5: Software waits 100ms.

      Wait (Milliseconds => 100);
   end Reset_Device_FLR;

   -------------------------------------------------------------------------

   procedure Setup_BARs
     (Device  :     Device_Type;
      Success : out Boolean)
   is
      use type Interfaces.Unsigned_32;

      Regvalue : Interfaces.Unsigned_32;
   begin
      Success := True;

      Decode_Disable (SID => Device.SID);

      for I in Device.BARs'Range loop
         if Device.BARs (I) /= Null_BAR then
            Space (Device.SID).Header.Base_Address_Registers (I)
              := Device.BARs (I).Register_Value;

            Regvalue := Space (Device.SID).Header.Base_Address_Registers (I);
            if Device.BARs (I).Register_Value /= Regvalue then
               Success := False;
               return;
            end if;
         end if;
      end loop;

      Decode_Enable (SID => Device.SID);
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
     (SID        :     Musinfo.SID_Type;
      Timeout_MS :     Positive;
      Success    : out Boolean)
   is
      pragma Unreferenced (Timeout_MS);

      Cmd_Val   : Interfaces.Unsigned_16;
      Not_Ready : constant Interfaces.Unsigned_16 := 16#ffff#;
   begin

      --  TODO: Handle Request Retry Status completions if supported by device.

      --  Use command register instead of vendor ID to ignore CRS
      --  completions for now. See also Linux pci_dev_wait() function.

      for I in 1 .. 5 loop
         Cmd_Val := Space (SID).Header.Command;
         exit when Cmd_Val /= Not_Ready;
         Wait (Milliseconds => 200);
      end loop;

      Success := Cmd_Val /= Not_Ready;
   end Wait_For_Device;

end Mupci.Config_Space;
