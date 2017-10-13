--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with SK.Strings;

with Debug_Ops;

package body Devices.Pciconf.Quirks
is

   use type SK.Word16;
   use type SK.Word32;

   --  See Intel Platform Controller Hub (PCH) specification.
   USB3_Intel_XUSB2PR : constant := 16#d0#;
   USB3_Intel_PSSEN   : constant := 16#d8#;

   -------------------------------------------------------------------------

   --  See drivers/usb/host/pci-quirks.c in Linux kernel, functions
   --  * usb_is_intel_switchable_xhci
   --  * usb_is_intel_lpt_switchable_xhci
   --  * usb_is_intel_ppt_switchable_xhci

   PCI_Vendor_Intel : constant := 16#8086#;

   PCI_Device_ID_Intel_Panther_Point_xHCI  : constant := 16#1e31#;
   PCI_Device_ID_Intel_Lynx_Point_xHCI     : constant := 16#8c31#;
   PCI_Device_ID_Intel_Lynx_Point_LP_xHCI  : constant := 16#9c31#;

   PCI_Class_Serial_Usb_xHCI : constant := 16#0c0330#;

   function USB_Intel_Panther_Switchable_xHCI
     (Vendor : SK.Word16;
      Device : SK.Word16;
      Class  : SK.Word32)
      return Boolean
   is (Vendor = PCI_Vendor_Intel
       and then Device = PCI_Device_ID_Intel_Panther_Point_xHCI
       and then Class  = PCI_Class_Serial_Usb_xHCI);

   function USB_Intel_Lynx_Switchable_xHCI
     (Vendor : SK.Word16;
      Device : SK.Word16;
      Class  : SK.Word32)
      return Boolean
   is (Vendor = PCI_Vendor_Intel
       and then Class = PCI_Class_Serial_Usb_xHCI
       and then (Device = PCI_Device_ID_Intel_Lynx_Point_xHCI
                 or Device = PCI_Device_ID_Intel_Lynx_Point_LP_xHCI));

   function USB_Intel_Switchable_xHCI
     (Vendor : SK.Word16;
      Device : SK.Word16;
      Class  : SK.Word32)
      return Boolean
   is (USB_Intel_Panther_Switchable_xHCI
       (Vendor => Vendor,
        Device => Device,
        Class  => Class)
       or else USB_Intel_Lynx_Switchable_xHCI
         (Vendor => Vendor,
          Device => Device,
          Class  => Class));

   -------------------------------------------------------------------------

   procedure Register
     (Vendor : SK.Word16;
      Device : SK.Word16;
      Class  : SK.Word32)
   with
      SPARK_Mode => Off
   is
   begin
      if USB_Intel_Switchable_xHCI
        (Vendor => Vendor,
         Device => Device,
         Class  => Class)
      then
         pragma Debug
           (Debug_Ops.Put_Line
              (Item => "Pciconf: Registering xHCI handoff quirk for vendor "
               & SK.Strings.Img (Vendor)& " device " & SK.Strings.Img (Device)
               & " class " & SK.Strings.Img (Class)));
         Append_Config (C => (Offset      => USB3_Intel_XUSB2PR,
                              Read_Mask   => Read_No_Virt,
                              Vread       => Vread_None,
                              Write_Perm  => Write_Direct,
                              Write_Width => Access_16,
                              Vwrite      => Vwrite_None));
         Append_Config (C => (Offset      => USB3_Intel_PSSEN,
                              Read_Mask   => Read_No_Virt,
                              Vread       => Vread_None,
                              Write_Perm  => Write_Direct,
                              Write_Width => Access_8,
                              Vwrite      => Vwrite_None));
      end if;
   end Register;

end Devices.Pciconf.Quirks;
