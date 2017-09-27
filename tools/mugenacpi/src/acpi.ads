--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

package Acpi
is

   type Range_3 is range 1 .. 3;
   type Range_4 is range 1 .. 4;
   type Range_6 is range 1 .. 6;
   type Range_8 is range 1 .. 8;
   type ID_3 is array (Range_3) of Interfaces.Unsigned_8;
   type ID_4 is array (Range_4) of Interfaces.Unsigned_8;
   type ID_6 is array (Range_6) of Interfaces.Unsigned_8;
   type ID_8 is array (Range_8) of Interfaces.Unsigned_8;

   function To_ID_4 (Str : String) return ID_4;
   function To_ID_6 (Str : String) return ID_6;
   function To_ID_8 (Str : String) return ID_8;

   type System_Description_Table_Header is record
      Signature        : ID_4;
      Length           : Interfaces.Unsigned_32;
      Revision         : Interfaces.Unsigned_8;
      Checksum         : Interfaces.Unsigned_8;
      OEMID            : ID_6;
      OEM_Table_ID     : ID_8;
      OEM_Revision     : Interfaces.Unsigned_32;
      Creator_ID       : ID_4;
      Creator_Revision : ID_4;
   end record;
   pragma Pack (System_Description_Table_Header);

   -------------------------------------------------------------------------

   type Generic_Address_Structure is record
      Address_Space_ID    : Interfaces.Unsigned_8;
      Register_Bit_Width  : Interfaces.Unsigned_8;
      Register_Bit_Offset : Interfaces.Unsigned_8;
      Access_Size         : Interfaces.Unsigned_8;
      Address             : Interfaces.Unsigned_64;
   end record;
   pragma Pack (Generic_Address_Structure);

   Null_Generic_Address : constant Generic_Address_Structure :=
     (Address_Space_ID    => 16#00#,
      Register_Bit_Width  => 16#00#,
      Register_Bit_Offset => 16#00#,
      Access_Size         => 16#00#,
      Address             => 16#0000_0000_0000_0000#);

   -------------------------------------------------------------------------

   type Root_System_Description_Pointer is record             --  Revision 2
      Signature         : ID_8;
      Checksum          : Interfaces.Unsigned_8;
      OEMID             : ID_6;
      Revision          : Interfaces.Unsigned_8;
      RsdtAddress       : Interfaces.Unsigned_32;
      Length            : Interfaces.Unsigned_32;
      XsdtAddress       : Interfaces.Unsigned_64;
      Extended_Checksum : Interfaces.Unsigned_8;
      Reserved          : ID_3;
   end record;
   pragma Pack (Root_System_Description_Pointer);

   -------------------------------------------------------------------------

   type XSDT_Entries is array (1 .. 1) of Interfaces.Unsigned_64;

   type Extended_System_Description_Table is record           --  Revision 1
      Header  : System_Description_Table_Header;
      Entries : XSDT_Entries;
   end record;
   pragma Pack (Extended_System_Description_Table);

   -------------------------------------------------------------------------

   IAPC_LEGACY_DEVICES       : constant := 16#0001#;          --  Revision 2
   IAPC_8042                 : constant := 16#0002#;          --  Revision 3
   IAPC_VGA_Not_Present      : constant := 16#0004#;          --  Revision 4
   IAPC_MSI_Not_Supported    : constant := 16#0008#;          --  Revision 4
   IAPC_PCIe_ASPM_Controls   : constant := 16#0010#;          --  Revision 4
   IAPC_CMOS_RTC_Not_Present : constant := 16#0020#;          --  Revision 5

   FADT_WBINVD               : constant := 16#0000_0001#;     --  Revision 1
   FADT_WBINVD_FLUSH         : constant := 16#0000_0002#;     --  Revision 1
   FADT_C1_SUPPORTED         : constant := 16#0000_0004#;     --  Revision 1
   FADT_C2_MP_SUPPORTED      : constant := 16#0000_0008#;     --  Revision 1
   FADT_POWER_BUTTON         : constant := 16#0000_0010#;     --  Revision 1
   FADT_SLEEP_BUTTON         : constant := 16#0000_0020#;     --  Revision 1
   FADT_FIXED_RTC            : constant := 16#0000_0040#;     --  Revision 1
   FADT_S4_RTC_WAKE          : constant := 16#0000_0080#;     --  Revision 1
   FADT_32BIT_TIMER          : constant := 16#0000_0100#;     --  Revision 1
   FADT_DOCKING_SUPPORTED    : constant := 16#0000_0200#;     --  Revision 1
   FADT_RESET_REGISTER       : constant := 16#0000_0400#;     --  Revision 2
   FADT_SEALED_CASE          : constant := 16#0000_0800#;     --  Revision 3
   FADT_HEADLESS             : constant := 16#0000_1000#;     --  Revision 3
   FADT_SLEEP_TYPE           : constant := 16#0000_2000#;     --  Revision 3
   FADT_PCI_EXPRESS_WAKE     : constant := 16#0000_4000#;     --  Revision 4
   FADT_PLATFORM_CLOCK       : constant := 16#0000_8000#;     --  Revision 4
   FADT_S4_RTC_VALID         : constant := 16#0001_0000#;     --  Revision 4
   FADT_REMOTE_POWER_ON      : constant := 16#0002_0000#;     --  Revision 4
   FADT_APIC_CLUSTER         : constant := 16#0004_0000#;     --  Revision 4
   FADT_APIC_PHYSICAL        : constant := 16#0008_0000#;     --  Revision 4
   FADT_HW_REDUCED           : constant := 16#0010_0000#;     --  Revision 5
   FADT_LOW_POWER_S0         : constant := 16#0020_0000#;     --  Revision 5

   type Fixed_ACPI_Description_Table is record                --  Revision 5
      Header               : System_Description_Table_Header;
      FIRMWARE_CTRL        : Interfaces.Unsigned_32;
      DSDT                 : Interfaces.Unsigned_32;
      Reserved_1           : Interfaces.Unsigned_8;
      Preferred_PM_Profile : Interfaces.Unsigned_8;
      SCI_INT              : Interfaces.Unsigned_16;
      SMI_CMD              : Interfaces.Unsigned_32;
      ACPI_ENABLE          : Interfaces.Unsigned_8;
      ACPI_DISABLE         : Interfaces.Unsigned_8;
      S4BIOS_REQ           : Interfaces.Unsigned_8;
      PSTATE_CNT           : Interfaces.Unsigned_8;
      PM1a_EVT_BLK         : Interfaces.Unsigned_32;
      PM1b_EVT_BLK         : Interfaces.Unsigned_32;
      PM1a_CNT_BLK         : Interfaces.Unsigned_32;
      PM1b_CNT_BLK         : Interfaces.Unsigned_32;
      PM2_CNT_BLK          : Interfaces.Unsigned_32;
      PM_TMR_BLK           : Interfaces.Unsigned_32;
      GPE0_BLK             : Interfaces.Unsigned_32;
      GPE1_BLK             : Interfaces.Unsigned_32;
      PM1_EVT_LEN          : Interfaces.Unsigned_8;
      PM1_CNT_LEN          : Interfaces.Unsigned_8;
      PM2_CNT_LEN          : Interfaces.Unsigned_8;
      PM_TMR_LEN           : Interfaces.Unsigned_8;
      GPE0_BLK_LEN         : Interfaces.Unsigned_8;
      GPE1_BLK_LEN         : Interfaces.Unsigned_8;
      GPE1_BASE            : Interfaces.Unsigned_8;
      CST_CNT              : Interfaces.Unsigned_8;
      P_LVL2_LAT           : Interfaces.Unsigned_16;
      P_LVL3_LAT           : Interfaces.Unsigned_16;
      FLUSH_SIZE           : Interfaces.Unsigned_16;
      FLUSH_STRIDE         : Interfaces.Unsigned_16;
      DUTY_OFFSET          : Interfaces.Unsigned_8;
      DUTY_WIDTH           : Interfaces.Unsigned_8;
      DAY_ALRM             : Interfaces.Unsigned_8;
      MON_ALRM             : Interfaces.Unsigned_8;
      CENTURY              : Interfaces.Unsigned_8;
      IAPC_BOOT_ARCH       : Interfaces.Unsigned_16;
      Reserved_2           : Interfaces.Unsigned_8;
      Flags                : Interfaces.Unsigned_32;
      RESET_REG            : Generic_Address_Structure;
      RESET_VALUE          : Interfaces.Unsigned_8;
      Reserved_3           : ID_3;
      X_FIRMWARE_CTRL      : Interfaces.Unsigned_64;
      X_DSDT               : Interfaces.Unsigned_64;
      X_PM1a_EVT_BLK       : Generic_Address_Structure;
      X_PM1b_EVT_BLK       : Generic_Address_Structure;
      X_PM1a_CNT_BLK       : Generic_Address_Structure;
      X_PM1b_CNT_BLK       : Generic_Address_Structure;
      X_PM2_CNT_BLK        : Generic_Address_Structure;
      X_PM_TMR_BLK         : Generic_Address_Structure;
      X_GPE0_BLK           : Generic_Address_Structure;
      X_GPE1_BLK           : Generic_Address_Structure;
      SLEEP_CONTROL_REG    : Generic_Address_Structure;
      SLEEP_STATUS_REG     : Generic_Address_Structure;
   end record;
   pragma Pack (Fixed_ACPI_Description_Table);

   -------------------------------------------------------------------------

   generic
      Length : Positive;
      type Table_T is private;
   function Fixed_Length_Checksum
     (Table : Table_T)
      return Interfaces.Unsigned_8;

   generic
      type Table_T is private;
   function Checksum (Table : Table_T) return Interfaces.Unsigned_8;

end Acpi;
