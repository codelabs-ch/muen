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

private package Skp.Writers.ACPI
is

   use type SK.Word64;

   type Range_3 is range 1 .. 3;
   type Range_4 is range 1 .. 4;
   type Range_6 is range 1 .. 6;
   type Range_8 is range 1 .. 8;
   type ID_3 is array (Range_3) of SK.Byte;
   type ID_4 is array (Range_4) of SK.Byte;
   type ID_6 is array (Range_6) of SK.Byte;
   type ID_8 is array (Range_8) of SK.Byte;

   function To_ID_4 (Str : String) return ID_4;
   function To_ID_6 (Str : String) return ID_6;
   function To_ID_8 (Str : String) return ID_8;

   type System_Description_Table_Header is record
      Signature         : ID_4;
      Length            : SK.Word32;
      Revision          : SK.Byte;
      Checksum          : SK.Byte;
      OEMID             : ID_6;
      OEM_Table_ID      : ID_8;
      OEM_Revision      : SK.Word32;
      Creator_ID        : ID_4;
      Creator_Revision  : ID_4;
   end record;
   pragma Pack (System_Description_Table_Header);

   -------------------------------------------------------------------------

   type Generic_Address_Structure is record
      Address_Space_ID     : SK.Byte;
      Register_Bit_Width   : SK.Byte;
      Register_Bit_Offset  : SK.Byte;
      Access_Size          : SK.Byte;
      Address              : SK.Word64;
   end record;
   pragma Pack (Generic_Address_Structure);

   Null_Generic_Address : constant Generic_Address_Structure :=
     (Address_Space_ID     => 16#00#,
      Register_Bit_Width   => 16#00#,
      Register_Bit_Offset  => 16#00#,
      Access_Size          => 16#00#,
      Address              => 16#0000_0000_0000_0000#);

   -------------------------------------------------------------------------

   type Root_System_Description_Pointer is record              --  Revision 2
      Signature         : ID_8;
      Checksum          : SK.Byte;
      OEMID             : ID_6;
      Revision          : SK.Byte;
      RsdtAddress       : SK.Word32;
      Length            : SK.Word32;
      XsdtAddress       : SK.Word64;
      Extended_Checksum : SK.Byte;
      Reserved          : ID_3;
   end record;
   pragma Pack (Root_System_Description_Pointer);

   -------------------------------------------------------------------------

   type XSDT_Entries is array (1 .. 1) of SK.Word64;

   type Extended_System_Description_Table is record            --  Revision 1
      Header   : System_Description_Table_Header;
      Entries  : XSDT_Entries;
   end record;
   pragma Pack (Extended_System_Description_Table);

   -------------------------------------------------------------------------

   IAPC_LEGACY_DEVICES        : constant := 16#0001#;          --  Revision 2
   IAPC_8042                  : constant := 16#0002#;          --  Revision 3
   IAPC_VGA_Not_Present       : constant := 16#0004#;          --  Revision 4
   IAPC_MSI_Not_Supported     : constant := 16#0008#;          --  Revision 4
   IAPC_PCIe_ASPM_Controls    : constant := 16#0010#;          --  Revision 4
   IAPC_CMOS_RTC_Not_Present  : constant := 16#0020#;          --  Revision 5

   FADT_WBINVD                : constant := 16#0000_0001#;     --  Revision 1
   FADT_WBINVD_FLUSH          : constant := 16#0000_0002#;     --  Revision 1
   FADT_C1_SUPPORTED          : constant := 16#0000_0004#;     --  Revision 1
   FADT_C2_MP_SUPPORTED       : constant := 16#0000_0008#;     --  Revision 1
   FADT_POWER_BUTTON          : constant := 16#0000_0010#;     --  Revision 1
   FADT_SLEEP_BUTTON          : constant := 16#0000_0020#;     --  Revision 1
   FADT_FIXED_RTC             : constant := 16#0000_0040#;     --  Revision 1
   FADT_S4_RTC_WAKE           : constant := 16#0000_0080#;     --  Revision 1
   FADT_32BIT_TIMER           : constant := 16#0000_0100#;     --  Revision 1
   FADT_DOCKING_SUPPORTED     : constant := 16#0000_0200#;     --  Revision 1
   FADT_RESET_REGISTER        : constant := 16#0000_0400#;     --  Revision 2
   FADT_SEALED_CASE           : constant := 16#0000_0800#;     --  Revision 3
   FADT_HEADLESS              : constant := 16#0000_1000#;     --  Revision 3
   FADT_SLEEP_TYPE            : constant := 16#0000_2000#;     --  Revision 3
   FADT_PCI_EXPRESS_WAKE      : constant := 16#0000_4000#;     --  Revision 4
   FADT_PLATFORM_CLOCK        : constant := 16#0000_8000#;     --  Revision 4
   FADT_S4_RTC_VALID          : constant := 16#0001_0000#;     --  Revision 4
   FADT_REMOTE_POWER_ON       : constant := 16#0002_0000#;     --  Revision 4
   FADT_APIC_CLUSTER          : constant := 16#0004_0000#;     --  Revision 4
   FADT_APIC_PHYSICAL         : constant := 16#0008_0000#;     --  Revision 4
   FADT_HW_REDUCED            : constant := 16#0010_0000#;     --  Revision 5
   FADT_LOW_POWER_S0          : constant := 16#0020_0000#;     --  Revision 5

   type Fixed_ACPI_Description_Table is record                 --  Revision 5
      Header               : System_Description_Table_Header;
      FIRMWARE_CTRL        : SK.Word32;
      DSDT                 : SK.Word32;
      Reserved_1           : SK.Byte;
      Preferred_PM_Profile : SK.Byte;
      SCI_INT              : SK.Word16;
      SMI_CMD              : SK.Word32;
      ACPI_ENABLE          : SK.Byte;
      ACPI_DISABLE         : SK.Byte;
      S4BIOS_REQ           : SK.Byte;
      PSTATE_CNT           : SK.Byte;
      PM1a_EVT_BLK         : SK.Word32;
      PM1b_EVT_BLK         : SK.Word32;
      PM1a_CNT_BLK         : SK.Word32;
      PM1b_CNT_BLK         : SK.Word32;
      PM2_CNT_BLK          : SK.Word32;
      PM_TMR_BLK           : SK.Word32;
      GPE0_BLK             : SK.Word32;
      GPE1_BLK             : SK.Word32;
      PM1_EVT_LEN          : SK.Byte;
      PM1_CNT_LEN          : SK.Byte;
      PM2_CNT_LEN          : SK.Byte;
      PM_TMR_LEN           : SK.Byte;
      GPE0_BLK_LEN         : SK.Byte;
      GPE1_BLK_LEN         : SK.Byte;
      GPE1_BASE            : SK.Byte;
      CST_CNT              : SK.Byte;
      P_LVL2_LAT           : SK.Word16;
      P_LVL3_LAT           : SK.Word16;
      FLUSH_SIZE           : SK.Word16;
      FLUSH_STRIDE         : SK.Word16;
      DUTY_OFFSET          : SK.Byte;
      DUTY_WIDTH           : SK.Byte;
      DAY_ALRM             : SK.Byte;
      MON_ALRM             : SK.Byte;
      CENTURY              : SK.Byte;
      IAPC_BOOT_ARCH       : SK.Word16;
      Reserved_2           : SK.Byte;
      Flags                : SK.Word32;
      RESET_REG            : Generic_Address_Structure;
      RESET_VALUE          : SK.Byte;
      Reserved_3           : ID_3;
      X_FIRMWARE_CTRL      : SK.Word64;
      X_DSDT               : SK.Word64;
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
   function Fixed_Length_Checksum (Table : Table_T) return SK.Byte;

   generic
      type Table_T is private;
   function Checksum (Table : Table_T) return SK.Byte;

   -------------------------------------------------------------------------

   XSDT_Offset : SK.Word64 := Root_System_Description_Pointer'Size / 8;
   FADT_Offset : SK.Word64 := XSDT_Offset +
                              Extended_System_Description_Table'Size / 8;
   DSDT_Offset : SK.Word64 := FADT_Offset +
                              Fixed_ACPI_Description_Table'Size / 8;

end Skp.Writers.ACPI;
