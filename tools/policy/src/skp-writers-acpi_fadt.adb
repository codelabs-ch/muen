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

with Skp.Writers.ACPI;

package body Skp.Writers.ACPI_FADT
is

   procedure Write
     (ACPI_Tables_Base : SK.Word64;
      Filename         : String)
   is
      use Ada.Streams.Stream_IO;
      use type SK.Word64;
      use type SK.Word32;
      use type SK.Word16;
      use type SK.Byte;

      function FADT_Checksum is new ACPI.Checksum
        (Table_T => ACPI.Fixed_ACPI_Description_Table);

      File : Ada.Streams.Stream_IO.File_Type;
      FADT : ACPI.Fixed_ACPI_Description_Table;
   begin
      FADT := ACPI.Fixed_ACPI_Description_Table'
        (Header => ACPI.System_Description_Table_Header'
           (Signature        => ACPI.To_ID_4 ("FACP"),
            Length           => FADT'Size / 8,
            Revision         => 5,
            Checksum         => 16#00#,
            OEMID            => ACPI.To_ID_6 ("Muen"),
            OEM_Table_ID     => ACPI.To_ID_8 (" first "),
            OEM_Revision     => 0,
            Creator_ID       => ACPI.To_ID_4 ("SKP"),
            Creator_Revision => ACPI.To_ID_4 ("DRTY")),
         FIRMWARE_CTRL        => 16#0000_0000#,
         DSDT                 => 16#0000_0000#,
         Reserved_1           => 16#00#,
         Preferred_PM_Profile => 16#00#,
         SCI_INT              => 9,
         SMI_CMD              => 16#0000_0000#,
         ACPI_ENABLE          => 16#00#,
         ACPI_DISABLE         => 16#00#,
         S4BIOS_REQ           => 16#00#,
         PSTATE_CNT           => 16#00#,
         PM1a_EVT_BLK         => 16#0000_0000#,
         PM1b_EVT_BLK         => 16#0000_0000#,
         PM1a_CNT_BLK         => 16#0000_0000#,
         PM1b_CNT_BLK         => 16#0000_0000#,
         PM2_CNT_BLK          => 16#0000_0000#,
         PM_TMR_BLK           => 16#0000_0000#,
         GPE0_BLK             => 16#0000_0000#,
         GPE1_BLK             => 16#0000_0000#,
         PM1_EVT_LEN          => 16#00#,
         PM1_CNT_LEN          => 16#00#,
         PM2_CNT_LEN          => 16#00#,
         PM_TMR_LEN           => 16#00#,
         GPE0_BLK_LEN         => 16#00#,
         GPE1_BLK_LEN         => 16#00#,
         GPE1_BASE            => 16#00#,
         CST_CNT              => 16#00#,
         P_LVL2_LAT           => 16#0000#,
         P_LVL3_LAT           => 16#0000#,
         FLUSH_SIZE           => 16#0000#, --  TODO: Fill or set WBINVD
         FLUSH_STRIDE         => 16#0000#, --
         DUTY_OFFSET          => 16#00#,
         DUTY_WIDTH           => 16#00#,
         DAY_ALRM             => 16#00#,
         MON_ALRM             => 16#00#,
         CENTURY              => 16#00#,
         IAPC_BOOT_ARCH       => ACPI.IAPC_VGA_Not_Present     or
                                 ACPI.IAPC_MSI_Not_Supported   or
                                 ACPI.IAPC_PCIe_ASPM_Controls  or
                                 ACPI.IAPC_CMOS_RTC_Not_Present,
         Reserved_2           => 16#00#,
         Flags                => ACPI.FADT_POWER_BUTTON        or
                                 ACPI.FADT_SLEEP_BUTTON        or
                                 ACPI.FADT_FIXED_RTC           or
                                 ACPI.FADT_HEADLESS            or
                                 ACPI.FADT_HW_REDUCED,
         RESET_REG            => ACPI.Null_Generic_Address,
         RESET_VALUE          => 16#00#,
         Reserved_3           => (others => 16#00#),
         X_FIRMWARE_CTRL      => 16#0000_0000_0000_0000#,
         X_DSDT               => ACPI_Tables_Base + ACPI.DSDT_Offset,
         others               => ACPI.Null_Generic_Address);

      FADT.Header.Checksum := FADT_Checksum (Table => FADT);

      Open (Filename => Filename,
            File     => File);
      ACPI.Fixed_ACPI_Description_Table'Write
        (Stream (File => File), FADT);
      Close (File => File);
   end Write;

end Skp.Writers.ACPI_FADT;
