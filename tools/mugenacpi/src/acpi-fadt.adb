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

with Ada.Streams.Stream_IO;

with Mutools.Files;

package body Acpi.FADT
is

   -------------------------------------------------------------------------

   procedure Write
     (DSDT_Address : Interfaces.Unsigned_64;
      Filename     : String)
   is
      use type Interfaces.Unsigned_32;
      use type Interfaces.Unsigned_16;

      function FADT_Checksum is new Checksum
        (Table_T => Fixed_ACPI_Description_Table);

      File : Ada.Streams.Stream_IO.File_Type;
      FADT : Fixed_ACPI_Description_Table;
   begin
      FADT := Fixed_ACPI_Description_Table'
        (Header => System_Description_Table_Header'
           (Signature        => To_ID_4 ("FACP"),
            Length           => FADT'Size / 8,
            Revision         => 5,
            Checksum         => 16#00#,
            OEMID            => To_ID_6 ("Muen"),
            OEM_Table_ID     => To_ID_8 (" first "),
            OEM_Revision     => 0,
            Creator_ID       => To_ID_4 ("SKP"),
            Creator_Revision => To_ID_4 ("DRTY")),
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
         IAPC_BOOT_ARCH       => IAPC_VGA_Not_Present    or
                                 IAPC_PCIe_ASPM_Controls,
         Reserved_2           => 16#00#,
         Flags                => FADT_POWER_BUTTON       or
                                 FADT_SLEEP_BUTTON       or
                                 FADT_FIXED_RTC          or
                                 FADT_HEADLESS           or
                                 FADT_HW_REDUCED,
         RESET_REG            => Null_Generic_Address,
         RESET_VALUE          => 16#00#,
         Reserved_3           => (others => 16#00#),
         X_FIRMWARE_CTRL      => 16#0000_0000_0000_0000#,
         X_DSDT               => DSDT_Address,
         others               => Null_Generic_Address);

      FADT.Header.Checksum := FADT_Checksum (Table => FADT);

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Fixed_ACPI_Description_Table'Write
        (Ada.Streams.Stream_IO.Stream (File => File), FADT);
      Ada.Streams.Stream_IO.Close (File => File);
   end Write;

end Acpi.FADT;
