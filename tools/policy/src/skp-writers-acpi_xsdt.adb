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

package body Skp.Writers.ACPI_XSDT
is

   procedure Write
     (ACPI_Tables_Base : SK.Word64;
      Filename         : String)
   is
      use Ada.Streams.Stream_IO;
      use type SK.Word64;
      use type SK.Word32;
      use type SK.Byte;

      function XSDT_Checksum is new ACPI.Checksum
        (Table_T => ACPI.Extended_System_Description_Table);

      File : Ada.Streams.Stream_IO.File_Type;
      XSDT : ACPI.Extended_System_Description_Table;
   begin
      XSDT := ACPI.Extended_System_Description_Table'
        (Header   => ACPI.System_Description_Table_Header'
           (Signature         => ACPI.To_ID_4 ("XSDT"),
            Length            => XSDT'Size / 8,
            Revision          => 1,
            Checksum          => 16#00#,
            OEMID             => ACPI.To_ID_6 ("Muen"),
            OEM_Table_ID      => ACPI.To_ID_8 ("1 entry"),
            OEM_Revision      => 0,
            Creator_ID        => ACPI.To_ID_4 ("SKP"),
            Creator_Revision  => ACPI.To_ID_4 ("DRTY")),
         Entries  => ACPI.XSDT_Entries'
           (1 => ACPI_Tables_Base + ACPI.FADT_Offset));

      XSDT.Header.Checksum := XSDT_Checksum (Table => XSDT);

      Open (Filename => Filename,
            File     => File);
      ACPI.Extended_System_Description_Table'Write
        (Stream (File => File), XSDT);
      Close (File => File);
   end Write;

end Skp.Writers.ACPI_XSDT;
