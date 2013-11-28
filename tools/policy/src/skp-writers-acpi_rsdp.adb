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

package body Skp.Writers.ACPI_RSDP
is

   procedure Write
     (ACPI_Tables_Base : SK.Word64;
      Filename         : String)
   is
      use Ada.Streams.Stream_IO;
      use type SK.Word64;
      use type SK.Byte;

      function RSDP_Checksum is new ACPI.Fixed_Length_Checksum
        (Length  => 20,
         Table_T => ACPI.Root_System_Description_Pointer);
      function RSDP_Extended_Checksum is new ACPI.Checksum
        (Table_T => ACPI.Root_System_Description_Pointer);

      File : Ada.Streams.Stream_IO.File_Type;
      RSDP : ACPI.Root_System_Description_Pointer;
   begin
      RSDP := ACPI.Root_System_Description_Pointer'
        (Signature         => ACPI.To_ID_8 ("RSD PTR"),
         Checksum          => 16#00#,
         OEMID             => ACPI.To_ID_6 ("Muen"),
         Revision          => 2,
         RsdtAddress       => 16#0000_0000#,
         Length            => 16#0000_0024#,
         XsdtAddress       => ACPI_Tables_Base + ACPI.XSDT_Offset,
         Extended_Checksum => 16#00#,
         Reserved          => (others => 16#00#));

      RSDP.Checksum          := RSDP_Checksum (Table => RSDP);
      RSDP.Extended_Checksum := RSDP_Extended_Checksum (Table => RSDP);

      Open (Filename => Filename,
            File     => File);
      ACPI.Root_System_Description_Pointer'Write (Stream (File => File), RSDP);
      Close (File => File);
   end Write;

end Skp.Writers.ACPI_RSDP;
