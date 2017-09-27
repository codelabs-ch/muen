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

package body Acpi.XSDT
is

   -------------------------------------------------------------------------

   procedure Write
     (FADT_Address : Interfaces.Unsigned_64;
      Filename     : String)
   is
      use Ada.Streams.Stream_IO;
      use type Interfaces.Unsigned_32;

      function XSDT_Checksum is new Checksum
        (Table_T => Extended_System_Description_Table);

      File : Ada.Streams.Stream_IO.File_Type;
      XSDT : Extended_System_Description_Table;
   begin
      XSDT := Extended_System_Description_Table'
        (Header  => System_Description_Table_Header'
           (Signature        => To_ID_4 ("XSDT"),
            Length           => XSDT'Size / 8,
            Revision         => 1,
            Checksum         => 16#00#,
            OEMID            => To_ID_6 ("Muen"),
            OEM_Table_ID     => To_ID_8 ("1 entry"),
            OEM_Revision     => 0,
            Creator_ID       => To_ID_4 ("SKP"),
            Creator_Revision => To_ID_4 ("DRTY")),
         Entries => XSDT_Entries'(1 => FADT_Address));

      XSDT.Header.Checksum := XSDT_Checksum (Table => XSDT);

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Extended_System_Description_Table'Write
        (Stream (File => File), XSDT);
      Close (File => File);
   end Write;

end Acpi.XSDT;
