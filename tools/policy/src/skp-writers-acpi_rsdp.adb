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

package body Skp.Writers.ACPI_RSDP
is

   type RSDP_Signature_T is array (1 .. 8) of SK.Byte;
   type RSDP_OEMID_T     is array (1 .. 6) of SK.Byte;

   type Root_System_Description_Pointer is record
      Signature   : RSDP_Signature_T;
      Checksum    : SK.Byte;
      OEMID       : RSDP_OEMID_T;
      Revision    : SK.Byte;
      RsdtAddress : SK.Word32;
   end record;
   pragma Pack (Root_System_Description_Pointer);

   -------------------------------------------------------------------------

   procedure Write (ACPI_Tables_Base : SK.Word64; Filename : String)
   is
      use Ada.Streams.Stream_IO;
      use type SK.Word64;
      use type SK.Word32;
      use type SK.Byte;

      File : Ada.Streams.Stream_IO.File_Type;
      RSDP : Root_System_Description_Pointer;
   begin
      if ACPI_Tables_Base > SK.Word64 (SK.Word32'Last) then
         raise Not_Supported with "64-bit addresses not supported (yet)";
      end if;
      RSDP := Root_System_Description_Pointer'
        (Signature   => (Character'Pos ('R'), Character'Pos ('S'),
                         Character'Pos ('D'), Character'Pos (' '),
                         Character'Pos ('P'), Character'Pos ('T'),
                         Character'Pos ('R'), Character'Pos (' ')),
         Checksum    => 16#00#,
         OEMID       => (Character'Pos ('M'), Character'Pos ('u'),
                         Character'Pos ('e'), Character'Pos ('n'),
                         Character'Pos (' '), Character'Pos (' ')),
         Revision    => 16#00#,
         RsdtAddress => SK.Word32 (ACPI_Tables_Base) + RSDP'Size / 8);

      RSDP.Checksum := 16#00# - (RSDP.Signature (1) + RSDP.Signature (2) +
                                 RSDP.Signature (3) + RSDP.Signature (4) +
                                 RSDP.Signature (5) + RSDP.Signature (6) +
                                 RSDP.Signature (7) + RSDP.Signature (8) +
                                 RSDP.OEMID (1) + RSDP.OEMID (2) +
                                 RSDP.OEMID (3) + RSDP.OEMID (4) +
                                 RSDP.OEMID (5) + RSDP.OEMID (6) +
                                 RSDP.Revision +
                                 SK.Byte
                                   ((RSDP.RsdtAddress / 2 **  0) and 16#ff#) +
                                 SK.Byte
                                   ((RSDP.RsdtAddress / 2 **  8) and 16#ff#) +
                                 SK.Byte
                                   ((RSDP.RsdtAddress / 2 ** 16) and 16#ff#) +
                                 SK.Byte
                                   ((RSDP.RsdtAddress / 2 ** 24) and 16#ff#));

      Open (Filename => Filename,
            File     => File);
      Root_System_Description_Pointer'Write (Stream (File => File), RSDP);
      Close (File => File);
   end Write;

end Skp.Writers.ACPI_RSDP;
