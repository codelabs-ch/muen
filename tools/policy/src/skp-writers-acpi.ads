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

   type Root_System_Description_Pointer is record
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

   type Extended_System_Description_Table is record
      Header   : System_Description_Table_Header;
      Entries  : XSDT_Entries;
   end record;
   pragma Pack (Extended_System_Description_Table);

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

end Skp.Writers.ACPI;
