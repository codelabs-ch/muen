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

with Ada.Strings.Unbounded;

with Mutools.Utils;

package body Acpi.Asl
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function DWordMemory
     (Base_Address : Interfaces.Unsigned_32;
      Size         : Interfaces.Unsigned_32)
      return String
   is
      use type Interfaces.Unsigned_32;

      End_Addr : constant Interfaces.Unsigned_32 := Base_Address + Size - 1;
      Buffer   : Unbounded_String := To_Unbounded_String
        ("DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,"
         & " Cacheable, ReadWrite, 0x0,");
   begin
      Buffer := Buffer & " 0x" & Mutools.Utils.To_Hex
        (Number     => Interfaces.Unsigned_64 (Base_Address),
         Normalize  => False) & ",";
      Buffer := Buffer & " 0x" & Mutools.Utils.To_Hex
        (Number     => Interfaces.Unsigned_64 (End_Addr),
         Normalize  => False) & ",";
      Buffer := Buffer & " 0x0,";
      Buffer := Buffer & " 0x" & Mutools.Utils.To_Hex
        (Number     => Interfaces.Unsigned_64 (Size),
         Normalize  => False) & ",";
      Buffer := Buffer & ",,, AddressRangeMemory, TypeStatic)";

      return To_String (Buffer);
   end DWordMemory;

end Acpi.Asl;
