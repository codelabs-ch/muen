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

   --  Return memory resource string for given parameters.
   function Get_Mem_Resource_String
     (Descr     : String;
      Base_Addr : Interfaces.Unsigned_64;
      End_Addr  : Interfaces.Unsigned_64;
      Size      : Interfaces.Unsigned_64;
      Cacheable : Boolean)
      return String;

   -------------------------------------------------------------------------

   function DWordMemory
     (Base_Address : Interfaces.Unsigned_32;
      Size         : Interfaces.Unsigned_32;
      Cacheable    : Boolean)
      return String
   is
      use type Interfaces.Unsigned_32;
   begin
      return Get_Mem_Resource_String
        (Descr     => "DWordMemory",
         Base_Addr => Interfaces.Unsigned_64 (Base_Address),
         End_Addr  => Interfaces.Unsigned_64 (Base_Address + Size - 1),
         Size      => Interfaces.Unsigned_64 (Size),
         Cacheable => Cacheable);
   end DWordMemory;

   -------------------------------------------------------------------------

   function Get_Mem_Resource_String
     (Descr     : String;
      Base_Addr : Interfaces.Unsigned_64;
      End_Addr  : Interfaces.Unsigned_64;
      Size      : Interfaces.Unsigned_64;
      Cacheable : Boolean)
      return String
   is
      Buffer : Unbounded_String := To_Unbounded_String
        (Descr & " (ResourceProducer, PosDecode, MinFixed, MaxFixed, "
         & (if Cacheable then "" else "Non") & "Cacheable, ReadWrite, 0x0,");
   begin
      Buffer := Buffer & " 0x" & Mutools.Utils.To_Hex
        (Number    => Base_Addr,
         Normalize => False) & ",";
      Buffer := Buffer & " 0x" & Mutools.Utils.To_Hex
        (Number    => End_Addr,
         Normalize => False) & ",";
      Buffer := Buffer & " 0x0,";
      Buffer := Buffer & " 0x" & Mutools.Utils.To_Hex
        (Number    => Size,
         Normalize => False) & ",";
      Buffer := Buffer & ",,, AddressRangeMemory, TypeStatic)";

      return To_String (Buffer);
   end Get_Mem_Resource_String;

   -------------------------------------------------------------------------

   function IO
     (Start_Port : Interfaces.Unsigned_16;
      Port_Range : Interfaces.Unsigned_16)
      return String
   is
      Start  : constant String  := Mutools.Utils.To_Hex
        (Number    => Interfaces.Unsigned_64 (Start_Port),
         Normalize => False);
      Buffer : Unbounded_String := To_Unbounded_String ("IO (Decode16,");
   begin
      Buffer := Buffer & " 0x" & Start & ", 0x" & Start;
      Buffer := Buffer & ", 0x08, 0x" & Mutools.Utils.To_Hex
        (Number    => Interfaces.Unsigned_64 (Port_Range),
         Normalize => False) & ",)";

      return To_String (Buffer);
   end IO;

   -------------------------------------------------------------------------

   function IRQNoFlags (Number : Interfaces.Unsigned_8) return String
   is
   begin
      return "IRQNoFlags () {" & Number'Img & " }";
   end IRQNoFlags;

   -------------------------------------------------------------------------

   function QWordMemory
     (Base_Address : Interfaces.Unsigned_64;
      Size         : Interfaces.Unsigned_64;
      Cacheable    : Boolean)
      return String
   is
      use type Interfaces.Unsigned_64;
   begin
      return Get_Mem_Resource_String
        (Descr     => "QWordMemory",
         Base_Addr => Base_Address,
         End_Addr  => Base_Address + Size - 1,
         Size      => Size,
         Cacheable => Cacheable);
   end QWordMemory;

   -------------------------------------------------------------------------

end Acpi.Asl;
