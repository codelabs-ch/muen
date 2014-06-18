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

package VTd.Tables
is

   --  DMAR root table, see Intel VT-d specification, section 9.1.
   type Root_Table_Type is private;

   --  I/O bus range.
   type Bus_Range is range 0 .. 255;

   --  A Context-table Pointer (CTP) points to a context table for a specific
   --  bus. Assuming a Host Address Width (HAW) of 35 bits, the CTP can be
   --  maximum 23 bits (i.e. context tables must be below 8 MiB).
   type CT_Pointer_Type is range 0 .. 2 ** 23 - 1
     with
       Size => 23;

   --  Add an entry to given DMAR root table with specified bus number and
   --  Context-table Pointer (CTP).
   procedure Add_Entry
     (RT  : in out Root_Table_Type;
      Bus :        Bus_Range;
      CTP :        CT_Pointer_Type);

   --  Serialize given root table to file with specified filename.
   procedure Serialize
     (RT       : Root_Table_Type;
      Filename : String);

private

   type Bit_Type is range 0 .. 1
     with
       Size => 1;

   type Bit_Array is array (Positive range <>) of Bit_Type
     with
       Pack;

   type Root_Entry_Type is record
      Present    : Bit_Type            := 0;
      Reserved_1 : Bit_Array (1 .. 11) := (others => 0);
      CTP        : CT_Pointer_Type     := 0;
      Reserved_2 : Bit_Array (1 .. 93) := (others => 0);
   end record
     with
       Size => 128;

   for Root_Entry_Type use record
      Present    at 0 range 0  .. 0;
      Reserved_1 at 0 range 1  .. 11;
      CTP        at 0 range 12 .. 34;
      Reserved_2 at 0 range 35 .. 127;
   end record;

   type Root_Entry_Array is array (Bus_Range) of Root_Entry_Type
     with
       Size => 256 * 128;

   type Root_Table_Type is record
      Entries : Root_Entry_Array;
   end record
     with
       Size => 256 * 128;

end VTd.Tables;
