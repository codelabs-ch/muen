--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with Mutools.PCI;
with Mutools.Constants;

package VTd.Tables.DMAR
is

   type Table_Index_Type is range 0 .. 255;

   package MC renames Mutools.Constants;

   --  Pointer used to reference context and address translation tables
   --  (assuming a Host Address Width (HAW) of 35 bits).
   type Table_Pointer_Type is range 0 .. 2 ** 35 - 1
     with
       Size              => 35,
       Dynamic_Predicate => Table_Pointer_Type mod MC.Page_Size = 0;

   --  DMAR root table, see Intel VT-d specification, section 9.1.
   type Root_Table_Type is private;

   --  Add an entry to given DMAR root table with specified bus number and
   --  Context-table Pointer (CTP).
   procedure Add_Entry
     (RT  : in out Root_Table_Type;
      Bus :        Table_Index_Type;
      CTP :        Table_Pointer_Type);

   --  Serialize given root table to file with specified filename.
   procedure Serialize
     (RT       : Root_Table_Type;
      Filename : String);

   --  DMAR context table, see Intel VT-d specification, section 9.3.
   type Context_Table_Type is private;

   type Domain_Range is range 1 .. 255;

   subtype Paging_Level is Positive range 3 .. 4;

   --  Add an entry to given DMAR context table for given device, function with
   --  specified domain identifier and a pointer to the base of second-level
   --  paging entries (SLPTPTR : second-level page-table pointer). The paging
   --  level parameter specifies the number of levels of the associated
   --  second-level page-tables.
   procedure Add_Entry
     (CT        : in out Context_Table_Type;
      Device    :        Mutools.PCI.Device_Range;
      Func      :        Mutools.PCI.Function_Range;
      Domain    :        Domain_Range;
      PT_Levels :        Paging_Level;
      SLPTPTR   :        Table_Pointer_Type);

   --  Serialize given context table to file with specified filename.
   procedure Serialize
     (CT       : Context_Table_Type;
      Filename : String);

private

   type Aligned_Pointer_Type is range 0 .. 2 ** 23 - 1
     with
       Size => 23;

   type Root_Entry_Type is record
      Present    : Bit_Type             := 0;
      Reserved_1 : Bit_Array (1 .. 11)  := (others => 0);
      CTP        : Aligned_Pointer_Type := 0;
      Reserved_2 : Bit_Array (1 .. 93)  := (others => 0);
   end record
     with
       Size => 128;

   for Root_Entry_Type use record
      Present    at 0 range 0  .. 0;
      Reserved_1 at 0 range 1  .. 11;
      CTP        at 0 range 12 .. 34;
      Reserved_2 at 0 range 35 .. 127;
   end record;

   type Root_Entry_Array is array (Table_Index_Type) of Root_Entry_Type
     with
       Size => 256 * 128;

   type Root_Table_Type is record
      Entries : Root_Entry_Array;
   end record
     with
       Size => 256 * 128;

   AGAW_39_Bit       : constant Bit_Array (1 .. 3) := (1 => 1, others => 0);
   AGAW_48_Bit       : constant Bit_Array (1 .. 3) := (2 => 1, others => 0);
   Level_2_Translate : constant Bit_Array (1 .. 2) := (others => 0);

   type Context_Entry_Type is record
      Present    : Bit_Type               := 0;
      FPD        : Bit_Type               := 0;
      T          : Bit_Array (1 .. 2)     := Level_2_Translate;
      Reserved_1 : Bit_Array (1 .. 8)     := (others => 0);
      SLPTPTR    : Aligned_Pointer_Type   := 0;
      Reserved_2 : Bit_Array (1 .. 29)    := (others => 0);
      AW         : Bit_Array (1 .. 3)     := (others => 0);
      IGN        : Bit_Array (1 .. 4)     := (others => 0);
      Reserved_3 : Bit_Type               := 0;
      DID        : Interfaces.Unsigned_16 := 0;
      Reserved_4 : Bit_Array (1 .. 40)    := (others => 0);
   end record
     with
       Size => 128;

   for Context_Entry_Type use record
      Present    at 0 range  0 .. 0;
      FPD        at 0 range  1 .. 1;
      T          at 0 range  2 .. 3;
      Reserved_1 at 0 range  4 .. 11;
      SLPTPTR    at 0 range 12 .. 34;
      Reserved_2 at 0 range 35 .. 63;
      AW         at 0 range 64 .. 66;
      IGN        at 0 range 67 .. 70;
      Reserved_3 at 0 range 71 .. 71;
      DID        at 0 range 72 .. 87;
      Reserved_4 at 0 range 88 .. 127;
   end record;

   type Context_Entry_Array is array (Table_Index_Type) of Context_Entry_Type
     with
       Size => 256 * 128;

   type Context_Table_Type is record
      Entries : Context_Entry_Array;
   end record
     with
       Size => 256 * 128;

end VTd.Tables.DMAR;
