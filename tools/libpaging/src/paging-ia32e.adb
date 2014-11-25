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

with Mutools.Utils;

with Paging.Entries;

package body Paging.IA32e
is

   use type Interfaces.Unsigned_64;

   type Raw_Table_Type is array (Entry_Range) of Interfaces.Unsigned_64;

   --  Table entry flags.
   Present_Flag   : constant := 0;
   RW_Flag        : constant := 1;
   US_Flag        : constant := 2;
   PWT_Flag       : constant := 3;
   PCD_Flag       : constant := 4;
   Page_Size_Flag : constant := 7;
   PTE_PAT_Flag   : constant := 7;
   Global_Flag    : constant := 8;
   PD_PAT_Flag    : constant := 12;
   NXE_Flag       : constant := 63;

   type PAT_Entry is record
      PAT : Boolean;
      PCD : Boolean;
      PWT : Boolean;
   end record;

   --  Memory type to PAT entry mapping.
   PAT_Mapping : constant array (Caching_Type) of PAT_Entry :=
     (WB => (PAT => False, PCD => False, PWT => False),
      WT => (PAT => False, PCD => False, PWT => True),
      WC => (PAT => False, PCD => True,  PWT => False),
      UC => (PAT => False, PCD => True,  PWT => True),
      WP => (PAT => True,  PCD => False, PWT => False));

   --  Table entry address range is bits 12 .. 47.
   Address_Mask : constant Interfaces.Unsigned_64 := 16#0000fffffffff000#;

   --  Create table entry with given parameters.
   function Create_Entry
     (Address       : Interfaces.Unsigned_64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Interfaces.Unsigned_64;

   --  Create paging directory entry with given parameters.
   function Create_Dir_Entry
     (Address      : Interfaces.Unsigned_64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Caching      : Caching_Type;
      Exec_Disable : Boolean)
      return Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   function Create_Dir_Entry
     (Address      : Interfaces.Unsigned_64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Caching      : Caching_Type;
      Exec_Disable : Boolean)
      return Interfaces.Unsigned_64
   is
      PAT    : constant PAT_Entry := PAT_Mapping (Caching);
      Result : Interfaces.Unsigned_64;
   begin
      Result := Create_Entry
        (Address       => Address,
         Writable      => Writable,
         User_Access   => User_Access,
         Writethrough  => PAT.PWT,
         Cache_Disable => PAT.PCD,
         Exec_Disable  => Exec_Disable);

      if Map_Page then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => Page_Size_Flag);

         if PAT.PAT then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => PD_PAT_Flag);
         end if;

         if Global then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => Global_Flag);
         end if;
      end if;

      return Result;
   end Create_Dir_Entry;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address       : Interfaces.Unsigned_64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Interfaces.Unsigned_64
   is
      Result : Interfaces.Unsigned_64 := 0;
   begin
      Result := Address and Address_Mask;

      Result := Mutools.Utils.Bit_Set
        (Value => Result,
         Pos   => Present_Flag);

      if Writable then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => RW_Flag);
      end if;

      if User_Access then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => US_Flag);
      end if;

      if Writethrough then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => PWT_Flag);
      end if;

      if Cache_Disable then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => PCD_Flag);
      end if;

      if Exec_Disable then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => NXE_Flag);
      end if;

      return Result;
   end Create_Entry;

   -------------------------------------------------------------------------

   procedure Serialize_PD
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type)
   is
      Raw_Table : Raw_Table_Type := (others => 0);

      --  Add given table entry to raw table.
      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type);

      ----------------------------------------------------------------------

      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type)
      is
      begin
         Raw_Table (Index) := Create_Dir_Entry
           (Address      => TEntry.Get_Dst_Address,
            Writable     => TEntry.Is_Writable,
            User_Access  => TEntry.Is_Readable,
            Map_Page     => TEntry.Maps_Page,
            Global       => TEntry.Is_Global,
            Caching      => TEntry.Get_Caching,
            Exec_Disable => not TEntry.Is_Executable);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_PD;

   -------------------------------------------------------------------------

   procedure Serialize_PDPT
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type)
   is
      Raw_Table : Raw_Table_Type := (others => 0);

      --  Add given table entry to raw table.
      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type);

      ----------------------------------------------------------------------

      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type)
      is
      begin
         Raw_Table (Index) := Create_Dir_Entry
           (Address      => TEntry.Get_Dst_Address,
            Writable     => TEntry.Is_Writable,
            User_Access  => TEntry.Is_Readable,
            Map_Page     => TEntry.Maps_Page,
            Global       => TEntry.Is_Global,
            Caching      => TEntry.Get_Caching,
            Exec_Disable => not TEntry.Is_Executable);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_PDPT;

   -------------------------------------------------------------------------

   procedure Serialize_PML4
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type)
   is
      Raw_Table : Raw_Table_Type := (others => 0);

      --  Add given table entry to raw table.
      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type);

      ----------------------------------------------------------------------

      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type)
      is
         PAT : constant PAT_Entry := PAT_Mapping (TEntry.Get_Caching);
      begin
         Raw_Table (Index) := Create_Entry
           (Address       => TEntry.Get_Dst_Address,
            Writable      => TEntry.Is_Writable,
            User_Access   => TEntry.Is_Readable,
            Writethrough  => PAT.PWT,
            Cache_Disable => PAT.PCD,
            Exec_Disable  => not TEntry.Is_Executable);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_PML4;

   -------------------------------------------------------------------------

   procedure Serialize_PT
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class;
      Table  : Tables.Page_Table_Type)
   is
      Raw_Table : Raw_Table_Type := (others => 0);

      --  Add given table entry to raw table.
      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type);

      ----------------------------------------------------------------------

      procedure Add_To_Raw_Table
        (Index  : Entry_Range;
         TEntry : Entries.Table_Entry_Type)
      is
         PAT    : constant PAT_Entry := PAT_Mapping (TEntry.Get_Caching);
         Result : Interfaces.Unsigned_64;
      begin
         Result := Create_Entry
           (Address       => TEntry.Get_Dst_Address,
            Writable      => TEntry.Is_Writable,
            User_Access   => TEntry.Is_Readable,
            Writethrough  => PAT.PWT,
            Cache_Disable => PAT.PCD,
            Exec_Disable  => not TEntry.Is_Executable);

         if PAT.PAT then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => PTE_PAT_Flag);
         end if;

         if TEntry.Is_Global then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => Global_Flag);
         end if;

         Raw_Table (Index) := Result;
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_PT;

end Paging.IA32e;
