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

package body Paging.EPT
is

   type Raw_Table_Type is array (Entry_Range) of Interfaces.Unsigned_64;

   --  EPT paging structure entry layout, see Intel SDM Vol. 3C, figure 28-1.

   Read_Flag       : constant := 0;
   Write_Flag      : constant := 1;
   Execute_Flag    : constant := 2;
   Ignore_PAT_Flag : constant := 6;
   Present_Flag    : constant := 7;

   --  Mapping of memory type to EPT memory type bits, see Intel SDM Vol. 3C,
   --  chapter 28.2.5.
   EPT_MT_Mapping : constant array (Caching_Type) of Interfaces.Unsigned_64
     := (UC => 16#00#,
         WC => 16#08#,
         WT => 16#20#,
         WP => 16#28#,
         WB => 16#30#);

   --  EPT Table entry address range is bits 12 .. 47.
   Address_Mask : constant Interfaces.Unsigned_64 := 16#0000fffffffff000#;

   --  Create page table entry.
   function Create_Entry
     (Address    : Interfaces.Unsigned_64;
      Present    : Boolean;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Interfaces.Unsigned_64;

   --  Create mapping entry with given parameters.
   function Create_Map_Entry
     (Address     : Interfaces.Unsigned_64;
      Present     : Boolean;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return Interfaces.Unsigned_64;

   --  Create directory entry with given parameters.
   function Create_Dir_Entry
     (Address     : Interfaces.Unsigned_64;
      Present     : Boolean;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   function Create_Dir_Entry
     (Address     : Interfaces.Unsigned_64;
      Present     : Boolean;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;

      Result : Interfaces.Unsigned_64;
   begin
      Result := Create_Map_Entry
        (Address     => Address,
         Present     => Present,
         Readable    => Readable,
         Writable    => Writable,
         Executable  => Executable,
         Map_Page    => Map_Page,
         Ignore_PAT  => Ignore_PAT,
         Memory_Type => Memory_Type);

      if Map_Page then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => Present_Flag);
      end if;

      return Result;
   end Create_Dir_Entry;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address    : Interfaces.Unsigned_64;
      Present    : Boolean;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;

      Result : Interfaces.Unsigned_64 := 0;
   begin
      if Present then
         Result := Address and Address_Mask;
         if Readable then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => Read_Flag);
         end if;

         if Writable then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => Write_Flag);
         end if;

         if Executable then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => Execute_Flag);
         end if;
      end if;

      return Result;
   end Create_Entry;

   -------------------------------------------------------------------------

   function Create_Map_Entry
     (Address     : Interfaces.Unsigned_64;
      Present     : Boolean;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return Interfaces.Unsigned_64
   is
      use type Interfaces.Unsigned_64;

      Result : Interfaces.Unsigned_64;
   begin
      Result := Create_Entry
        (Address    => Address,
         Present    => Present,
         Readable   => Readable,
         Writable   => Writable,
         Executable => Executable);

      if Map_Page then
         if Ignore_PAT then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => Ignore_PAT_Flag);
         end if;
         Result := Result or EPT_MT_Mapping (Memory_Type);
      end if;

      return Result;
   end Create_Map_Entry;

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
           (Address     => TEntry.Get_Dst_Address,
            Present     => TEntry.Is_Present,
            Readable    => TEntry.Is_Readable,
            Writable    => TEntry.Is_Writable,
            Executable  => TEntry.Is_Executable,
            Map_Page    => TEntry.Maps_Page,
            Ignore_PAT  => True,
            Memory_Type => TEntry.Get_Caching);
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
           (Address     => TEntry.Get_Dst_Address,
            Present     => TEntry.Is_Present,
            Readable    => TEntry.Is_Readable,
            Writable    => TEntry.Is_Writable,
            Executable  => TEntry.Is_Executable,
            Map_Page    => TEntry.Maps_Page,
            Ignore_PAT  => True,
            Memory_Type => TEntry.Get_Caching);
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
      begin
         Raw_Table (Index) := Create_Entry
           (Address    => TEntry.Get_Dst_Address,
            Present    => TEntry.Is_Present,
            Readable   => TEntry.Is_Readable,
            Writable   => TEntry.Is_Writable,
            Executable => TEntry.Is_Executable);
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
      begin
         Raw_Table (Index) := Create_Map_Entry
           (Address     => TEntry.Get_Dst_Address,
            Present     => TEntry.Is_Present,
            Readable    => TEntry.Is_Readable,
            Writable    => TEntry.Is_Writable,
            Executable  => TEntry.Is_Executable,
            Map_Page    => TEntry.Maps_Page,
            Ignore_PAT  => True,
            Memory_Type => TEntry.Get_Caching);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_PT;

end Paging.EPT;
