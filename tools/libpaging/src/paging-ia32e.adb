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
   Active_Flag    : constant := 11;
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
      Present       : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Interfaces.Unsigned_64;

   --  Create paging directory entry with given parameters.
   function Create_Dir_Entry
     (Address      : Interfaces.Unsigned_64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Present      : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Caching      : Caching_Type;
      Exec_Disable : Boolean)
      return Interfaces.Unsigned_64;

   --  Create table entry of specified level based on given raw IA-32e paging
   --  structure entry.
   function Create_Entry
     (Raw_Entry : Interfaces.Unsigned_64;
      Level     : Paging_Level)
      return Entries.Table_Entry_Type;

   -------------------------------------------------------------------------

   function Cache_Mapping (IA32E_Mem_Type : Natural) return Caching_Type
   is
   begin
      case IA32E_Mem_Type is
         when 0 => return WB;
         when 1 => return WT;
         when 2 => return WC;
         when 3 => return UC;
         when 4 => return WP;
         when others =>
            raise Constraint_Error with "Invalid IA32-e memory type:"
              & IA32E_Mem_Type'Img;
      end case;
   end Cache_Mapping;

   -------------------------------------------------------------------------

   function Create_Dir_Entry
     (Address      : Interfaces.Unsigned_64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Present      : Boolean;
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
         Present       => Present,
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
     (Raw_Entry : Interfaces.Unsigned_64;
      Level     : Paging_Level)
      return Entries.Table_Entry_Type
   is
      Dst_Addr  : constant Interfaces.Unsigned_64
        := Raw_Entry and Address_Mask;
      Maps_Page : constant Boolean := Mutools.Utils.Bit_Test
           (Value => Raw_Entry,
            Pos   => Page_Size_Flag) or Level = Paging_Level'Last;
      PAT_Bit   : constant Boolean := Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => (if Level < Paging_Level'Last
                   then PD_PAT_Flag
                   else PTE_PAT_Flag));
      PCD_Bit   : constant Boolean := Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => PCD_Flag);
      PWT_Bit   : constant Boolean := Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => PWT_Flag);
      PAT_Index : Natural := 0;
   begin

      --  For PAT index calculation see Intel SDM Vol. 3A, "4.9.2 Paging and
      --  Memory Typing When the PAT is Supported (Pentium III and More Recent
      --  Processor Families)".

      PAT_Index :=
        (if PAT_Bit and Maps_Page then 4 else 0) +
        (if PCD_Bit then 2 else 0) +
        (if PWT_Bit then 1 else 0);

      return Entries.Create
        (Dst_Index   =>
           (if Maps_Page then 0
            else Table_Range (Get_Index (Address => Dst_Addr,
                                         Level   => Level))),
         Dst_Address => Dst_Addr,
         Present     => Mutools.Utils.Bit_Test
           (Value => Raw_Entry,
            Pos   => Present_Flag),
         Readable    => Mutools.Utils.Bit_Test
           (Value => Raw_Entry,
            Pos   => US_Flag),
         Writable    => Mutools.Utils.Bit_Test
           (Value => Raw_Entry,
            Pos   => RW_Flag),
         Executable  => not Mutools.Utils.Bit_Test
           (Value => Raw_Entry,
            Pos   => NXE_Flag),
         Maps_Page   => Maps_Page,
         Global      => Mutools.Utils.Bit_Test
           (Value => Raw_Entry,
            Pos   => Global_Flag),
         Caching     => Cache_Mapping (PAT_Index));
   end Create_Entry;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address       : Interfaces.Unsigned_64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Present       : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Interfaces.Unsigned_64
   is
      Result : Interfaces.Unsigned_64 := 0;
   begin
      Result := Address and Address_Mask;

      if Present then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => Present_Flag);
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => Active_Flag);
      end if;

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

   procedure Deserialze_PD_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 3);
   end Deserialze_PD_Entry;

   -------------------------------------------------------------------------

   procedure Deserialze_PDPT_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 2);
   end Deserialze_PDPT_Entry;

   -------------------------------------------------------------------------

   procedure Deserialze_PML4_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 1);
   end Deserialze_PML4_Entry;

   -------------------------------------------------------------------------

   procedure Deserialze_PT_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 4);
   end Deserialze_PT_Entry;

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
            Present      => TEntry.Is_Present,
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
            Present      => TEntry.Is_Present,
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
            Present       => TEntry.Is_Present,
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
            Present       => TEntry.Is_Present,
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
