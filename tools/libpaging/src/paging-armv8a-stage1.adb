--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Paging.ARMv8a.Stage1
is

   type Raw_Table_Type is array (Entry_Range) of Interfaces.Unsigned_64;

   Attr_Index_Bitpos  : constant :=  2;
   User_Readable_Flag : constant :=  6;
   Not_Writable_Flag  : constant :=  7;
   Not_Global_Flag    : constant := 11;
   UXN_Flag           : constant := 54;

   --  ARMv8a Table entry address range is bits 12 .. 47.
   Address_Mask   : constant Interfaces.Unsigned_64 := 16#0000_ffff_ffff_f000#;

   --  ARMv8a Table entry MAIR index range is bits 2 .. 4.
   ARMv8a_MT_Mask : constant Interfaces.Unsigned_64 := 16#0000_0000_0000_001c#;

   --  Create page table entry with given parameters.
   function Create_Entry
     (Address        : Interfaces.Unsigned_64;
      Present        : Boolean;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      Not_Large_Page : Boolean;
      Accessed       : Boolean;
      Memory_Type    : Caching_Type)
      return Interfaces.Unsigned_64;

   --  Create table entry of specified level based on given raw ARMv8a paging
   --  structure entry.
   function Create_Entry
     (Raw_Entry : Interfaces.Unsigned_64;
      Level     : Paging_Level)
      return Entries.Table_Entry_Type;

   --  Returns the MAIR index for the given caching type.
   function MAIR_Index (Caching : Caching_Type) return Interfaces.Unsigned_64;

   -------------------------------------------------------------------------

   function Cache_Mapping (ARMv8a_Memory_Type : Natural) return Caching_Type
   is
   begin

      --  MAIR for native components using Stage1 translation:
      --  0. => 0xff : Normal memory, Outer Write-Back Non-transient, RW-Alloc
      --               Normal memory, Inner Write-Back Non-transient, RW-Alloc
      --  others => 0x00 : Device-nGnRnE memory

      case ARMv8a_Memory_Type is
         when 0      => return WB;
         when 1 .. 7 => return UC;
         when others =>
            raise Constraint_Error with "Invalid ARMv8a stage 1 memory type:"
              & ARMv8a_Memory_Type'Img;
      end case;
   end Cache_Mapping;

   -------------------------------------------------------------------------

   function Create_Entry
     (Raw_Entry : Interfaces.Unsigned_64;
      Level     : Paging_Level)
      return Entries.Table_Entry_Type
   is
      use type Interfaces.Unsigned_64;

      Dst_Addr   : constant Interfaces.Unsigned_64
        := Raw_Entry and Address_Mask;
      Present    : constant Boolean := Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => Present_Flag);
      --  Mappings are always readable from exception level >EL0.
      Readable   : constant Boolean := True;
      Writable   : constant Boolean := not Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => Not_Writable_Flag);
      Executable : constant Boolean := not Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => UXN_Flag);
      Global     : constant Boolean := not Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => Not_Global_Flag);
      Maps_Page  : constant Boolean := not Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => Not_Large_Page_Flag) or (Level = Paging_Level'Last);
      DAIR_Idx   : constant Natural
        := Natural (Interfaces.Shift_Right
                    (Value  => Raw_Entry and ARMv8a_MT_Mask,
                     Amount => Attr_Index_Bitpos));
   begin
      return Entries.Create
        (Dst_Index   =>
           (if Maps_Page then 0
            else Table_Range (Get_Index (Address => Dst_Addr,
                                         Level   => Level))),
         Dst_Address => Dst_Addr,
         Present     => Present,
         Readable    => Present and Readable,
         Writable    => Present and Writable,
         Executable  => Present and Executable,
         Maps_Page   => Present and Maps_Page,
         Global      => Global,
         Caching     => Cache_Mapping (DAIR_Idx));
   end Create_Entry;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address        : Interfaces.Unsigned_64;
      Present        : Boolean;
      Readable       : Boolean;
      Writable       : Boolean;
      Executable     : Boolean;
      Not_Large_Page : Boolean;
      Accessed       : Boolean;
      Memory_Type    : Caching_Type)
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
               Pos   => User_Readable_Flag);
         end if;

         if not Writable then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => Not_Writable_Flag);
         end if;

         if not Executable then
            Result := Mutools.Utils.Bit_Set
              (Value => Result,
               Pos   => UXN_Flag);
         end if;

         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => Present_Flag);
      end if;

      if Not_Large_Page then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => Not_Large_Page_Flag);
      end if;

      if Accessed then
         Result := Mutools.Utils.Bit_Set
           (Value => Result,
            Pos   => Accessed_Flag);

         if Memory_Type = WB then

            --  Mark write-back memory as outer shareable. However, this is only
            --  necessary for cacheable, non-device memory shared with agents in
            --  the outer domain, i.e. GPU.

            Result := Result or ARMv8_Outer_Shareable;
         end if;
      end if;

      return Result or MAIR_Index (Caching => Memory_Type);
   end Create_Entry;

   -------------------------------------------------------------------------

   procedure Deserialize_Level0_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 1);
   end Deserialize_Level0_Entry;

   -------------------------------------------------------------------------

   procedure Deserialize_Level1_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 2);
   end Deserialize_Level1_Entry;

   -------------------------------------------------------------------------

   procedure Deserialize_Level2_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 3);
   end Deserialize_Level2_Entry;

   -------------------------------------------------------------------------

   procedure Deserialize_Level3_Entry
     (Stream      : not null access Ada.Streams.Root_Stream_Type'Class;
      Table_Entry : out Entries.Table_Entry_Type)
   is
      Raw_Entry : Interfaces.Unsigned_64;
   begin
      Interfaces.Unsigned_64'Read (Stream, Raw_Entry);
      Table_Entry := Create_Entry (Raw_Entry => Raw_Entry,
                                   Level     => 4);
   end Deserialize_Level3_Entry;

   -------------------------------------------------------------------------

   function MAIR_Index (Caching : Caching_Type) return Interfaces.Unsigned_64
   is
      Result : Interfaces.Unsigned_64;
   begin

      --  MAIR for native components using Stage1 translation:
      --  0. => 0xff : Normal memory, Outer Write-Back Non-transient, RW-Alloc
      --               Normal memory, Inner Write-Back Non-transient, RW-Alloc
      --  others => 0x00 : Device-nGnRnE memory

      case Caching is
         when WB     => Result := 0;
         when UC     => Result := 1;
         when others =>
            raise Constraint_Error with "Caching type not supported by ARMv8a "
              & "stage 1: " & Caching'Img;
      end case;

      return Interfaces.Shift_Left (Value  => Result,
                                    Amount => Attr_Index_Bitpos);
   end MAIR_Index;

   -------------------------------------------------------------------------

   procedure Serialize_Level0
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
           (Address        => TEntry.Get_Dst_Address,
            Present        => TEntry.Is_Present,
            Readable       => TEntry.Is_Readable,
            Writable       => TEntry.Is_Writable,
            Executable     => TEntry.Is_Executable,
            Not_Large_Page => not TEntry.Maps_Page,
            Accessed       => TEntry.Maps_Page,
            Memory_Type    => TEntry.Get_Caching);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_Level0;

   -------------------------------------------------------------------------

   procedure Serialize_Level1
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
           (Address        => TEntry.Get_Dst_Address,
            Present        => TEntry.Is_Present,
            Readable       => TEntry.Is_Readable,
            Writable       => TEntry.Is_Writable,
            Executable     => TEntry.Is_Executable,
            Not_Large_Page => not TEntry.Maps_Page,
            Accessed       => TEntry.Maps_Page,
            Memory_Type    => TEntry.Get_Caching);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_Level1;

   -------------------------------------------------------------------------

   procedure Serialize_Level2
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
           (Address        => TEntry.Get_Dst_Address,
            Present        => TEntry.Is_Present,
            Readable       => TEntry.Is_Readable,
            Writable       => TEntry.Is_Writable,
            Executable     => TEntry.Is_Executable,
            Not_Large_Page => not TEntry.Maps_Page,
            Accessed       => TEntry.Maps_Page,
            Memory_Type    => TEntry.Get_Caching);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_Level2;

   -------------------------------------------------------------------------

   procedure Serialize_Level3
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
           (Address        => TEntry.Get_Dst_Address,
            Present        => TEntry.Is_Present,
            Readable       => TEntry.Is_Readable,
            Writable       => TEntry.Is_Writable,
            Executable     => TEntry.Is_Executable,
            Not_Large_Page => True,
            Accessed       => TEntry.Maps_Page,
            Memory_Type    => TEntry.Get_Caching);
      end Add_To_Raw_Table;
   begin
      Tables.Iterate (Table   => Table,
                      Process => Add_To_Raw_Table'Access);
      Raw_Table_Type'Write (Stream, Raw_Table);
   end Serialize_Level3;

end Paging.ARMv8a.Stage1;
