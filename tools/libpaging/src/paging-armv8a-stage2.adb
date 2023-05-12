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

package body Paging.ARMv8a.Stage2
is

   Present_Flag        : constant :=  0;
   Not_Large_Page_Flag : constant :=  1;
   Attr_Index_Bitpos   : constant :=  2;
   Readable_Flag       : constant :=  6;
   Writable_Flag       : constant :=  7;
   Execute_Never_Flag  : constant := 54;

   --  ARMv8a Table entry address range is bits 12 .. 47.
   Address_Mask   : constant Interfaces.Unsigned_64 := 16#0000_ffff_ffff_f000#;

   --  ARMv8a stage2 Table entry memory attributes is bits 2 .. 5.
   ARMv8a_MT_Mask : constant Interfaces.Unsigned_64 := 16#0000_0000_0000_003c#;

   --  Create table entry of specified level based on given raw ARMv8a paging
   --  structure entry.
   function Create_Entry
     (Raw_Entry : Interfaces.Unsigned_64;
      Level     : Paging_Level)
      return Entries.Table_Entry_Type;

   -------------------------------------------------------------------------

   function Cache_Mapping
     (ARMv8a_Stage2_Memory_Attrs : Interfaces.Unsigned_64)
      return Caching_Type
   is
      --  Reference: ARM Arm, D8.5.4,
      --  "Stage2 memory type and Cacheability attributes"

      type Attr_Mod_Type is mod 2 ** 2;

      Outer_Attrs : constant Attr_Mod_Type
        := Attr_Mod_Type (Interfaces.Shift_Right
                          (Value  => ARMv8a_Stage2_Memory_Attrs,
                           Amount => 2));
      Inner_Attrs : constant Attr_Mod_Type
        := Attr_Mod_Type'Mod (ARMv8a_Stage2_Memory_Attrs);

      Result : Caching_Type;
   begin
      case Outer_Attrs is
         when 2#00# => return UC;     --  Device memory
         when 2#01# => Result := UC;  --  Non-cacheable
         when 2#10# => Result := WT;  --  Write-Through Cacheable
         when 2#11# => Result := WB;  --  Write-Back Cacheable
      end case;

      if Inner_Attrs = 2#00# then
         raise Constraint_Error with "Invalid ARMv8a stage 2 memory type:"
           & ARMv8a_Stage2_Memory_Attrs'Img;
      end if;

      return Result;
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
      Maps_Page  : constant Boolean := not Mutools.Utils.Bit_Test
        (Value => Raw_Entry,
         Pos   => Not_Large_Page_Flag) or (Level = Paging_Level'Last);
      Readable   : constant Boolean
        := (if Maps_Page then Mutools.Utils.Bit_Test
            (Value => Raw_Entry,
             Pos   => Readable_Flag)
            else True);
      Writable   : constant Boolean
        := (if Maps_Page then Mutools.Utils.Bit_Test
            (Value => Raw_Entry,
             Pos   => Writable_Flag)
            else True);
      Executable : constant Boolean
        := (if Maps_Page then not Mutools.Utils.Bit_Test
            (Value => Raw_Entry,
             Pos   => Execute_Never_Flag)
            else True);
      Global     : constant Boolean := False;

      Mem_Attrs  : constant Interfaces.Unsigned_64
        := Interfaces.Shift_Right
          (Value  => Raw_Entry and ARMv8a_MT_Mask,
           Amount => Attr_Index_Bitpos);
      --  We assume caching of page tables is WB. Note that ,emory type for
      --  stage2 page table walks is determined by VTCR_EL2.[SH0|ORGN0|IRGN0].
      Caching : constant Caching_Type
        := (if Maps_Page then Cache_Mapping (Mem_Attrs)
            else WB);
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
         Caching     => Caching);
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

end Paging.ARMv8a.Stage2;
