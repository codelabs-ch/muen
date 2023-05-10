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

   Present_Flag        : constant :=  0;
   Not_Large_Page_Flag : constant :=  1;
   Not_Writable_Flag   : constant :=  7;
   Not_Global_Flag     : constant := 11;
   UXN_Flag            : constant := 54;

   --  ARMv8a Table entry address range is bits 12 .. 47.
   Address_Mask   : constant Interfaces.Unsigned_64 := 16#0000_ffff_ffff_f000#;

   --  ARMv8a Table entry MAIR index range is bits 2 .. 4.
   ARMv8a_MT_Mask : constant Interfaces.Unsigned_64 := 16#0000_0000_0000_001c#;

   --  Create table entry of specified level based on given raw ARMv8a paging
   --  structure entry.
   function Create_Entry
     (Raw_Entry : Interfaces.Unsigned_64;
      Level     : Paging_Level)
      return Entries.Table_Entry_Type;

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
                     Amount => 2));
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

end Paging.ARMv8a.Stage1;
