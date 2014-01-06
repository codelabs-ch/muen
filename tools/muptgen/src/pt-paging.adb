--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Pt.Paging
is

   type PAT_Entry is record
      PAT : Boolean;
      PCD : Boolean;
      PWT : Boolean;
   end record;

   --  Memory type to PAT entry mapping.
   PAT_Mapping : constant array (Caching_Type) of PAT_Entry :=
     (UC => (PAT => False, PCD => False, PWT => False),
      WC => (PAT => False, PCD => False, PWT => True),
      WT => (PAT => False, PCD => True,  PWT => False),
      WP => (PAT => False, PCD => True,  PWT => True),
      WB => (PAT => True,  PCD => False, PWT => False));

   --  PDPTE address range is bits 30 .. 47; entry maps a 1 GB page.
   PDPT_Address_Mask : constant PDPT_Entry_Type := 16#0000ffffc0000000#;

   --  PDE address range is bits 21 .. 47; entry maps a 2 MB page.
   PD_Address_Mask : constant PD_Entry_Type := 16#0000ffffffe00000#;

   --  PML4 index is bits 39 .. 47 of the linear address.
   PML4_Index_Mask : constant SK.Word64 := 16#0000ff8000000000#;

   --  PDPT index is bits 30 .. 38 of the linear address.
   PDPT_Index_Mask : constant SK.Word64 := 16#0000007fc0000000#;

   --  PD index is bits 21 .. 29 of the linear address.
   PD_Index_Mask : constant SK.Word64 := 16#000000003fe00000#;

   --  PT index is bits 12 .. 20 of the linear address.
   PT_Index_Mask : constant SK.Word64 := 16#00000000001ff000#;

   --  Create paging structure entry with specified parameters.
   generic
      type Entry_Type is new Table_Entry_Type;
   function Create_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Entry_Type;

   --  Create paging directory entry (PDPT or PD) with specified parameters.
   generic
      type Entry_Type is new Directory_Entry_Type;
   function Create_Directory_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Memory_Type  : Caching_Type;
      Exec_Disable : Boolean)
      return Entry_Type;

   --  Returns the physical address the table entry is pointing to.
   function Get_Address (E : Table_Entry_Type) return SK.Word64;

   -------------------------------------------------------------------------

   function Create_Directory_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Memory_Type  : Caching_Type;
      Exec_Disable : Boolean)
      return Entry_Type
   is
      function Create_DE is new Create_Entry
        (Entry_Type => Directory_Entry_Type);

      DE : Table_Entry_Type;
   begin
      DE := Create_DE (Address       => Address,
                       Writable      => Writable,
                       User_Access   => User_Access,
                       Writethrough  => PAT_Mapping (Memory_Type).PWT,
                       Cache_Disable => PAT_Mapping (Memory_Type).PCD,
                       Exec_Disable  => Exec_Disable);

      if Map_Page then
         Set_Flag (E    => DE,
                   Flag => Page_Size_Flag);

         if PAT_Mapping (Memory_Type).PAT then
            Set_Flag (E    => DE,
                      Flag => PD_PAT_Flag);
         end if;

         if Global then
            Set_Flag (E    => DE,
                      Flag => Global_Flag);
         end if;
      end if;

      return Entry_Type (DE);
   end Create_Directory_Entry;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return Entry_Type
   is
      New_Entry : Table_Entry_Type;
   begin
      New_Entry := Table_Entry_Type (Address) and Address_Mask;

      Set_Flag (E    => New_Entry,
                Flag => Present_Flag);

      if Writable then
         Set_Flag (E    => New_Entry,
                   Flag => RW_Flag);
      end if;

      if User_Access then
         Set_Flag (E    => New_Entry,
                   Flag => US_Flag);
      end if;

      if Writethrough then
         Set_Flag (E    => New_Entry,
                   Flag => PWT_Flag);
      end if;

      if Cache_Disable then
         Set_Flag (E    => New_Entry,
                   Flag => PCD_Flag);
      end if;

      if Exec_Disable then
         Set_Flag (E    => New_Entry,
                   Flag => NXE_Flag);
      end if;

      return Entry_Type (New_Entry);
   end Create_Entry;

   -------------------------------------------------------------------------

   function Create_PD is new Create_Directory_Entry
     (Entry_Type => PD_Entry_Type);
   function Create_PD_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Memory_Type  : Caching_Type;
      Exec_Disable : Boolean)
      return PD_Entry_Type renames Create_PD;

   -------------------------------------------------------------------------

   function Create_PDPT is new Create_Directory_Entry
     (Entry_Type => PDPT_Entry_Type);
   function Create_PDPT_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Map_Page     : Boolean;
      Global       : Boolean;
      Memory_Type  : Caching_Type;
      Exec_Disable : Boolean)
      return PDPT_Entry_Type renames Create_PDPT;

   -------------------------------------------------------------------------

   function Create_PML4 is new Create_Entry (Entry_Type => PML4_Entry_Type);
   function Create_PML4_Entry
     (Address       : SK.Word64;
      Writable      : Boolean;
      User_Access   : Boolean;
      Writethrough  : Boolean;
      Cache_Disable : Boolean;
      Exec_Disable  : Boolean)
      return PML4_Entry_Type renames Create_PML4;

   -------------------------------------------------------------------------

   function Create_PT_Entry
     (Address      : SK.Word64;
      Writable     : Boolean;
      User_Access  : Boolean;
      Global       : Boolean;
      Memory_Type  : Caching_Type;
      Exec_Disable : Boolean)
      return PT_Entry_Type
   is
      function Create_PT is new Create_Entry (Entry_Type => PT_Entry_Type);

      PTE : PT_Entry_Type;
   begin
      PTE := Create_PT (Address       => Address,
                        Writable      => Writable,
                        User_Access   => User_Access,
                        Writethrough  => PAT_Mapping (Memory_Type).PWT,
                        Cache_Disable => PAT_Mapping (Memory_Type).PCD,
                        Exec_Disable  => Exec_Disable);

      if PAT_Mapping (Memory_Type).PAT then
         Set_Flag (E    => Table_Entry_Type (PTE),
                   Flag => PTE_PAT_Flag);
      end if;

      if Global then
         Set_Flag (E    => Table_Entry_Type (PTE),
                   Flag => Global_Flag);
      end if;

      return PTE;
   end Create_PT_Entry;

   -------------------------------------------------------------------------

   function Get_Address (E : Table_Entry_Type) return SK.Word64
   is
   begin
      return SK.Word64 (E and Address_Mask);
   end Get_Address;

   -------------------------------------------------------------------------

   function Get_Address (E : PT_Entry_Type) return SK.Word64
   is
   begin
      return Get_Address (E => Table_Entry_Type (E));
   end Get_Address;

   -------------------------------------------------------------------------

   procedure Get_Indexes
     (Address    :     SK.Word64;
      PML4_Index : out Table_Range;
      PDPT_Index : out Table_Range;
      PD_Index   : out Table_Range;
      PT_Index   : out Table_Range)
   is
      use type SK.Word64;
   begin
      PML4_Index := Table_Range ((Address and PML4_Index_Mask) / 2 ** 39);
      PDPT_Index := Table_Range ((Address and PDPT_Index_Mask) / 2 ** 30);
      PD_Index   := Table_Range ((Address and PD_Index_Mask) / 2 ** 21);
      PT_Index   := Table_Range ((Address and PT_Index_Mask) / 2 ** 12);
   end Get_Indexes;

   -------------------------------------------------------------------------

   function Get_PD_Address (E : PDPT_Entry_Type) return SK.Word64
   is
      Address : SK.Word64;
   begin
      if SK.Bit_Test
        (Value => SK.Word64 (E),
         Pos   => Page_Size_Flag)
      then
         Address := SK.Word64 (E and PDPT_Address_Mask);
      else
         Address := SK.Word64 (Table_Entry_Type (E) and Address_Mask);
      end if;

      return Address;
   end Get_PD_Address;

   -------------------------------------------------------------------------

   function Get_PDPT_Address (E : PML4_Entry_Type) return SK.Word64
   is
   begin
      return Get_Address (E => Table_Entry_Type (E));
   end Get_PDPT_Address;

   -------------------------------------------------------------------------

   function Get_PT_Address (E : PD_Entry_Type) return SK.Word64
   is
      Address : SK.Word64;
   begin
      if SK.Bit_Test
        (Value => SK.Word64 (E),
         Pos   => Page_Size_Flag)
      then
         Address := SK.Word64 (E and PD_Address_Mask);
      else
         Address := SK.Word64 (Table_Entry_Type (E) and Address_Mask);
      end if;

      return Address;
   end Get_PT_Address;

   -------------------------------------------------------------------------

   procedure Set_Flag
     (E    : in out Table_Entry_Type;
      Flag :        SK.Word64_Pos)
   is
   begin
      E := Table_Entry_Type
        (SK.Bit_Set (Value => SK.Word64 (E),
                     Pos   => Flag));
   end Set_Flag;

end Pt.Paging;
