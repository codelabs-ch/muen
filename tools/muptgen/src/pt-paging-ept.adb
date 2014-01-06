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

package body Pt.Paging.EPT
is

   Read_Flag       : constant := 0;
   Write_Flag      : constant := 1;
   Execute_Flag    : constant := 2;
   Ignore_PAT_Flag : constant := 6;
   Present_Flag    : constant := 7;

   --  Mapping of memory type to EPT memory type bits, see Intel SDM Vol. 3C,
   --  chapter 28.2.5.
   EPT_MT_Mapping : constant array (Caching_Type) of Table_Entry_Type
     := (UC => 16#00#,
         WC => 16#08#,
         WT => 16#20#,
         WP => 16#28#,
         WB => 16#30#);

   --  Create EPT paging structure entry with specified parameters.
   generic
      type Entry_Type is new Table_Entry_Type;
   function Create_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Entry_Type;

   --  Create EPT paging structure entry that can map a page (PDPTE, PDE or
   --  PTE) with specified parameters.
   generic
      type Entry_Type is new Table_Entry_Type;
   function Create_Map_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return Entry_Type;

   -------------------------------------------------------------------------

   function Create_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return Entry_Type
   is
      New_Entry : Table_Entry_Type;
   begin
      New_Entry := Table_Entry_Type (Address) and Address_Mask;

      if Readable then
         Set_Flag (E    => New_Entry,
                   Flag => Read_Flag);
      end if;

      if Writable then
         Set_Flag (E    => New_Entry,
                   Flag => Write_Flag);
      end if;

      if Executable then
         Set_Flag (E    => New_Entry,
                   Flag => Execute_Flag);
      end if;

      return Entry_Type (New_Entry);
   end Create_Entry;

   -------------------------------------------------------------------------

   function Create_Map_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return Entry_Type
   is
      function Create is new Create_Entry (Entry_Type => Entry_Type);

      New_Entry : Entry_Type;
   begin
      New_Entry := Create (Address    => Address,
                           Readable   => Readable,
                           Writable   => Writable,
                           Executable => Executable);

      if Map_Page then
         if Ignore_PAT then
            Set_Flag (E    => Table_Entry_Type (New_Entry),
                      Flag => Ignore_PAT_Flag);
         end if;

         New_Entry := New_Entry or Entry_Type (EPT_MT_Mapping (Memory_Type));

         Set_Flag (E    => Table_Entry_Type (New_Entry),
                   Flag => Present_Flag);
      end if;

      return New_Entry;
   end Create_Map_Entry;

   -------------------------------------------------------------------------

   function Create_PD is new Create_Map_Entry (Entry_Type => PD_Entry_Type);
   function Create_PD_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return PD_Entry_Type renames Create_PD;

   -------------------------------------------------------------------------

   function Create_PDPT is new Create_Map_Entry
     (Entry_Type => PDPT_Entry_Type);
   function Create_PDPT_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return PDPT_Entry_Type renames Create_PDPT;

   -------------------------------------------------------------------------

   function Create_PML4 is new Create_Entry (Entry_Type => PML4_Entry_Type);
   function Create_PML4_Entry
     (Address    : SK.Word64;
      Readable   : Boolean;
      Writable   : Boolean;
      Executable : Boolean)
      return PML4_Entry_Type renames Create_PML4;

   -------------------------------------------------------------------------

   function Create_PT is new Create_Map_Entry (Entry_Type => PT_Entry_Type);
   function Create_PT_Entry
     (Address     : SK.Word64;
      Readable    : Boolean;
      Writable    : Boolean;
      Executable  : Boolean;
      Map_Page    : Boolean;
      Ignore_PAT  : Boolean;
      Memory_Type : Caching_Type)
      return PT_Entry_Type renames Create_PT;

end Pt.Paging.EPT;
