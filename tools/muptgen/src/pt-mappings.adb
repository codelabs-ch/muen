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

with Ada.Streams.Stream_IO;

with Mutools.Files;
with Mutools.Constants;

with Pt.Paging.EPT;

package body Pt.Mappings
is

   ----------------------------------------------------------------------

   procedure Add_Memory_Region
     (Mem_Layout       : in out Memory_Layout_Type;
      Physical_Address :        Interfaces.Unsigned_64;
      Virtual_Address  :        Interfaces.Unsigned_64;
      Size             :        Interfaces.Unsigned_64;
      Caching          :        Paging.Caching_Type;
      Writable         :        Boolean;
      Executable       :        Boolean)
   is
      use type Interfaces.Unsigned_64;
      use type Paging.PML4_Entry_Type;
      use type Paging.PDPT_Entry_Type;
      use type Paging.PD_Entry_Type;
      use type Paging.PT_Entry_Type;
      use type Paging.Table_Range;

      Is_PDPT_Page : constant Boolean := Size mod Paging.PDPT_Page_Size = 0;
      Is_PD_Page   : constant Boolean := Size mod Paging.PD_Page_Size = 0;
      Virt_End     : constant Interfaces.Unsigned_64
        := Virtual_Address + Size - 1;

      PML4_Idx_Start, PML4_Idx_End : Paging.Table_Range;
      PDPT_Idx_Start, PDPT_Idx_End : Paging.Table_Range;
      PD_Idx_Start, PD_Idx_End     : Paging.Table_Range;
      PT_Idx_Start, PT_Idx_End     : Paging.Table_Range;

      --  Physical start address of PDPT paging structure(s).
      PDPT_Addr : Interfaces.Unsigned_64;
      --  Physical start address of PD paging structure(s).
      PD_Addr   : Interfaces.Unsigned_64;
      --  Physical start address of PT paging structure(s).
      PT_Addr   : Interfaces.Unsigned_64;

      Physical_Addr : Interfaces.Unsigned_64 := Physical_Address;
   begin
      Paging.Get_Indexes (Address    => Virtual_Address,
                          PML4_Index => PML4_Idx_Start,
                          PDPT_Index => PDPT_Idx_Start,
                          PD_Index   => PD_Idx_Start,
                          PT_Index   => PT_Idx_Start);
      Paging.Get_Indexes (Address    => Virt_End,
                          PML4_Index => PML4_Idx_End,
                          PDPT_Index => PDPT_Idx_End,
                          PD_Index   => PD_Idx_End,
                          PT_Index   => PT_Idx_End);

      PDPT_Addr := Mem_Layout.PML4_Address +
        (Interfaces.Unsigned_64
           (PML4_Idx_End) + 1) * Mutools.Constants.Page_Size;
      PD_Addr   := PDPT_Addr +
        (Interfaces.Unsigned_64
           (PDPT_Idx_End) + 1) * Mutools.Constants.Page_Size;
      PT_Addr   := PD_Addr +
        (Interfaces.Unsigned_64
           (PD_Idx_End) + 1) * Mutools.Constants.Page_Size;

      for Idx in Paging.Table_Range range PML4_Idx_Start .. PML4_Idx_End
      loop
         if Mem_Layout.PML4 (Idx) = Paging.PML4_Null_Entry then
            case Mem_Layout.PT_Type is
               when IA32e =>
                  Mem_Layout.PML4 (Idx) := Paging.Create_PML4_Entry
                    (Address       => PDPT_Addr +
                       Interfaces.Unsigned_64
                         (Idx) * Mutools.Constants.Page_Size,
                     Writable      => True,
                     User_Access   => True,
                     Writethrough  => True,
                     Cache_Disable => False,
                     Exec_Disable  => False);
               when EPT =>
                  Mem_Layout.PML4 (Idx) := Paging.EPT.Create_PML4_Entry
                    (Address    => PDPT_Addr +
                       Interfaces.Unsigned_64
                         (Idx) * Mutools.Constants.Page_Size,
                     Readable   => True,
                     Writable   => True,
                     Executable => True);
            end case;
         end if;
      end loop;

      for Idx in Paging.Table_Range range PDPT_Idx_Start .. PDPT_Idx_End
      loop
         if Is_PDPT_Page then
            PD_Addr := Physical_Addr +
              Interfaces.Unsigned_64
                (Idx - PDPT_Idx_Start) * Paging.PDPT_Page_Size;
         else
            PD_Addr := PD_Addr +
              Interfaces.Unsigned_64
                (Idx - PDPT_Idx_Start) * Mutools.Constants.Page_Size;
         end if;

         if Mem_Layout.PDPT (Idx) = Paging.PDPT_Null_Entry then
            case Mem_Layout.PT_Type is
               when IA32e =>
                  Mem_Layout.PDPT (Idx) := Paging.Create_PDPT_Entry
                    (Address      => PD_Addr,
                     Writable     => not Is_PDPT_Page or Writable,
                     User_Access  => True,
                     Map_Page     => Is_PDPT_Page,
                     Global       => False,
                     Memory_Type  => Caching,
                     Exec_Disable => Is_PDPT_Page and not Executable);
               when EPT =>
                  Mem_Layout.PDPT (Idx) := Paging.EPT.Create_PDPT_Entry
                    (Address     => PD_Addr,
                     Readable    => True,
                     Writable    => not Is_PDPT_Page or Writable,
                     Executable  => not Is_PDPT_Page or Executable,
                     Map_Page    => Is_PDPT_Page,
                     Ignore_PAT  => True,
                     Memory_Type => Caching);
            end case;
         end if;
      end loop;

      if Is_PDPT_Page then
         return;
      end if;

      for Idx in Paging.Table_Range range PD_Idx_Start .. PD_Idx_End loop
         if Is_PD_Page then
            PT_Addr := Physical_Addr +
              Interfaces.Unsigned_64
                (Idx - PD_Idx_Start) * Paging.PD_Page_Size;
         else
            PT_Addr := PT_Addr +
              Interfaces.Unsigned_64
                (Idx - PD_Idx_Start) * Mutools.Constants.Page_Size;
         end if;

         if Mem_Layout.PD (Idx) = Paging.PD_Null_Entry then
            case Mem_Layout.PT_Type is
               when IA32e =>
                  Mem_Layout.PD (Idx) := Paging.Create_PD_Entry
                    (Address      => PT_Addr,
                     Writable     => not Is_PD_Page or Writable,
                     User_Access  => True,
                     Map_Page     => Is_PD_Page,
                     Global       => False,
                     Memory_Type  => Caching,
                     Exec_Disable => Is_PD_Page and not Executable);
               when EPT =>
                  Mem_Layout.PD (Idx) := Paging.EPT.Create_PD_Entry
                    (Address     => PT_Addr,
                     Readable    => True,
                     Writable    => not Is_PD_Page or Writable,
                     Executable  => not Is_PD_Page or Executable,
                     Map_Page    => Is_PD_Page,
                     Ignore_PAT  => True,
                     Memory_Type => Caching);
            end case;
         end if;
      end loop;

      if Is_PD_Page then
         return;
      end if;

      for Idx in Paging.Table_Range range PT_Idx_Start .. PT_Idx_End loop
         if Mem_Layout.PT (Idx) = Paging.PT_Null_Entry then
            case Mem_Layout.PT_Type is
               when IA32e =>
                  Mem_Layout.PT (Idx) := Paging.Create_PT_Entry
                    (Address      => Physical_Addr,
                     Writable     => Writable,
                     User_Access  => True,
                     Global       => False,
                     Memory_Type  => Caching,
                     Exec_Disable => not Executable);
               when EPT =>
                  Mem_Layout.PT (Idx) := Paging.EPT.Create_PT_Entry
                    (Address     => Physical_Addr,
                     Readable    => True,
                     Writable    => Writable,
                     Executable  => Executable,
                     Map_Page    => True,
                     Ignore_PAT  => True,
                     Memory_Type => Caching);
            end case;
         end if;

         Physical_Addr := Physical_Addr + Mutools.Constants.Page_Size;
      end loop;
   end Add_Memory_Region;

   -------------------------------------------------------------------------

   procedure Write_Pagetables
     (Mem_Layout : Memory_Layout_Type;
      Filename   : String)
   is
      use Ada.Streams.Stream_IO;

      File : File_Type;
   begin
      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Paging.PML4_Table_Type'Write (Stream (File => File), Mem_Layout.PML4);
      Paging.PDP_Table_Type'Write  (Stream (File => File), Mem_Layout.PDPT);
      Paging.PD_Table_Type'Write   (Stream (File => File), Mem_Layout.PD);
      Paging.Page_Table_Type'Write (Stream (File => File), Mem_Layout.PT);
      Close (File => File);
   end Write_Pagetables;

end Pt.Mappings;
