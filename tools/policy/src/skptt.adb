--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;

with SK.Utils;

with Skp.Paging;

procedure Skptt
is

   use Ada.Streams.Stream_IO;
   use type SK.Word64;

   type Table_Type is array (Skp.Paging.Table_Range) of SK.Word64;

   File                               : File_Type;
   Linear_Address, Frame_Address      : SK.Word64;
   PML4, PDPT, PD, PT                 : Table_Type;
   PML4_Idx, PDPT_Idx, PD_Idx, PT_Idx : Skp.Paging.Table_Range;

   --  Perform a page table walk for the calculated page table indexes. Raises
   --  a page fault exception on failure.
   procedure Page_Walk;

   --  Print entries of given page table.
   procedure Print_Table
     (Table : Table_Type;
      Name  : String);

   --  Print tool usage.
   procedure Print_Usage;

   --  Raise a page fault exception.
   procedure Raise_Exception;

   -------------------------------------------------------------------------

   Page_Fault : exception;

   procedure Page_Walk
   is
   begin
      if PML4 (PML4_Idx) = 0 then
         Raise_Exception;
      end if;

      if PDPT (PDPT_Idx) = 0 then
         Raise_Exception;
      end if;

      if SK.Bit_Test
        (Value => PDPT (PDPT_Idx),
         Pos   => Skp.Paging.Page_Size_Flag)
      then

         --  1 GB page.

         Frame_Address := Skp.Paging.Get_PD_Address
           (E => Skp.Paging.PDPT_Entry_Type
              (PDPT (PDPT_Idx)));
         return;
      end if;

      if PD (PD_Idx) = 0 then
         Raise_Exception;
      end if;

      if SK.Bit_Test
        (Value => PD (PD_Idx),
         Pos   => Skp.Paging.Page_Size_Flag)
      then

         --  2 M page.

         Frame_Address := Skp.Paging.Get_PD_Address
           (E => Skp.Paging.PDPT_Entry_Type
              (PDPT (PDPT_Idx)));
         return;
      end if;

      if PT (PT_Idx) = 0 then
         Raise_Exception;
      end if;

      if SK.Bit_Test
        (Value => PT (PT_Idx),
         Pos   => Skp.Paging.Page_Size_Flag)
      then

         --  4k page.

         Frame_Address := Skp.Paging.Get_Address
           (E => Skp.Paging.PT_Entry_Type
              (PT (PT_Idx)));
         return;
      end if;

      Raise_Exception;
   end Page_Walk;

   -------------------------------------------------------------------------

   procedure Print_Table
     (Table : Table_Type;
      Name  : String)
   is
   begin
      Ada.Text_IO.Put_Line (Name);
      for J in Table'Range loop
         if Table (J) /= 0 then
            Ada.Text_IO.Put (J'Img);
            Ada.Text_IO.Set_Col (To => 6);
            Ada.Text_IO.Put ("=> ");
            Ada.Text_IO.Put (SK.Utils.To_Hex (Item => Table (J)));
            if SK.Bit_Test (Value => Table (J),
                            Pos   => Skp.Paging.Page_Size_Flag)
            then
               Ada.Text_IO.Put (" PS");
            end if;
            Ada.Text_IO.New_Line;
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Table;

   -------------------------------------------------------------------------

   procedure Print_Usage
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name & " <file> <address>");
      Ada.Text_IO.Put_Line ("  <file>      Page table file");
      Ada.Text_IO.Put_Line ("  <address>   Linear address in hex");
   end Print_Usage;

   -------------------------------------------------------------------------

   procedure Raise_Exception
   is
   begin
      raise Page_Fault with "Access to " & SK.Utils.To_Hex
        (Item => Linear_Address) & " leads to a page fault";
   end Raise_Exception;

begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Print_Usage;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Open (File => File,
         Mode => In_File,
         Name => Ada.Command_Line.Argument (1));
   Table_Type'Read (Stream (File => File), PML4);
   Table_Type'Read (Stream (File => File), PDPT);
   Table_Type'Read (Stream (File => File), PD);
   Table_Type'Read (Stream (File => File), PT);
   Close (File => File);

   Print_Table (Table => PML4,
                Name  => "PML4");
   Print_Table (Table => PDPT,
                Name  => "PDPT");
   Print_Table (Table => PD,
                Name  => "PD");
   Print_Table (Table => PT,
                Name  => "PT");

   Linear_Address := SK.Word64'Value
     ("16#" & Ada.Command_Line.Argument (2) & "#");
   Skp.Paging.Get_Indexes
     (Address    => Linear_Address,
      PML4_Index => PML4_Idx,
      PDPT_Index => PDPT_Idx,
      PD_Index   => PD_Idx,
      PT_Index   => PT_Idx);
   Ada.Text_IO.Put_Line ("PML4_Idx" & PML4_Idx'Img);
   Ada.Text_IO.Put_Line ("PDPT_Idx" & PDPT_Idx'Img);
   Ada.Text_IO.Put_Line ("PD_Idx  " & PD_Idx'Img);
   Ada.Text_IO.Put_Line ("PT_Idx  " & PT_Idx'Img);

   Page_Walk;

   Ada.Text_IO.Put ("Page frame address: " & SK.Utils.To_Hex
                    (Item => Frame_Address));
end Skptt;
