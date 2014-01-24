--
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with Test_Utils;
with Alloc.Map;
with Ada.Text_IO;

package body Map_Tests
is
   Output_File : Ada.Text_IO.File_Type;

   procedure Write_Region (R : Alloc.Map.Region_Type);

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Memory map tests");
      T.Add_Test_Routine
        (Routine => Overlapping_Empty_Left'Access,
         Name    => "Overlapping, left side");
      T.Add_Test_Routine
        (Routine => Overlapping_Empty_Right'Access,
         Name    => "Overlapping, right side");
      T.Add_Test_Routine
        (Routine => Overlapping_Empty_Included'Access,
         Name    => "Overlapping, included");
      T.Add_Test_Routine
        (Routine => Overlapping_Empty_Encompassing'Access,
         Name    => "Overlapping, encompassing");
      T.Add_Test_Routine
        (Routine => Non_Overlapping_Sorted'Access,
         Name    => "Non-overlapping (sorted)");
      T.Add_Test_Routine
        (Routine => Non_Overlapping_Random'Access,
         Name    => "Non-overlapping (random)");
      T.Add_Test_Routine
        (Routine => Non_Overlapping_Reversed'Access,
         Name    => "Non-overlapping (reversed)");
      T.Add_Test_Routine
        (Routine => Region_Merge'Access,
         Name    => "Merging consecutive regions");
   end Initialize;

   ----------------------------------------------------------------------------

   procedure Non_Overlapping_Random
   is
      use Ahven;
      use Alloc.Map;
      use Ada.Text_IO;

      M : Map_Type;
   begin
      M.Insert_Empty_Region (11000, 15000);
      M.Insert_Empty_Region (1001, 2000);
      M.Insert_Empty_Region (5000, 10000);
      M.Insert_Empty_Region (0,    1000);
      M.Insert_Empty_Region (16000, 30000);
      Create (Output_File, Out_File, "obj/non_overlapping1b.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping1b.txt"),
              Message => "Memory map missmatch (random)");
   end Non_Overlapping_Random;

   ----------------------------------------------------------------------------

   procedure Non_Overlapping_Reversed
   is
      use Ahven;
      use Alloc.Map;
      use Ada.Text_IO;

      M : Map_Type;
   begin
      M.Insert_Empty_Region (16000, 30000);
      M.Insert_Empty_Region (11000, 15000);
      M.Insert_Empty_Region (5000, 10000);
      M.Insert_Empty_Region (1001, 2000);
      M.Insert_Empty_Region (0,    1000);
      Create (Output_File, Out_File, "obj/non_overlapping1c.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping1c.txt"),
              Message => "Memory map missmatch (reversed)");
   end Non_Overlapping_Reversed;

   ----------------------------------------------------------------------------

   procedure Non_Overlapping_Sorted
   is
      use Ahven;
      use Alloc.Map;
      use Ada.Text_IO;

      M : Map_Type;
   begin
      M.Insert_Empty_Region (0,    1000);
      M.Insert_Empty_Region (1001, 2000);
      M.Insert_Empty_Region (5000, 10000);
      M.Insert_Empty_Region (11000, 15000);
      M.Insert_Empty_Region (16000, 30000);
      Create (Output_File, Out_File, "obj/non_overlapping1a.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping1a.txt"),
              Message => "Memory map missmatch (sorted)");
   end Non_Overlapping_Sorted;

   ----------------------------------------------------------------------------

   procedure Overlapping_Empty_Encompassing
   is
      use Ahven;
      use Alloc.Map;
      M : Map_Type;
   begin
      M.Insert_Empty_Region (100,  900);
      M.Insert_Empty_Region (0,   1000);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Encompassing;

   ----------------------------------------------------------------------------

   procedure Overlapping_Empty_Included
   is
      use Ahven;
      use Alloc.Map;
      M : Map_Type;
   begin
      M.Insert_Empty_Region (0,   1000);
      M.Insert_Empty_Region (100,  900);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Included;

   ----------------------------------------------------------------------------

   procedure Overlapping_Empty_Left
   is
      use Ahven;
      use Alloc.Map;
      M : Map_Type;
   begin
      M.Insert_Empty_Region (0,   1000);
      M.Insert_Empty_Region (900, 2000);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Left;

   ----------------------------------------------------------------------------

   procedure Overlapping_Empty_Right
   is
      use Ahven;
      use Alloc.Map;
      M : Map_Type;
   begin
      M.Insert_Empty_Region (900, 2000);
      M.Insert_Empty_Region (0,   1000);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Right;

   ----------------------------------------------------------------------------

   procedure Region_Merge
   is
      use Ahven;
      use Alloc.Map;
      use Ada.Text_IO;

      M : Map_Type;
   begin
      M.Insert_Empty_Region (1000, 2000);
      M.Insert_Empty_Region (2001, 3000);
      M.Insert_Empty_Region (4000, 10000);
      M.Insert_Empty_Region (11000, 12000);
      M.Insert_Empty_Region (12001, 15000);
      Create (Output_File, Out_File, "obj/merge.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/merge.txt",
                   Filename2 => "obj/merge.txt"),
              Message => "Region merge failed");
   end Region_Merge;

   ----------------------------------------------------------------------------

   procedure Write_Region (R : Alloc.Map.Region_Type)
   is
      use Ada.Text_IO;
   begin
      Put_Line
         (Output_File,
          R.Kind'Img &
          R.First_Address'Img &
          R.Last_Address'Img);
   end Write_Region;

end Map_Tests;
