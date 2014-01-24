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
        (Routine => Non_Overlapping'Access,
         Name    => "Non-overlapping");
   end Initialize;

   ----------------------------------------------------------------------------

   procedure Non_Overlapping
   is
      use Alloc.Map;
      use Ada.Text_IO;

      M1, M2, M3  : Map_Type;
      Output_File : File_Type;

      procedure Write_Region (R : Region_Type);
      procedure Write_Region (R : Region_Type)
      is
      begin
         Put_Line
            (Output_File,
             R.Kind'Img &
             R.First_Address'Img &
             R.Last_Address'Img);
      end Write_Region;

      use Ahven;
   begin
      M1.Insert_Empty_Region (0,    1000);
      M1.Insert_Empty_Region (1001, 2000);
      M1.Insert_Empty_Region (5000, 10000);
      M1.Insert_Empty_Region (11000, 15000);
      M1.Insert_Empty_Region (16000, 30000);
      Create (Output_File, Out_File, "obj/non_overlapping1a.txt");
      M1.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping1a.txt"),
              Message => "Memory map missmatch (sorted)");

      M2.Insert_Empty_Region (11000, 15000);
      M2.Insert_Empty_Region (1001, 2000);
      M2.Insert_Empty_Region (5000, 10000);
      M2.Insert_Empty_Region (0,    1000);
      M2.Insert_Empty_Region (16000, 30000);
      Create (Output_File, Out_File, "obj/non_overlapping1b.txt");
      M2.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping1b.txt"),
              Message => "Memory map missmatch (random)");

      M3.Insert_Empty_Region (16000, 30000);
      M3.Insert_Empty_Region (11000, 15000);
      M3.Insert_Empty_Region (5000, 10000);
      M3.Insert_Empty_Region (1001, 2000);
      M3.Insert_Empty_Region (0,    1000);
      Create (Output_File, Out_File, "obj/non_overlapping1c.txt");
      M3.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping1c.txt"),
              Message => "Memory map missmatch (reversed)");

   end Non_Overlapping;

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

end Map_Tests;
