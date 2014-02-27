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
with Ada.Strings.Unbounded;

package body Map_Tests
is

   use Ahven;
   use Alloc.Map;
   use Ada.Text_IO;

   Output_File : Ada.Text_IO.File_Type;

   procedure Write_Region (R : Alloc.Map.Region_Type);

   function U (S : String) return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   procedure Allocate_Device
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region  (U ("RAM1"), True,    0,  999);
      M.Insert_Empty_Region  (U ("RAM2"), True, 1000, 1999);
      M.Insert_Device_Region (U ("DEV1"), 11000, 15000);
      M.Allocate_Fixed (U ("APP1"), 500, 799);
      M.Allocate_Fixed (U ("D1"), 11000, 15000);
      Create (Output_File, Out_File, "obj/allocate_device.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/allocate_device.txt",
                   Filename2 => "obj/allocate_device.txt"),
              Message => "Device allocation failed");

   end Allocate_Device;

   ----------------------------------------------------------------------------

   procedure Allocate_Fixed_Full_Empty_Region
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1002, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 11000, 15000);
      M.Allocate_Fixed (U ("FIXED1"), 1002, 2000);
      Create (Output_File, Out_File, "obj/alloc_fixed_full_empty_region.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/alloc_fixed_full_empty_region.txt",
                   Filename2 => "obj/alloc_fixed_full_empty_region.txt"),
              Message => "Allocation of full empty region");
   end Allocate_Fixed_Full_Empty_Region;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Invalid_Double
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1002, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 1002, 2000);
      M.Allocate_Fixed (U ("FIXED2"), 1002, 2000);
      Fail ("Double allocation undetected");
   exception
      when Alloc.Map.Invalid_Fixed_Allocation => null;
   end Allocate_Fixed_Invalid_Double;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Invalid_Exceed
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1200, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 1000, 3000);
      Fail ("Invalid fixed allocation undetected (exceeding)");
   exception
      when Alloc.Map.Invalid_Fixed_Allocation => null;
   end Allocate_Fixed_Invalid_Exceed;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Invalid_Left
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1200, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 1100, 1500);
      Fail ("Invalid fixed allocation undetected (left)");
   exception
      when Alloc.Map.Invalid_Fixed_Allocation => null;
   end Allocate_Fixed_Invalid_Left;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Invalid_Multiple
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1500, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 2500, 3000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 1700, 2800);
      Fail ("Invalid fixed allocation of multiple empty regions");
   exception
      when Alloc.Map.Invalid_Fixed_Allocation => null;
   end Allocate_Fixed_Invalid_Multiple;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Invalid_Outside_Empty
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1500, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 2500, 3000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 2200, 2300);
      Fail ("Invalid fixed allocation outside empty regions");
   exception
      when Alloc.Map.Invalid_Fixed_Allocation => null;
   end Allocate_Fixed_Invalid_Outside_Empty;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Invalid_Partial_Double
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1200, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 1250, 1800);
      M.Allocate_Fixed (U ("FIXED2"), 1300, 1700);
      Fail ("Invalid fixed double-allocation undetected");
   exception
      when Alloc.Map.Invalid_Fixed_Allocation => null;
   end Allocate_Fixed_Invalid_Partial_Double;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Invalid_Right
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1500, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 2500, 3000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 1600, 2300);
      Fail ("Invalid fixed allocation outside empty regions (right)");
   exception
      when Alloc.Map.Invalid_Fixed_Allocation => null;
   end Allocate_Fixed_Invalid_Right;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Partial_Left
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1500, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 2500, 3000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 2500, 2800);
      Create (Output_File, Out_File, "obj/alloc_fixed_partial_left.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/alloc_fixed_partial_left.txt",
                   Filename2 => "obj/alloc_fixed_partial_left.txt"),
              Message => "Partial allocation of empty region (left)");
   end Allocate_Fixed_Partial_Left;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Partial_Middle
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1002, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 11000, 15000);
      M.Allocate_Fixed (U ("FIXED"), 1500, 1800);
      Create (Output_File, Out_File, "obj/alloc_fixed_partial_middle.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/alloc_fixed_partial_middle.txt",
                   Filename2 => "obj/alloc_fixed_partial_middle.txt"),
              Message => "Partial allocation of full empty region");
   end Allocate_Fixed_Partial_Middle;

   -------------------------------------------------------------------------

   procedure Allocate_Fixed_Partial_Right
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1500, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 2500, 3000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Allocate_Fixed (U ("FIXED1"), 2800, 3000);
      Create (Output_File, Out_File, "obj/alloc_fixed_partial_right.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/alloc_fixed_partial_right.txt",
                   Filename2 => "obj/alloc_fixed_partial_right.txt"),
              Message => "Partial allocation of empty region (right)");
   end Allocate_Fixed_Partial_Right;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_Aligned
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 100,    1000);
      M.Allocate_Variable (Size => 400, Alignment => 500, Name => U ("mem1"));
      Create (Output_File, Out_File, "obj/alloc_variable_alignment.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/alloc_variable_alignment.txt",
                   Filename2 => "obj/alloc_variable_alignment.txt"),
              Message => "Variable allocation (aligned)");
   end Allocate_Variable_Aligned;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_Below_OOM
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,     499);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1500, 1999);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 2500, 2999);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Allocate_Variable
         (Size => 1000, Name => U ("mem1"), Upper_Limit => 3000);
      Fail ("Invalid constraints undetected");
   exception
      when Alloc.Map.Limit_Exceeded => null;
   end Allocate_Variable_Below_OOM;

   ----------------------------------------------------------------------------

   procedure Allocate_Variable_Exact
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1500, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 2500, 3000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Allocate_Variable (Size => 1500, Name => U ("mem1"));
      Create (Output_File, Out_File, "obj/alloc_variable_exact.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/alloc_variable_exact.txt",
                   Filename2 => "obj/alloc_variable_exact.txt"),
              Message => "Variable allocation (exact)");
   end Allocate_Variable_Exact;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_OOM
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Allocate_Variable (Size => 1500, Name => U ("mem1"));
      Fail ("Out-of-memory undetected");
   exception
      when Alloc.Map.Out_Of_Memory => null;
   end Allocate_Variable_OOM;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_OOM_Alignment
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 1,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 2001, 3000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 4001, 5000);
      M.Allocate_Variable (Size => 1000, Alignment => 123, Name => U ("mem1"));
      Fail ("Out-of-memory undetected");
   exception
      when Alloc.Map.Out_Of_Memory => null;
   end Allocate_Variable_OOM_Alignment;

   -------------------------------------------------------------------------

   procedure Allocate_Variable_OOM_Fragmentation
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 2000, 3000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 4000, 5000);
      M.Allocate_Variable (Size => 800, Name => U ("mem1"));
      M.Allocate_Variable (Size => 800, Name => U ("mem2"));
      M.Allocate_Variable (Size => 800, Name => U ("mem3"));
      M.Allocate_Variable (Size => 300, Name => U ("mem4"));
      Fail ("Out-of-memory undetected");
   exception
      when Alloc.Map.Out_Of_Memory => null;
   end Allocate_Variable_OOM_Fragmentation;

   -------------------------------------------------------------------------

   procedure Device_Regions_Not_Merged
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 1001, 2000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 2001, 3000);
      M.Insert_Device_Region (U ("DEVICE1"), 3001, 4000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 4001, 5000);
      M.Insert_Device_Region (U ("DEVICE2"), 6001, 7000);
      M.Insert_Device_Region (U ("DEVICE3"), 7001, 9000);
      Create (Output_File, Out_File, "obj/device_regions_not_merged.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/device_regions_not_merged.txt",
                   Filename2 => "obj/device_regions_not_merged.txt"),
              Message => "Device regions being merged");
   end Device_Regions_Not_Merged;

   ----------------------------------------------------------------------------

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
        (Routine => Region_Merge_Random'Access,
         Name    => "Merging consecutive regions (random)");
      T.Add_Test_Routine
        (Routine => Region_Merge_Reversed'Access,
         Name    => "Merging consecutive regions (reversed)");
      T.Add_Test_Routine
        (Routine => Region_Merge_Sorted'Access,
         Name    => "Merging consecutive regions (sorted)");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Full_Empty_Region'Access,
         Name    => "Allocation of full empty region");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Partial_Left'Access,
         Name    => "Partial fixed alloc of empty region (left)");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Partial_Right'Access,
         Name    => "Partial fixed alloc of empty region (right)");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Partial_Middle'Access,
         Name    => "Partial fixed alloc of empty region (middle)");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Invalid_Outside_Empty'Access,
         Name    => "Detect fixed allocation of memory hole");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Invalid_Left'Access,
         Name    => "Alloc partially outside empty region (left)");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Invalid_Right'Access,
         Name    => "Alloc partially outside empty region (right)");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Invalid_Exceed'Access,
         Name    => "Allocation exceeding empty region");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Invalid_Double'Access,
         Name    => "Double allocation");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Invalid_Multiple'Access,
         Name    => "Allocation spanning multiple empty regions");
      T.Add_Test_Routine
        (Routine => Allocate_Fixed_Invalid_Partial_Double'Access,
         Name    => "Allocation of already allocated memory");
      T.Add_Test_Routine
        (Routine => Allocate_Variable_Aligned'Access,
         Name    => "Variable allocation (aligned)");
      T.Add_Test_Routine
        (Routine => Allocate_Variable_Exact'Access,
         Name    => "Variable allocation (exact)");
      T.Add_Test_Routine
        (Routine => Allocate_Variable_OOM'Access,
         Name    => "Out-of-memory");
      T.Add_Test_Routine
        (Routine => Allocate_Variable_OOM_Fragmentation'Access,
         Name    => "Fragmentation");
      T.Add_Test_Routine
        (Routine => Allocate_Variable_OOM_Alignment'Access,
         Name    => "Alignment");
      T.Add_Test_Routine
        (Routine => Ordering'Access,
         Name    => "Ordering of allocated regions");
      T.Add_Test_Routine
        (Routine => Allocate_Variable_Below_OOM'Access,
         Name    => "Invalid upper limits");
      T.Add_Test_Routine
        (Routine => Allocate_Device'Access,
         Name    => "Allocate device region");
      T.Add_Test_Routine
        (Routine => Device_Regions_Not_Merged'Access,
         Name    => "Merging of device regions");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Non_Overlapping_Random
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 11000, 15000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1002, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 5000, 10000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY5"), True, 16000, 30000);
      Create (Output_File, Out_File, "obj/non_overlapping_random.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping_random.txt"),
              Message => "Memory map mismatch (random)");
   end Non_Overlapping_Random;

   -------------------------------------------------------------------------

   procedure Non_Overlapping_Reversed
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 16000, 30000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1002, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 11000, 15000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 5000, 10000);
      M.Insert_Empty_Region (U ("EMPTY5"), True, 0,    1000);
      Create (Output_File, Out_File, "obj/non_overlapping_reversed.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping_reversed.txt"),
              Message => "Memory map mismatch (reversed)");
   end Non_Overlapping_Reversed;

   -------------------------------------------------------------------------

   procedure Non_Overlapping_Sorted
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,    1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1002, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 5000, 10000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 11000, 15000);
      M.Insert_Empty_Region (U ("EMPTY5"), True, 16000, 30000);
      Create (Output_File, Out_File, "obj/non_overlapping_sorted.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/non_overlapping1.txt",
                   Filename2 => "obj/non_overlapping_sorted.txt"),
              Message => "Memory map mismatch (sorted)");
   end Non_Overlapping_Sorted;

   -------------------------------------------------------------------------

   procedure Ordering
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0, 999);

      --  Only name differs
      M.Allocate_Variable (Size => 500, Alignment => 100, Name => U ("mem1"));
      M.Allocate_Variable (Size => 500, Alignment => 100, Name => U ("mem2"));
      Create (Output_File, Out_File, "obj/ordering.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);

      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/ordering.txt",
                   Filename2 => "obj/ordering.txt"),
              Message => "Wrong ordering");
   end Ordering;

   -------------------------------------------------------------------------

   procedure Overlapping_Empty_Encompassing
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 100,  900);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 0,   1000);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Encompassing;

   -------------------------------------------------------------------------

   procedure Overlapping_Empty_Included
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,   1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 100,  900);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Included;

   -------------------------------------------------------------------------

   procedure Overlapping_Empty_Left
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 0,   1000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 900, 2000);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Left;

   -------------------------------------------------------------------------

   procedure Overlapping_Empty_Right
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 900, 2000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 0,   1000);
      Fail ("Overlap undetected");
   exception
      when Overlapping_Empty_Region => null;
   end Overlapping_Empty_Right;

   -------------------------------------------------------------------------

   procedure Region_Merge_Random
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 12001, 15000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 1000, 2000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 4000, 10000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 11000, 12000);
      M.Insert_Empty_Region (U ("EMPTY5"), True, 2001, 3000);
      Create (Output_File, Out_File, "obj/merge_random.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/merge.txt",
                   Filename2 => "obj/merge_random.txt"),
              Message => "Region merge failed (random)");
   end Region_Merge_Random;

   -------------------------------------------------------------------------

   procedure Region_Merge_Reversed
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 12001, 15000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 11000, 12000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 4000, 10000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 2001, 3000);
      M.Insert_Empty_Region (U ("EMPTY5"), True, 1000, 2000);
      Create (Output_File, Out_File, "obj/merge_reversed.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/merge.txt",
                   Filename2 => "obj/merge_reversed.txt"),
              Message => "Region merge failed (reversed)");
   end Region_Merge_Reversed;

   -------------------------------------------------------------------------

   procedure Region_Merge_Sorted
   is
      M : Map_Type;
   begin
      M.Insert_Empty_Region (U ("EMPTY1"), True, 1000, 2000);
      M.Insert_Empty_Region (U ("EMPTY2"), True, 2001, 3000);
      M.Insert_Empty_Region (U ("EMPTY3"), True, 4000, 10000);
      M.Insert_Empty_Region (U ("EMPTY4"), True, 11000, 12000);
      M.Insert_Empty_Region (U ("EMPTY5"), True, 12001, 15000);
      Create (Output_File, Out_File, "obj/merge_sorted.txt");
      M.Iterate (Write_Region'Access);
      Close (Output_File);
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/merge.txt",
                   Filename2 => "obj/merge_sorted.txt"),
              Message => "Region merge failed (sorted)");
   end Region_Merge_Sorted;

   -------------------------------------------------------------------------

   procedure Write_Region (R : Alloc.Map.Region_Type)
   is
   begin
      Put_Line
         (Output_File,
          R.Kind'Img &
          R.First_Address'Img &
          R.Last_Address'Img);
   end Write_Region;

end Map_Tests;
