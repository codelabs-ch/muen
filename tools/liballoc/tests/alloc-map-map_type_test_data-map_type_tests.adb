--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Alloc.Map.Map_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Alloc.Map.Map_Type_Test_Data.Map_Type_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Insert_Device_Region (Gnattest_T : in out Test_Map_Type);
   procedure Test_Insert_Device_Region_ec5f2d (Gnattest_T : in out Test_Map_Type) renames Test_Insert_Device_Region;
--  id:2.2/ec5f2dabfa11e2ac/Insert_Device_Region/1/0/
   procedure Test_Insert_Device_Region (Gnattest_T : in out Test_Map_Type) is
--  end read only

      ----------------------------------------------------------------------

      procedure Allocate_Device
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("RAM1"), True,    0,  999);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("RAM2"), True, 1000, 1999);
         Gnattest_T.Fixture.Insert_Device_Region (U ("DEV1"), 11000, 15000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("APP1"), 500, 799);
         Gnattest_T.Fixture.Allocate_Fixed (U ("D1"), 11000, 15000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/allocate_device.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/allocate_device.txt",
                  Filename2 => "obj/allocate_device.txt"),
                 Message   => "Device allocation failed");
      end Allocate_Device;

      ----------------------------------------------------------------------

      procedure Device_Regions_Not_Merged
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 1001, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 2001, 3000);
         Gnattest_T.Fixture.Insert_Device_Region (U ("DEVICE1"), 3001, 4000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 4001, 5000);
         Gnattest_T.Fixture.Insert_Device_Region (U ("DEVICE2"), 6001, 7000);
         Gnattest_T.Fixture.Insert_Device_Region (U ("DEVICE3"), 7001, 9000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/device_regions_not_merged.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/device_regions_not_merged.txt",
                  Filename2 => "obj/device_regions_not_merged.txt"),
                 Message   => "Device regions being merged");
      end Device_Regions_Not_Merged;
   begin
      Allocate_Device;
      Device_Regions_Not_Merged;
--  begin read only
   end Test_Insert_Device_Region;
--  end read only


--  begin read only
   procedure Test_Insert_Empty_Region (Gnattest_T : in out Test_Map_Type);
   procedure Test_Insert_Empty_Region_67dfce (Gnattest_T : in out Test_Map_Type) renames Test_Insert_Empty_Region;
--  id:2.2/67dfce28e061906f/Insert_Empty_Region/1/0/
   procedure Test_Insert_Empty_Region (Gnattest_T : in out Test_Map_Type) is
--  end read only

      ----------------------------------------------------------------------

      procedure Non_Overlapping_Random
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 11000, 15000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1002, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 5000, 10000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY5"), True, 16000, 30000);
         Ada.Text_IO.Create
           (File => Output_File,
            Mode => Ada.Text_IO.Out_File,
            Name => "obj/non_overlapping_randoGnattest_T.Fixture.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert
           (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/non_overlapping1.txt",
               Filename2 => "obj/non_overlapping_randoGnattest_T.Fixture.txt"),
            Message   => "Memory map mismatch (random)");
      end Non_Overlapping_Random;

      ----------------------------------------------------------------------

      procedure Non_Overlapping_Reversed
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 16000, 30000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1002, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 11000, 15000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 5000, 10000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY5"), True, 0,    1000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/non_overlapping_reversed.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/non_overlapping1.txt",
                  Filename2 => "obj/non_overlapping_reversed.txt"),
                 Message   => "Memory map mismatch (reversed)");
      end Non_Overlapping_Reversed;

      ----------------------------------------------------------------------

      procedure Non_Overlapping_Sorted
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1002, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 5000, 10000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 11000, 15000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY5"), True, 16000, 30000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/non_overlapping_sorted.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/non_overlapping1.txt",
                  Filename2 => "obj/non_overlapping_sorted.txt"),
                 Message   => "Memory map mismatch (sorted)");
      end Non_Overlapping_Sorted;

      ----------------------------------------------------------------------

      procedure Overlapping_Empty_Encompassing
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 100,  900);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 0,   1000);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Overlapping_Empty_Region => Gnattest_T.Fixture.Clear;
      end Overlapping_Empty_Encompassing;

      ----------------------------------------------------------------------

      procedure Overlapping_Empty_Included
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,   1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 100,  900);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Overlapping_Empty_Region => Gnattest_T.Fixture.Clear;
      end Overlapping_Empty_Included;

      ----------------------------------------------------------------------

      procedure Overlapping_Empty_Left
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,   1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 900, 2000);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Overlapping_Empty_Region => Gnattest_T.Fixture.Clear;
      end Overlapping_Empty_Left;

      ----------------------------------------------------------------------

      procedure Overlapping_Empty_Right
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 900, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 0,   1000);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Overlapping_Empty_Region => Gnattest_T.Fixture.Clear;
      end Overlapping_Empty_Right;

      ----------------------------------------------------------------------

      procedure Region_Merge_Random
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 12001, 15000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1000, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 4000, 10000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 11000, 12000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY5"), True, 2001, 3000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/merge_randoGnattest_T.Fixture.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/merge.txt",
                  Filename2 => "obj/merge_randoGnattest_T.Fixture.txt"),
                 Message   => "Region merge failed (random)");
      end Region_Merge_Random;

      ----------------------------------------------------------------------

      procedure Region_Merge_Reversed
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 12001, 15000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 11000, 12000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 4000, 10000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 2001, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY5"), True, 1000, 2000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/merge_reversed.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/merge.txt",
                  Filename2 => "obj/merge_reversed.txt"),
                 Message   => "Region merge failed (reversed)");
      end Region_Merge_Reversed;

      ----------------------------------------------------------------------

      procedure Region_Merge_Sorted
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 1000, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 2001, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 4000, 10000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 11000, 12000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY5"), True, 12001, 15000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/merge_sorted.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/merge.txt",
                  Filename2 => "obj/merge_sorted.txt"),
                 Message   => "Region merge failed (sorted)");
      end Region_Merge_Sorted;

      use type Ada.Containers.Count_Type;
   begin
      Gnattest_T.Fixture.Insert_Empty_Region
        (U ("EMPTY"), True, 11000, 15000);
      Assert (Condition => Gnattest_T.Fixture.Data.Length = 1,
              Message   => "Length not 1");
      Gnattest_T.Fixture.Clear;

      Non_Overlapping_Random;
      Non_Overlapping_Reversed;
      Non_Overlapping_Sorted;
      Overlapping_Empty_Encompassing;
      Overlapping_Empty_Included;
      Overlapping_Empty_Left;
      Overlapping_Empty_Right;
      Region_Merge_Random;
      Region_Merge_Reversed;
      Region_Merge_Sorted;
--  begin read only
   end Test_Insert_Empty_Region;
--  end read only


--  begin read only
   procedure Test_Allocate_Fixed (Gnattest_T : in out Test_Map_Type);
   procedure Test_Allocate_Fixed_0265c4 (Gnattest_T : in out Test_Map_Type) renames Test_Allocate_Fixed;
--  id:2.2/0265c4a1c3332ce9/Allocate_Fixed/1/0/
   procedure Test_Allocate_Fixed (Gnattest_T : in out Test_Map_Type) is
--  end read only

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Full_Empty_Region
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1002, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 11000, 15000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 1002, 2000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/alloc_fixed_full_empty_region.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/alloc_fixed_full_empty_region.txt",
                  Filename2 => "obj/alloc_fixed_full_empty_region.txt"),
                 Message   => "Allocation of full empty region");
      end Allocate_Fixed_Full_Empty_Region;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Invalid_Double
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1002, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 1002, 2000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED2"), 1002, 2000);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Double allocation undetected");

      exception
         when Invalid_Fixed_Allocation => Gnattest_T.Fixture.Clear;
      end Allocate_Fixed_Invalid_Double;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Invalid_Exceed
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1200, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 1000, 3000);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Invalid fixed allocation undetected "
                 & "(exceeding)");

      exception
         when Invalid_Fixed_Allocation => Gnattest_T.Fixture.Clear;
      end Allocate_Fixed_Invalid_Exceed;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Invalid_Left
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1200, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 1100, 1500);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Invalid fixed allocation undetected (left)");

      exception
         when Invalid_Fixed_Allocation => Gnattest_T.Fixture.Clear;
      end Allocate_Fixed_Invalid_Left;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Invalid_Multiple
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1500, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 2500, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 1700, 2800);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Invalid fixed allocation of multiple empty "
                 & "regions");

      exception
         when Invalid_Fixed_Allocation => Gnattest_T.Fixture.Clear;
      end Allocate_Fixed_Invalid_Multiple;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Invalid_Outside_Empty
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1500, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 2500, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 2200, 2300);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Invalid fixed allocation outside empty "
                 & "regions");

      exception
         when Invalid_Fixed_Allocation => Gnattest_T.Fixture.Clear;
      end Allocate_Fixed_Invalid_Outside_Empty;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Invalid_Partial_Double
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1200, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 1250, 1800);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED2"), 1300, 1700);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Invalid fixed double-allocation undetected");

      exception
         when Invalid_Fixed_Allocation => Gnattest_T.Fixture.Clear;
      end Allocate_Fixed_Invalid_Partial_Double;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Invalid_Right
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1500, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 2500, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 1600, 2300);

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Invalid fixed allocation outside empty regions "
                 & "(right)");

      exception
         when Invalid_Fixed_Allocation => Gnattest_T.Fixture.Clear;
      end Allocate_Fixed_Invalid_Right;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Partial_Left
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1500, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 2500, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 2500, 2800);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/alloc_fixed_partial_left.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/alloc_fixed_partial_left.txt",
                  Filename2 => "obj/alloc_fixed_partial_left.txt"),
                 Message   => "Partial allocation of empty region (left)");
      end Allocate_Fixed_Partial_Left;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Partial_Middle
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1002, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 11000, 15000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED"), 1500, 1800);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/alloc_fixed_partial_middle.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/alloc_fixed_partial_middle.txt",
                  Filename2 => "obj/alloc_fixed_partial_middle.txt"),
                 Message   => "Partial allocation of full empty region");
      end Allocate_Fixed_Partial_Middle;

      ----------------------------------------------------------------------

      procedure Allocate_Fixed_Partial_Right
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1500, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 2500, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Fixed (U ("FIXED1"), 2800, 3000);
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/alloc_fixed_partial_right.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/alloc_fixed_partial_right.txt",
                  Filename2 => "obj/alloc_fixed_partial_right.txt"),
                 Message   => "Partial allocation of empty region (right)");
      end Allocate_Fixed_Partial_Right;
   begin
      Allocate_Fixed_Full_Empty_Region;
      Allocate_Fixed_Invalid_Double;
      Allocate_Fixed_Invalid_Exceed;
      Allocate_Fixed_Invalid_Left;
      Allocate_Fixed_Invalid_Multiple;
      Allocate_Fixed_Invalid_Outside_Empty;
      Allocate_Fixed_Invalid_Partial_Double;
      Allocate_Fixed_Invalid_Right;
      Allocate_Fixed_Partial_Left;
      Allocate_Fixed_Partial_Middle;
      Allocate_Fixed_Partial_Right;
--  begin read only
   end Test_Allocate_Fixed;
--  end read only


--  begin read only
   procedure Test_Allocate_Variable (Gnattest_T : in out Test_Map_Type);
   procedure Test_Allocate_Variable_40ab71 (Gnattest_T : in out Test_Map_Type) renames Test_Allocate_Variable;
--  id:2.2/40ab713e6fee4266/Allocate_Variable/1/0/
   procedure Test_Allocate_Variable (Gnattest_T : in out Test_Map_Type) is
--  end read only

      ----------------------------------------------------------------------

      procedure Allocate_Variable_Aligned
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 100,    1000);
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 400, Alignment => 500, Name => U ("mem1"));
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/alloc_variable_alignment.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/alloc_variable_alignment.txt",
                  Filename2 => "obj/alloc_variable_alignment.txt"),
                 Message   => "Variable allocation (aligned)");
      end Allocate_Variable_Aligned;

      ----------------------------------------------------------------------

      procedure Allocate_Variable_Exact
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 1500, 2000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 2500, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY4"), True, 5000, 10000);
         Gnattest_T.Fixture.Allocate_Variable (Size => 1500, Name => U ("mem1"));
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/alloc_variable_exact.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/alloc_variable_exact.txt",
                  Filename2 => "obj/alloc_variable_exact.txt"),
                 Message   => "Variable allocation (exact)");
      end Allocate_Variable_Exact;

      ----------------------------------------------------------------------

      procedure Allocate_Variable_OOM
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 1500, Name => U ("mem1"));

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Out-of-memory undetected");

      exception
         when Out_Of_Memory => Gnattest_T.Fixture.Clear;
      end Allocate_Variable_OOM;

      ----------------------------------------------------------------------

      procedure Allocate_Variable_OOM_Alignment
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 1,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 2001, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 4001, 5000);
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 1000, Alignment => 123, Name => U ("mem1"));

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Out-of-memory undetected");

      exception
         when Out_Of_Memory => Gnattest_T.Fixture.Clear;
      end Allocate_Variable_OOM_Alignment;

      ----------------------------------------------------------------------

      procedure Allocate_Variable_OOM_Fragmentation
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY1"), True, 0,    1000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY2"), True, 2000, 3000);
         Gnattest_T.Fixture.Insert_Empty_Region
           (U ("EMPTY3"), True, 4000, 5000);
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 800, Name => U ("mem1"));
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 800, Name => U ("mem2"));
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 800, Name => U ("mem3"));
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 300, Name => U ("mem4"));

         Gnattest_T.Fixture.Clear;
         Assert (Condition => False,
                 Message   => "Out-of-memory undetected");

      exception
         when Out_Of_Memory => Gnattest_T.Fixture.Clear;
      end Allocate_Variable_OOM_Fragmentation;

      ----------------------------------------------------------------------

      procedure Ordering
      is
      begin
         Gnattest_T.Fixture.Insert_Empty_Region (U ("EMPTY1"), True, 0, 999);

         --  Only name differs
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 500, Alignment => 100, Name => U ("mem1"));
         Gnattest_T.Fixture.Allocate_Variable
           (Size => 500, Alignment => 100, Name => U ("mem2"));
         Ada.Text_IO.Create (File => Output_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => "obj/ordering.txt");
         Gnattest_T.Fixture.Iterate (Write_Region'Access);
         Gnattest_T.Fixture.Clear;
         Ada.Text_IO.Close (File => Output_File);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/ordering.txt",
                  Filename2 => "obj/ordering.txt"),
                 Message   => "Wrong ordering");
      end Ordering;
   begin
      Allocate_Variable_Aligned;
      Allocate_Variable_Exact;
      Allocate_Variable_OOM;
      Allocate_Variable_OOM_Alignment;
      Allocate_Variable_OOM_Fragmentation;
      Ordering;
--  begin read only
   end Test_Allocate_Variable;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test_Map_Type);
   procedure Test_Iterate_d75863 (Gnattest_T : in out Test_Map_Type) renames Test_Iterate;
--  id:2.2/d7586358d3b827a9/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test_Map_Type) is
--  end read only
   begin
      Assert (Condition => True,
              Message   => "Implicitly tested");
--  begin read only
   end Test_Iterate;
--  end read only


--  begin read only
   procedure Test_Get_Region (Gnattest_T : in out Test_Map_Type);
   procedure Test_Get_Region_33413c (Gnattest_T : in out Test_Map_Type) renames Test_Get_Region;
--  id:2.2/33413c38c45924da/Get_Region/1/0/
   procedure Test_Get_Region (Gnattest_T : in out Test_Map_Type) is
--  end read only
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Interfaces.Unsigned_64;
   begin
      Gnattest_T.Fixture.Insert_Empty_Region
        (Name          => U ("foobar"),
         Allocatable   => True,
         First_Address => 1001,
         Last_Address  => 2000);

      declare
         Region : constant Region_Type := Gnattest_T.Fixture.Get_Region
           (Name => "foobar");
      begin
         Assert (Condition => Region.Name = U ("foobar"),
                 Message   => "Region name mismatch");
         Assert (Condition => Region.Kind = Empty,
                 Message   => "Region kind mismatch");
         Assert (Condition => Region.First_Address = 1001,
                 Message   => "Region first address mismatch");
         Assert (Condition => Region.Last_Address = 2000,
                 Message   => "Region last address mismatch");
         Assert (Condition => Region.Allocatable,
                 Message   => "Region not allocatable");
      end;

      begin
         declare
            Dummy : constant Region_Type := Gnattest_T.Fixture.Get_Region
              (Name => "nonexistent");
         begin
            Gnattest_T.Fixture.Clear;
            Assert (Condition => False,
                    Message   => "Exception expected");
         end;

      exception
         when No_Region => Gnattest_T.Fixture.Clear;
      end;
--  begin read only
   end Test_Get_Region;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test_Map_Type);
   procedure Test_Clear_88711b (Gnattest_T : in out Test_Map_Type) renames Test_Clear;
--  id:2.2/88711b92a83fb6bc/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test_Map_Type) is
--  end read only
      use type Ada.Containers.Count_Type;
   begin
      Gnattest_T.Fixture.Insert_Empty_Region
        (U ("EMPTY1"), True, 1001, 2000);
      Assert (Condition => Gnattest_T.Fixture.Data.Length = 1,
              Message   => "Length not 1");

      Gnattest_T.Fixture.Clear;
      Assert (Condition => Gnattest_T.Fixture.Data.Length = 0,
              Message   => "Map not cleared");
--  begin read only
   end Test_Clear;
--  end read only


--  begin read only
   procedure Test_Reserve (Gnattest_T : in out Test_Map_Type);
   procedure Test_Reserve_5363d1 (Gnattest_T : in out Test_Map_Type) renames Test_Reserve;
--  id:2.2/5363d11762bb0842/Reserve/1/0/
   procedure Test_Reserve (Gnattest_T : in out Test_Map_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Implicitly tested");
--  begin read only
   end Test_Reserve;
--  end read only


--  begin read only
   procedure Test_Insert_New_Region (Gnattest_T : in out Test_Map_Type);
   procedure Test_Insert_New_Region_c95170 (Gnattest_T : in out Test_Map_Type) renames Test_Insert_New_Region;
--  id:2.2/c9517067c2201e8a/Insert_New_Region/1/0/
   procedure Test_Insert_New_Region (Gnattest_T : in out Test_Map_Type) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Implicitly tested");
--  begin read only
   end Test_Insert_New_Region;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Alloc.Map.Map_Type_Test_Data.Map_Type_Tests;
