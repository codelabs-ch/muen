--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Alloc.Map.Map_Type_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Alloc.Map.Map_Type_Test_Data.Map_Type_Tests is


--  begin read only
   procedure Test_Insert_Device_Region (Gnattest_T : in out Test_Map_Type);
   procedure Test_Insert_Device_Region_ec5f2d (Gnattest_T : in out Test_Map_Type) renames Test_Insert_Device_Region;
--  id:2.2/ec5f2dabfa11e2ac/Insert_Device_Region/1/0/
   procedure Test_Insert_Device_Region (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:42:4:Insert_Device_Region
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
   --  alloc-map.ads:50:4:Insert_Empty_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Insert_Empty_Region;
--  end read only


--  begin read only
   procedure Test_Allocate_Fixed (Gnattest_T : in out Test_Map_Type);
   procedure Test_Allocate_Fixed_0265c4 (Gnattest_T : in out Test_Map_Type) renames Test_Allocate_Fixed;
--  id:2.2/0265c4a1c3332ce9/Allocate_Fixed/1/0/
   procedure Test_Allocate_Fixed (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:59:4:Allocate_Fixed
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
   procedure Test_Allocate_Variable_476a70 (Gnattest_T : in out Test_Map_Type) renames Test_Allocate_Variable;
--  id:2.2/476a707e3d43dfb1/Allocate_Variable/1/0/
   procedure Test_Allocate_Variable (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:67:4:Allocate_Variable
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Allocate_Variable;
--  end read only


--  begin read only
   procedure Test_Iterate (Gnattest_T : in out Test_Map_Type);
   procedure Test_Iterate_d75863 (Gnattest_T : in out Test_Map_Type) renames Test_Iterate;
--  id:2.2/d7586358d3b827a9/Iterate/1/0/
   procedure Test_Iterate (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:79:4:Iterate
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Iterate;
--  end read only


--  begin read only
   procedure Test_Get_Region (Gnattest_T : in out Test_Map_Type);
   procedure Test_Get_Region_33413c (Gnattest_T : in out Test_Map_Type) renames Test_Get_Region;
--  id:2.2/33413c38c45924da/Get_Region/1/0/
   procedure Test_Get_Region (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:86:4:Get_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Get_Region;
--  end read only


--  begin read only
   procedure Test_Clear (Gnattest_T : in out Test_Map_Type);
   procedure Test_Clear_88711b (Gnattest_T : in out Test_Map_Type) renames Test_Clear;
--  id:2.2/88711b92a83fb6bc/Clear/1/0/
   procedure Test_Clear (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:92:4:Clear
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Clear;
--  end read only


--  begin read only
   procedure Test_Reserve (Gnattest_T : in out Test_Map_Type);
   procedure Test_Reserve_5363d1 (Gnattest_T : in out Test_Map_Type) renames Test_Reserve;
--  id:2.2/5363d11762bb0842/Reserve/1/0/
   procedure Test_Reserve (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:110:4:Reserve
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Reserve;
--  end read only


--  begin read only
   procedure Test_Insert_New_Region (Gnattest_T : in out Test_Map_Type);
   procedure Test_Insert_New_Region_c95170 (Gnattest_T : in out Test_Map_Type) renames Test_Insert_New_Region;
--  id:2.2/c9517067c2201e8a/Insert_New_Region/1/0/
   procedure Test_Insert_New_Region (Gnattest_T : in out Test_Map_Type) is
   --  alloc-map.ads:118:4:Insert_New_Region
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin

      AUnit.Assertions.Assert
        (Gnattest_Generated.Default_Assert_Value,
         "Test not implemented.");

--  begin read only
   end Test_Insert_New_Region;
--  end read only

end Alloc.Map.Map_Type_Test_Data.Map_Type_Tests;
