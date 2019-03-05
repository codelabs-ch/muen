--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Allocator.Test_Data.

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
package body Allocator.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_2892fa (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/2892fa6e04d61b6a/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  allocator.ads:30:4:Write
--  end read only

      ----------------------------------------------------------------------

      procedure Allocation_With_Devices
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/allocation_with_devices.in.xml");

         Make_Directory (Name => "obj/allocation_with_devices");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/allocation_with_devices/system.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/allocation_with_devices.ref.xml",
                  Filename2 => "obj/allocation_with_devices/system.xml"),
                 Message   => "Invalid allocation involving devices");
      end Allocation_With_Devices;

      ----------------------------------------------------------------------

      procedure Automatic_Allocation
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/automatic_allocation.in.xml");

         Make_Directory (Name => "obj/automatic_allocation");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/automatic_allocation/system.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/automatic_allocation.ref.xml",
                  Filename2 => "obj/automatic_allocation/system.xml"),
                 Message   => "Automatic allocation");
      end Automatic_Allocation;

      ----------------------------------------------------------------------

      procedure File_Backed_First
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/file_backed_first.in.xml");

         Make_Directory (Name => "obj/file_backed_first");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/file_backed_first/system.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/file_backed_first.ref.xml",
                  Filename2 => "obj/file_backed_first/system.xml"),
                 Message   => "File-backed first");
      end File_Backed_First;

      ----------------------------------------------------------------------

      procedure Fill_Pattern_Second
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/fill_pattern_second.in.xml");

         Make_Directory (Name => "obj/fill_pattern_second");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/fill_pattern_second/system.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/fill_pattern_second.ref.xml",
                  Filename2 => "obj/fill_pattern_second/system.xml"),
                 Message   => "Fill pattern second");
      end Fill_Pattern_Second;

      ----------------------------------------------------------------------

      procedure Limited_Allocation
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/limited_allocation.in.xml");

         Make_Directory (Name => "obj/limited_allocation");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/limited_allocation/system.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/limited_allocation.ref.xml",
                  Filename2 => "obj/limited_allocation/system.xml"),
                 Message   => "Limited allocation");
      end Limited_Allocation;

      ----------------------------------------------------------------------

      procedure Overlap_Between_Device_Memory
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/overlap_between_device_memory.xml");
         Make_Directory ("obj/overlap_between_device_memory");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/overlap_between_device_memory/system.xml");
         pragma Unreferenced (Policy);
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Overlapping_Physical_Memory => null;
      end Overlap_Between_Device_Memory;

      ----------------------------------------------------------------------

      procedure Overlap_Between_Devices
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/overlap_between_devices.xml");
         Make_Directory ("obj/overlap_between_devices");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/overlap_between_devices/system.xml");
         pragma Unreferenced (Policy);
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Overlapping_Physical_Memory => null;
      end Overlap_Between_Devices;

      ----------------------------------------------------------------------

      procedure Overlapping_Devices
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/overlapping_device.xml");
         Make_Directory ("obj/overlapping_devices");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/overlapping_devices/system.xml");
         pragma Unreferenced (Policy);
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Overlapping_Physical_Memory => null;
      end Overlapping_Devices;

      ----------------------------------------------------------------------

      procedure Overlapping_Physical_Memory
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_A,
                      File => "data/overlapping.xml");
         Make_Directory ("obj/overlapping_physical_memory");
         Write
           (Input_Policy => Policy,
            Output_File  => "obj/overlapping_physical_memory/system.xml");
         pragma Unreferenced (Policy);
         Assert (Condition => False,
                 Message   => "Overlap undetected");

      exception
         when Allocator.Overlapping_Physical_Memory => null;
      end Overlapping_Physical_Memory;
   begin
      Allocation_With_Devices;
      Automatic_Allocation;
      File_Backed_First;
      Fill_Pattern_Second;
      Limited_Allocation;
      Overlap_Between_Device_Memory;
      Overlap_Between_Devices;
      Overlapping_Devices;
      Overlapping_Physical_Memory;
--  begin read only
   end Test_Write;
--  end read only


--  begin read only
   procedure Test_Add_Device_Regions (Gnattest_T : in out Test);
   procedure Test_Add_Device_Regions_1b40f3 (Gnattest_T : in out Test) renames Test_Add_Device_Regions;
--  id:2.2/1b40f3907f142123/Add_Device_Regions/1/0/
   procedure Test_Add_Device_Regions (Gnattest_T : in out Test) is
   --  allocator.ads:45:4:Add_Device_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Add_Device_Regions;
--  end read only


--  begin read only
   procedure Test_Add_Empty_Regions (Gnattest_T : in out Test);
   procedure Test_Add_Empty_Regions_c56154 (Gnattest_T : in out Test) renames Test_Add_Empty_Regions;
--  id:2.2/c56154c660d702e8/Add_Empty_Regions/1/0/
   procedure Test_Add_Empty_Regions (Gnattest_T : in out Test) is
   --  allocator.ads:49:4:Add_Empty_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Add_Empty_Regions;
--  end read only


--  begin read only
   procedure Test_Add_Fixed_Regions (Gnattest_T : in out Test);
   procedure Test_Add_Fixed_Regions_768f98 (Gnattest_T : in out Test) renames Test_Add_Fixed_Regions;
--  id:2.2/768f98394e92c2f7/Add_Fixed_Regions/1/0/
   procedure Test_Add_Fixed_Regions (Gnattest_T : in out Test) is
   --  allocator.ads:53:4:Add_Fixed_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Add_Fixed_Regions;
--  end read only


--  begin read only
   procedure Test_Allocate_Variable_Regions (Gnattest_T : in out Test);
   procedure Test_Allocate_Variable_Regions_f4dacd (Gnattest_T : in out Test) renames Test_Allocate_Variable_Regions;
--  id:2.2/f4dacd693f4e9bfe/Allocate_Variable_Regions/1/0/
   procedure Test_Allocate_Variable_Regions (Gnattest_T : in out Test) is
   --  allocator.ads:57:4:Allocate_Variable_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Allocate_Variable_Regions;
--  end read only


--  begin read only
   procedure Test_Allocate_Variable_Empty_Regions (Gnattest_T : in out Test);
   procedure Test_Allocate_Variable_Empty_Regions_dfed85 (Gnattest_T : in out Test) renames Test_Allocate_Variable_Empty_Regions;
--  id:2.2/dfed855132ea0b94/Allocate_Variable_Empty_Regions/1/0/
   procedure Test_Allocate_Variable_Empty_Regions (Gnattest_T : in out Test) is
   --  allocator.ads:62:4:Allocate_Variable_Empty_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Allocate_Variable_Empty_Regions;
--  end read only


--  begin read only
   procedure Test_Allocate_Variable_File_Regions (Gnattest_T : in out Test);
   procedure Test_Allocate_Variable_File_Regions_5081f2 (Gnattest_T : in out Test) renames Test_Allocate_Variable_File_Regions;
--  id:2.2/5081f2e9909ccb18/Allocate_Variable_File_Regions/1/0/
   procedure Test_Allocate_Variable_File_Regions (Gnattest_T : in out Test) is
   --  allocator.ads:66:4:Allocate_Variable_File_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Allocate_Variable_File_Regions;
--  end read only


--  begin read only
   procedure Test_Allocate_Variable_Fill_Regions (Gnattest_T : in out Test);
   procedure Test_Allocate_Variable_Fill_Regions_080868 (Gnattest_T : in out Test) renames Test_Allocate_Variable_Fill_Regions;
--  id:2.2/08086880ccd87ba6/Allocate_Variable_Fill_Regions/1/0/
   procedure Test_Allocate_Variable_Fill_Regions (Gnattest_T : in out Test) is
   --  allocator.ads:70:4:Allocate_Variable_Fill_Regions
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Allocate_Variable_Fill_Regions;
--  end read only


--  begin read only
   procedure Test_Less_Than (Gnattest_T : in out Test);
   procedure Test_Less_Than_876b8b (Gnattest_T : in out Test) renames Test_Less_Than;
--  id:2.2/876b8b84e4a301d7/Less_Than/1/0/
   procedure Test_Less_Than (Gnattest_T : in out Test) is
   --  allocator.ads:81:4:"<"
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested in Test_Write");
--  begin read only
   end Test_Less_Than;
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
end Allocator.Test_Data.Tests;
