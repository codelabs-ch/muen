--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mergers.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mergers.Test_Data.Tests is


--  begin read only
   procedure Test_Merge_XIncludes (Gnattest_T : in out Test);
   procedure Test_Merge_XIncludes_ff65dd (Gnattest_T : in out Test) renames Test_Merge_XIncludes;
--  id:2.2/ff65dd150e825ab2/Merge_XIncludes/1/0/
   procedure Test_Merge_XIncludes (Gnattest_T : in out Test) is
   --  mergers.ads:26:4:Merge_XIncludes
--  end read only

      pragma Unreferenced (Gnattest_T);

      XML_Doc  : Muxml.XML_Data_Type;
      Filename : constant String := "obj/xinclude.xml";
   begin
      Muxml.Parse (Data => XML_Doc,
                   Kind => Muxml.None,
                   File => "data/xinclude.xml");
      Merge_XIncludes (Policy  => XML_Doc,
                       Basedir => "data");

      Muxml.Write (Data => XML_Doc,
                   Kind => Muxml.None,
                   File => Filename);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Filename,
               Filename2 => "data/xinclude_resolved.xml"),
              Message   => "Reference mismatch");

      Ada.Directories.Delete_File (Name => Filename);
--  begin read only
   end Test_Merge_XIncludes;
--  end read only


--  begin read only
   procedure Test_Merge_Hardware (Gnattest_T : in out Test);
   procedure Test_Merge_Hardware_1be979 (Gnattest_T : in out Test) renames Test_Merge_Hardware;
--  id:2.2/1be9794c0d96dee0/Merge_Hardware/1/0/
   procedure Test_Merge_Hardware (Gnattest_T : in out Test) is
   --  mergers.ads:31:4:Merge_Hardware
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_Hardware
      is
         Filename     : constant String := "obj/merged_hardware.xml";
         Ref_Filename : constant String := "data/merged_hardware.ref.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Merge_Hardware (Policy        => Policy,
                         Hardware_File => "data/hardware.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch");

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Hardware;

      ----------------------------------------------------------------------

      procedure Merge_Hardware_Null
      is
         Filename     : constant String := "obj/merged_hardware_null.xml";
         Ref_Filename : constant String := "data/merged_hardware_null.ref.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.First_Child (N => Policy.Doc),
            Child_Name => "hardware");

         Merge_Hardware (Policy        => Policy,
                         Hardware_File => "data/hardware.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch");

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Hardware_Null;
   begin
      Merge_Hardware;
      Merge_Hardware_Null;
--  begin read only
   end Test_Merge_Hardware;
--  end read only


--  begin read only
   procedure Test_Merge_Platform (Gnattest_T : in out Test);
   procedure Test_Merge_Platform_0cd3e6 (Gnattest_T : in out Test) renames Test_Merge_Platform;
--  id:2.2/0cd3e620cc856c4c/Merge_Platform/1/0/
   procedure Test_Merge_Platform (Gnattest_T : in out Test) is
   --  mergers.ads:36:4:Merge_Platform
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_Platform
      is
         Filename     : constant String := "obj/merged_platform.xml";
         Ref_Filename : constant String := "data/merged_platform.ref.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Merge_Platform (Policy        => Policy,
                         Platform_File => "data/platform.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch");

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Platform;

      ----------------------------------------------------------------------

      procedure Merge_Platform_Null
      is
         Filename     : constant String := "obj/merged_platform_null.xml";
         Ref_Filename : constant String := "data/merged_platform_null.ref.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.First_Child (N => Policy.Doc),
            Child_Name => "platform");

         Merge_Platform (Policy        => Policy,
                         Platform_File => "data/platform.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch");

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Platform_Null;
   begin
      Merge_Platform;
      Merge_Platform_Null;
--  begin read only
   end Test_Merge_Platform;
--  end read only

end Mergers.Test_Data.Tests;
