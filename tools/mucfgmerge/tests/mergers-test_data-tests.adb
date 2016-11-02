--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mergers.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Mergers.Test_Data.Tests is


--  begin read only
   procedure Test_Merge_XIncludes (Gnattest_T : in out Test);
   procedure Test_Merge_XIncludes_61f398 (Gnattest_T : in out Test) renames Test_Merge_XIncludes;
--  id:2.2/61f3981e93dd3f4f/Merge_XIncludes/1/0/
   procedure Test_Merge_XIncludes (Gnattest_T : in out Test) is
   --  mergers.ads:28:4:Merge_XIncludes
--  end read only

      pragma Unreferenced (Gnattest_T);

      function U
        (Source : String)
         return Ada.Strings.Unbounded.Unbounded_String
         renames Ada.Strings.Unbounded.To_Unbounded_String;

      ----------------------------------------------------------------------

      procedure Multiple_Inc_Dirs_Precedence
      is
         Inc_1    : Muxml.XML_Data_Type;
         XML_Doc  : Muxml.XML_Data_Type;
         Filename : constant String := "obj/xinclude.xml";
      begin
         Muxml.Parse (Data => Inc_1,
                      Kind => Muxml.None,
                      File => "data/xinclude_1.xml");
         Muxml.Utils.Set_Attribute
           (Doc   => Inc_1.Doc,
            XPath => "/hardware/include[@href='xinclude_2.xml']",
            Name  => "href",
            Value => "xinclude_4.xml");
         Muxml.Write (Data => Inc_1,
                      Kind => Muxml.None,
                      File => "obj/xinclude_1.xml");

         Muxml.Parse (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => "data/xinclude.xml");

         --  Check that the obj/xinclude_1.xml file takes precedence over
         --  data/xinclude_1.xml.

         Merge_XIncludes (Policy       => XML_Doc,
                          Include_Dirs =>
                            (1 => U ("obj"),
                             2 => U ("data"),
                             3 => U ("./..")));

         Muxml.Write (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => Filename);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => "data/xinclude_incdir.xml"),
                 Message   => "Reference mismatch (incdir): " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
         Ada.Directories.Delete_File (Name => "obj/xinclude_1.xml");
      end Multiple_Inc_Dirs_Precedence;

      ----------------------------------------------------------------------

      procedure Positive_Test
      is
         XML_Doc  : Muxml.XML_Data_Type;
         Filename : constant String := "obj/xinclude.xml";
      begin
         Muxml.Parse (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => "data/xinclude.xml");
         Merge_XIncludes (Policy       => XML_Doc,
                          Include_Dirs => (1 => U ("data")));

         Muxml.Write (Data => XML_Doc,
                      Kind => Muxml.None,
                      File => Filename);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => "data/xinclude_resolved.xml"),
                 Message   => "Reference mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Positive_Test;
   begin
      Multiple_Inc_Dirs_Precedence;
      Positive_Test;
--  begin read only
   end Test_Merge_XIncludes;
--  end read only


--  begin read only
   procedure Test_Merge_Hardware (Gnattest_T : in out Test);
   procedure Test_Merge_Hardware_1be979 (Gnattest_T : in out Test) renames Test_Merge_Hardware;
--  id:2.2/1be9794c0d96dee0/Merge_Hardware/1/0/
   procedure Test_Merge_Hardware (Gnattest_T : in out Test) is
   --  mergers.ads:33:4:Merge_Hardware
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_Hardware
      is
         Filename     : constant String := "obj/merged_hardware.xml";
         Ref_Filename : constant String := "data/merged_hardware.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Merge_Hardware (Policy        => Policy,
                         Hardware_File => "data/hardware.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Hardware;

      ----------------------------------------------------------------------

      procedure Merge_Hardware_Null
      is
         Filename     : constant String := "obj/merged_hardware_null.xml";
         Ref_Filename : constant String := "data/merged_hardware_null.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.First_Child (N => Policy.Doc),
            Child_Name => "hardware");

         Merge_Hardware (Policy        => Policy,
                         Hardware_File => "data/hardware.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

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
   --  mergers.ads:38:4:Merge_Platform
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_Platform
      is
         Filename     : constant String := "obj/merged_platform.xml";
         Ref_Filename : constant String := "data/merged_platform.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Merge_Platform (Policy        => Policy,
                         Platform_File => "data/platform.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Platform;

      ----------------------------------------------------------------------

      procedure Merge_Platform_Null
      is
         Filename     : constant String := "obj/merged_platform_null.xml";
         Ref_Filename : constant String := "data/merged_platform_null.xml";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => "data/test_policy.xml");
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Nodes.First_Child (N => Policy.Doc),
            Child_Name => "platform");

         Merge_Platform (Policy        => Policy,
                         Platform_File => "data/platform.xml");
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Filename);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Filename,
                  Filename2 => Ref_Filename),
                 Message   => "Policy mismatch: " & Filename);

         Ada.Directories.Delete_File (Name => Filename);
      end Merge_Platform_Null;
   begin
      Merge_Platform;
      Merge_Platform_Null;
--  begin read only
   end Test_Merge_Platform;
--  end read only

end Mergers.Test_Data.Tests;
