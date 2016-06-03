--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Generators.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Cspec.Generators.Test_Data.Tests is


--  begin read only
   procedure Test_Get_Memory_Str (Gnattest_T : in out Test);
   procedure Test_Get_Memory_Str_dfbc76 (Gnattest_T : in out Test) renames Test_Get_Memory_Str;
--  id:2.2/dfbc76f006722165/Get_Memory_Str/1/0/
   procedure Test_Get_Memory_Str (Gnattest_T : in out Test) is
   --  cspec-generators.ads:27:4:Get_Memory_Str
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn     : constant String := "get_memory_str";
      Tmpl   : Mutools.Templates.Template_Type;
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Memory_Str
           (Policy    => Policy,
            Comp_Name => "vt"));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Assert (Condition => Get_Memory_Str
              (Policy    => Policy,
               Comp_Name => "no_res") = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Memory_Str;
--  end read only


--  begin read only
   procedure Test_Get_Channels_Str (Gnattest_T : in out Test);
   procedure Test_Get_Channels_Str_0f5480 (Gnattest_T : in out Test) renames Test_Get_Channels_Str;
--  id:2.2/0f54803daf364ee5/Get_Channels_Str/1/0/
   procedure Test_Get_Channels_Str (Gnattest_T : in out Test) is
   --  cspec-generators.ads:35:4:Get_Channels_Str
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn     : constant String := "get_channels_str";
      Tmpl   : Mutools.Templates.Template_Type;
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Channels_Str
           (Policy    => Policy,
            Comp_Name => "vt"));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Assert (Condition => Get_Channels_Str
              (Policy    => Policy,
               Comp_Name => "no_res") = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Channels_Str;
--  end read only


--  begin read only
   procedure Test_Get_Devices_Str (Gnattest_T : in out Test);
   procedure Test_Get_Devices_Str_6a2416 (Gnattest_T : in out Test) renames Test_Get_Devices_Str;
--  id:2.2/6a24161bb72c0677/Get_Devices_Str/1/0/
   procedure Test_Get_Devices_Str (Gnattest_T : in out Test) is
   --  cspec-generators.ads:43:4:Get_Devices_Str
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn     : constant String := "get_devices_str";
      Tmpl   : Mutools.Templates.Template_Type;
      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Devices_Str
           (Policy    => Policy,
            Comp_Name => "vt"));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Assert (Condition => Get_Devices_Str
              (Policy    => Policy,
               Comp_Name => "no_res") = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Devices_Str;
--  end read only

end Cspec.Generators.Test_Data.Tests;