--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cspec.Generators.Test_Data.

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
package body Cspec.Generators.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Get_Memory_Str (Gnattest_T : in out Test);
   procedure Test_Get_Memory_Str_d1878c (Gnattest_T : in out Test) renames Test_Get_Memory_Str;
--  id:2.2/d1878c1dbaaa398b/Get_Memory_Str/1/0/
   procedure Test_Get_Memory_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn   : constant String := "get_memory_str";
      Tmpl : Mutools.Templates.Template_Type;
      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Memory_Str (Spec => Spec));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_nores.xml");
      Assert (Condition => Get_Memory_Str (Spec => Spec) = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Memory_Str;
--  end read only


--  begin read only
   procedure Test_Get_Channels_Str (Gnattest_T : in out Test);
   procedure Test_Get_Channels_Str_d74b67 (Gnattest_T : in out Test) renames Test_Get_Channels_Str;
--  id:2.2/d74b674a2a965a59/Get_Channels_Str/1/0/
   procedure Test_Get_Channels_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn   : constant String := "get_channels_str";
      Tmpl : Mutools.Templates.Template_Type;
      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Channels_Str (Spec => Spec));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_nores.xml");
      Assert (Condition => Get_Channels_Str (Spec => Spec) = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Channels_Str;
--  end read only


--  begin read only
   procedure Test_Get_Devices_Str (Gnattest_T : in out Test);
   procedure Test_Get_Devices_Str_2fa8b1 (Gnattest_T : in out Test) renames Test_Get_Devices_Str;
--  id:2.2/2fa8b148427ecb4f/Get_Devices_Str/1/0/
   procedure Test_Get_Devices_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn   : constant String := "get_devices_str";
      Tmpl : Mutools.Templates.Template_Type;
      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Devices_Str (Spec => Spec));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_nores.xml");
      Assert (Condition => Get_Devices_Str (Spec => Spec) = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Devices_Str;
--  end read only


--  begin read only
   procedure Test_Get_Event_Str (Gnattest_T : in out Test);
   procedure Test_Get_Event_Str_06c1b4 (Gnattest_T : in out Test) renames Test_Get_Event_Str;
--  id:2.2/06c1b4962251c1a1/Get_Event_Str/1/0/
   procedure Test_Get_Event_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn   : constant String := "get_event_str";
      Tmpl : Mutools.Templates.Template_Type;
      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Event_Str (Spec => Spec));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_nores.xml");
      Assert (Condition => Get_Memory_Str (Spec => Spec) = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Event_Str;
--  end read only


--  begin read only
   procedure Test_Get_Memory_Arrays_Str (Gnattest_T : in out Test);
   procedure Test_Get_Memory_Arrays_Str_9d664b (Gnattest_T : in out Test) renames Test_Get_Memory_Arrays_Str;
--  id:2.2/9d664b04eddb254a/Get_Memory_Arrays_Str/1/0/
   procedure Test_Get_Memory_Arrays_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn   : constant String := "get_memory_arrays";
      Tmpl : Mutools.Templates.Template_Type;
      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Memory_Arrays_Str (Spec => Spec));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_nores.xml");
      Assert (Condition => Get_Memory_Arrays_Str (Spec => Spec) = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Memory_Arrays_Str;
--  end read only


--  begin read only
   procedure Test_Get_Channel_Arrays_Str (Gnattest_T : in out Test);
   procedure Test_Get_Channel_Arrays_Str_f5bab9 (Gnattest_T : in out Test) renames Test_Get_Channel_Arrays_Str;
--  id:2.2/f5bab9e21e0677ea/Get_Channel_Arrays_Str/1/0/
   procedure Test_Get_Channel_Arrays_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn   : constant String := "get_channel_arrays";
      Tmpl : Mutools.Templates.Template_Type;
      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Channel_Arrays_Str (Spec => Spec));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_nores.xml");
      Assert (Condition => Get_Channel_Arrays_Str (Spec => Spec) = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Channel_Arrays_Str;
--  end read only


--  begin read only
   procedure Test_Get_Config_Str (Gnattest_T : in out Test);
   procedure Test_Get_Config_Str_e1882c (Gnattest_T : in out Test) renames Test_Get_Config_Str;
--  id:2.2/e1882c78bcd9832f/Get_Config_Str/1/0/
   procedure Test_Get_Config_Str (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Fn   : constant String := "get_config_str";
      Tmpl : Mutools.Templates.Template_Type;
      Spec : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_vt.xml");

      Tmpl := Mutools.Templates.Create
        (Content => Get_Config_Str (Spec => Spec));
      Mutools.Templates.Write (Template => Tmpl,
                               Filename => "obj/" & Fn);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "obj/" & Fn,
               Filename2 => "data/" & Fn),
              Message   => "Content mismatch (1)");
      Ada.Directories.Delete_File (Name => "obj/" & Fn);

      Muxml.Parse (Data => Spec,
                   Kind => Muxml.None,
                   File => "data/component_nores.xml");
      Assert (Condition => Get_Config_Str (Spec => Spec) = "",
              Message   => "Content mismatch (2)");
--  begin read only
   end Test_Get_Config_Str;
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
end Cspec.Generators.Test_Data.Tests;
