--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Cmd_Stream.Devices.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Ada.Directories;

with Muxml.Utils;

with Test_Utils;
--  begin read only
--  end read only
package body Cmd_Stream.Devices.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create_Physical_PCI_Devices (Gnattest_T : in out Test);
   procedure Test_Create_Physical_PCI_Devices_cdc2a2 (Gnattest_T : in out Test) renames Test_Create_Physical_PCI_Devices;
--  id:2.2/cdc2a255188579c0/Create_Physical_PCI_Devices/1/0/
   procedure Test_Create_Physical_PCI_Devices (Gnattest_T : in out Test) is
   --  cmd_stream-devices.ads:26:4:Create_Physical_PCI_Devices
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Fn     : constant String := "create_phyiscal_pci_devices.xml";
      Fn_Obj : constant String := "obj/" & Fn;
      Stream : Utils.Stream_Document_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Utils.Create (Stream_Doc => Stream,
                    Filename   => Fn_Obj);

      Create_Physical_PCI_Devices
        (Policy     => Policy,
         Stream_Doc => Stream);
      Utils.Close (Stream_Doc => Stream);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/" & Fn,
               Filename2 => Fn_Obj),
              Message   => "Files differ");
      Ada.Directories.Delete_File (Name => Fn_Obj);
--  begin read only
   end Test_Create_Physical_PCI_Devices;
--  end read only


--  begin read only
   procedure Test_Create_Physical_Legacy_Devices (Gnattest_T : in out Test);
   procedure Test_Create_Physical_Legacy_Devices_d247f0 (Gnattest_T : in out Test) renames Test_Create_Physical_Legacy_Devices;
--  id:2.2/d247f04b4bc16b6d/Create_Physical_Legacy_Devices/1/0/
   procedure Test_Create_Physical_Legacy_Devices (Gnattest_T : in out Test) is
   --  cmd_stream-devices.ads:32:4:Create_Physical_Legacy_Devices
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Fn     : constant String := "create_phyiscal_legacy_devices.xml";
      Fn_Obj : constant String := "obj/" & Fn;
      Stream : Utils.Stream_Document_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Utils.Create (Stream_Doc => Stream,
                    Filename   => Fn_Obj);

      Create_Physical_Legacy_Devices
        (Policy     => Policy,
         Stream_Doc => Stream);
      Utils.Close (Stream_Doc => Stream);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/" & Fn,
               Filename2 => Fn_Obj),
              Message   => "Files differ");
      Ada.Directories.Delete_File (Name => Fn_Obj);
--  begin read only
   end Test_Create_Physical_Legacy_Devices;
--  end read only


--  begin read only
   procedure Test_Create_VTd_Tables (Gnattest_T : in out Test);
   procedure Test_Create_VTd_Tables_c4767f (Gnattest_T : in out Test) renames Test_Create_VTd_Tables;
--  id:2.2/c4767f64bf48dc38/Create_VTd_Tables/1/0/
   procedure Test_Create_VTd_Tables (Gnattest_T : in out Test) is
   --  cmd_stream-devices.ads:38:4:Create_VTd_Tables
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
      Fn     : constant String := "create_vtd_tables_enabled.xml";
      Fn_Obj : constant String := "obj/" & Fn;
      Stream : Utils.Stream_Document_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Utils.Create (Stream_Doc => Stream,
                    Filename   => Fn_Obj);

      Create_VTd_Tables
        (Policy     => Policy,
         Stream_Doc => Stream);
      Utils.Close (Stream_Doc => Stream);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/" & Fn,
               Filename2 => Fn_Obj),
              Message   => "Files differ (1)");
      Ada.Directories.Delete_File (Name => Fn_Obj);
--  begin read only
   end Test_Create_VTd_Tables;
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
end Cmd_Stream.Devices.Test_Data.Tests;
