--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Msrstore.Generator.Test_Data.

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
package body Msrstore.Generator.Test_Data.Tests is

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
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sub1_Msrstore : constant String := "obj/subject1_msrstore";
      Sub2_Msrstore : constant String := "obj/subject2_msrstore";

      procedure Write_MSR_Stores;
      procedure Read_Access_Only;

      ----------------------------------------------------------------------

      procedure Read_Access_Only
      is
         Policy : Muxml.XML_Data_Type;

      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            MSRs : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/subjects/subject/"
                 & "vcpu/msrs/msr");
         begin
            for I in 1 .. DOM.Core.Nodes.Length (List => MSRs) loop
               DOM.Core.Elements.Set_Attribute
                 (Elem  => DOM.Core.Nodes.Item (List  => MSRs,
                                                Index => I - 1),
                  Name  => "mode",
                  Value => "r");
            end loop;
         end;

         Write (Output_Dir => "obj",
                Policy     => Policy);

         Assert (Condition => not Ada.Directories.Exists
                 (Name => Sub1_Msrstore),
                 Message   => "Subject 1 MSR store created");
         Assert (Condition => not Ada.Directories.Exists
                 (Name => Sub2_Msrstore),
                 Message   => "Subject 2 MSR store created");
      end Read_Access_Only;

      ----------------------------------------------------------------------

      procedure Write_MSR_Stores
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => "obj",
                Policy     => Policy);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/subject1_msrstore.ref",
                  Filename2 => Sub1_Msrstore),
                 Message   => "Subject 1 MSR store mismatch");
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/subject2_msrstore.ref",
                  Filename2 => Sub2_Msrstore),
                 Message   => "Subject 2 MSR store mismatch");

         Ada.Directories.Delete_File (Name => Sub1_Msrstore);
         Ada.Directories.Delete_File (Name => Sub2_Msrstore);
      end Write_MSR_Stores;
   begin
      Write_MSR_Stores;
      Read_Access_Only;
--  begin read only
   end Test_Write;
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
end Msrstore.Generator.Test_Data.Tests;
