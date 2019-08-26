--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Pt.Generator.Test_Data.

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
package body Pt.Generator.Test_Data.Tests is

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

      Policy : Muxml.XML_Data_Type;

      Knl_Pt0 : constant String := "obj/kernel_pt_0";
      Knl_Pt1 : constant String := "obj/kernel_pt_1";
      Tau0_Pt : constant String := "obj/tau0_pt";
      Sub1_Pt : constant String := "obj/subject1_pt";
      Sub2_Pt : constant String := "obj/subject2_pt";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => "obj",
             Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/kernel_pt_0.ref",
               Filename2 => Knl_Pt0),
              Message   => "Kernel pagetables mismatch (0)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/kernel_pt_1.ref",
               Filename2 => Knl_Pt1),
              Message   => "Kernel pagetables mismatch (1)");

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/tau0_pt.ref",
               Filename2 => Tau0_Pt),
              Message   => "Tau0 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject1_pt.ref",
               Filename2 => Sub1_Pt),
              Message   => "Subject 1 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/subject2_pt.ref",
               Filename2 => Sub2_Pt),
              Message   => "Subject 2 pagetables mismatch");
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
end Pt.Generator.Test_Data.Tests;
