--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body VTd.Generator.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  vtd-generator.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;

      Root_Table : constant String := "obj/vtd_root";
      Context_0  : constant String := "obj/vtd_context_bus_0";
      Context_23 : constant String := "obj/vtd_context_bus_23";
      Lnx_Dom_Pt : constant String := "obj/vtd_lnx_domain_pt";
      Net_Dom_Pt : constant String := "obj/vtd_net_domain_pt";
      IR_Table   : constant String := "obj/vtd_ir";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write (Output_Dir => "obj",
             Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_root",
               Filename2 => Root_Table),
              Message   => "Root table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_context_bus_0",
               Filename2 => Context_0),
              Message   => "Context 0 table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_context_bus_23",
               Filename2 => Context_23),
              Message   => "Context 23 table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_lnx_domain_pt.ref",
               Filename2 => Lnx_Dom_Pt),
              Message   => "Lnx device domain paging structures mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_net_domain_pt.ref",
               Filename2 => Net_Dom_Pt),
              Message   => "Net device domain paging structures mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_ir",
               Filename2 => IR_Table),
              Message   => "IR table mismatch");

      Ada.Directories.Delete_File (Name => Root_Table);
      Ada.Directories.Delete_File (Name => Context_0);
      Ada.Directories.Delete_File (Name => Context_23);
      Ada.Directories.Delete_File (Name => Lnx_Dom_Pt);
      Ada.Directories.Delete_File (Name => Net_Dom_Pt);
      Ada.Directories.Delete_File (Name => IR_Table);

      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
         Child_Name => "deviceDomains");

      --  No device domains present, no context tables and paging structures
      --  must be generated.

      Write (Output_Dir => "obj",
             Policy     => Policy);
      Assert (Condition => Ada.Directories.Exists (Name => Root_Table),
              Message   => "Root table does not exist");
      Assert (Condition => not Ada.Directories.Exists (Name => Context_0),
              Message   => "Context 0 table exists (1)");
      Assert (Condition => not Ada.Directories.Exists (Name => Context_23),
              Message   => "Context 23 table exists (1)");
      Assert (Condition => not Ada.Directories.Exists (Name => Lnx_Dom_Pt),
              Message   => "Lnx domain table exists (1)");
      Assert (Condition => not Ada.Directories.Exists (Name => Net_Dom_Pt),
              Message   => "Net domain table exists (1)");

      Ada.Directories.Delete_File (Name => Root_Table);

      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
         Child_Name => "features");

      --  IOMMU feature not enabled, no tables must be generated.

      Write (Output_Dir => "obj",
             Policy     => Policy);
      Assert (Condition => not Ada.Directories.Exists (Name => Root_Table),
              Message   => "Root table exists");
      Assert (Condition => not Ada.Directories.Exists (Name => Context_0),
              Message   => "Context 0 table exists (2)");
      Assert (Condition => not Ada.Directories.Exists (Name => Context_23),
              Message   => "Context 23 table exists (2)");
      Assert (Condition => not Ada.Directories.Exists (Name => Lnx_Dom_Pt),
              Message   => "Lnx domain table exists (2)");
      Assert (Condition => not Ada.Directories.Exists (Name => Net_Dom_Pt),
              Message   => "Net domain table exists (2)");
--  begin read only
   end Test_Write;
--  end read only

end VTd.Generator.Test_Data.Tests;
