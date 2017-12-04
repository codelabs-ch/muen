--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into VTd.Generator.Test_Data.

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
package body VTd.Generator.Test_Data.Tests is

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
   --  vtd-generator.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
         Child_Name => "deviceDomains");

      --  No device domains present, no context tables and paging structures
      --  must be generated.

      Write (Output_Dir => Output_Dir,
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

      Muxml.Utils.Set_Attribute
        (Doc   => Policy.Doc,
         XPath => "/system/config/boolean[@name='iommu_enabled']",
         Name  => "value",
         Value => "false");

      --  IOMMU not enabled, no tables must be generated.

      Write (Output_Dir => Output_Dir,
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


--  begin read only
   procedure Test_Write_Root_Table (Gnattest_T : in out Test);
   procedure Test_Write_Root_Table_f7d782 (Gnattest_T : in out Test) renames Test_Write_Root_Table;
--  id:2.2/f7d782ff599d6081/Write_Root_Table/1/0/
   procedure Test_Write_Root_Table (Gnattest_T : in out Test) is
   --  vtd-generator.ads:33:4:Write_Root_Table
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Write_Root_Table (Output_Dir => Output_Dir,
                        Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_root",
               Filename2 => Root_Table),
              Message   => "Root table mismatch");

      Ada.Directories.Delete_File (Name => Root_Table);
--  begin read only
   end Test_Write_Root_Table;
--  end read only


--  begin read only
   procedure Test_Write_Context_Tables (Gnattest_T : in out Test);
   procedure Test_Write_Context_Tables_129b9c (Gnattest_T : in out Test) renames Test_Write_Context_Tables;
--  id:2.2/129b9c48fba5232a/Write_Context_Tables/1/0/
   procedure Test_Write_Context_Tables (Gnattest_T : in out Test) is
   --  vtd-generator.ads:39:4:Write_Context_Tables
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Write_Context_Tables (Output_Dir => Output_Dir,
                            Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_context_bus_0",
               Filename2 => Context_0),
              Message   => "Context 0 table mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_context_bus_23",
               Filename2 => Context_23),
              Message   => "Context 23 table mismatch");

      Ada.Directories.Delete_File (Name => Context_0);
      Ada.Directories.Delete_File (Name => Context_23);
--  begin read only
   end Test_Write_Context_Tables;
--  end read only


--  begin read only
   procedure Test_Write_Domain_Pagetables (Gnattest_T : in out Test);
   procedure Test_Write_Domain_Pagetables_9a4dfd (Gnattest_T : in out Test) renames Test_Write_Domain_Pagetables;
--  id:2.2/9a4dfd4af316ec97/Write_Domain_Pagetables/1/0/
   procedure Test_Write_Domain_Pagetables (Gnattest_T : in out Test) is
   --  vtd-generator.ads:45:4:Write_Domain_Pagetables
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Write_Domain_Pagetables (Output_Dir => Output_Dir,
                               Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_lnx_domain_pt.ref",
               Filename2 => Lnx_Dom_Pt),
              Message   => "Lnx device domain paging structures mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_net_domain_pt.ref",
               Filename2 => Net_Dom_Pt),
              Message   => "Net device domain paging structures mismatch");

      Ada.Directories.Delete_File (Name => Lnx_Dom_Pt);
      Ada.Directories.Delete_File (Name => Net_Dom_Pt);
--  begin read only
   end Test_Write_Domain_Pagetables;
--  end read only


--  begin read only
   procedure Test_Write_IR_Table (Gnattest_T : in out Test);
   procedure Test_Write_IR_Table_ddfa42 (Gnattest_T : in out Test) renames Test_Write_IR_Table;
--  id:2.2/ddfa42fd934de5cb/Write_IR_Table/1/0/
   procedure Test_Write_IR_Table (Gnattest_T : in out Test) is
   --  vtd-generator.ads:51:4:Write_IR_Table
--  end read only

      pragma Unreferenced (Gnattest_T);

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Write_IR_Table (Output_Dir => Output_Dir,
                      Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_ir",
               Filename2 => IR_Table),
              Message   => "IR table mismatch");
      Ada.Directories.Delete_File (Name => IR_Table);

      --  Delete all IRQ nodes of devices.

      declare
         IRQs : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Policy.Doc,
              XPath => "//irq");
      begin
         for I in 0 .. DOM.Core.Nodes.Length (List => IRQs) - 1 loop
            declare
               Node : DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => IRQs,
                                         Index => I);
            begin
               Node := DOM.Core.Nodes.Remove_Child
                 (N         => DOM.Core.Nodes.Parent_Node (N => Node),
                  Old_Child => Node);
            end;
         end loop;
      end;

      Write_IR_Table (Output_Dir => Output_Dir,
                      Policy     => Policy);
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/vtd_ir_empty",
               Filename2 => IR_Table),
              Message   => "Empty IR table mismatch");
      Ada.Directories.Delete_File (Name => IR_Table);
--  begin read only
   end Test_Write_IR_Table;
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
end VTd.Generator.Test_Data.Tests;
