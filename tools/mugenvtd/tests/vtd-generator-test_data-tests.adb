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
      Lnx_Dom_Pt : constant String := "obj/lnx_domain_pt";
      Net_Dom_Pt : constant String := "obj/net_domain_pt";
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
              (Filename1 => "data/lnx_domain_pt.ref",
               Filename2 => Lnx_Dom_Pt),
              Message   => "Lnx device domain paging structures mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/net_domain_pt.ref",
               Filename2 => Net_Dom_Pt),
              Message   => "Net device domain paging structures mismatch");

      Ada.Directories.Delete_File (Name => Root_Table);
      Ada.Directories.Delete_File (Name => Lnx_Dom_Pt);
      Ada.Directories.Delete_File (Name => Net_Dom_Pt);

      declare
         Platform : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/platform");
      begin
         Muxml.Utils.Remove_Child
           (Node       => Platform,
            Child_Name => "devices");
         Muxml.Utils.Remove_Child
           (Node       => DOM.Core.Documents.Get_Element (Doc => Policy.Doc),
            Child_Name => "deviceDomains");
      end;

      --  No IOMMU and device domains present, no tables must be generated.

      Write (Output_Dir => "obj",
             Policy     => Policy);
      Assert (Condition => not Ada.Directories.Exists (Name => Root_Table),
              Message   => "Root table exists");
      Assert (Condition => not Ada.Directories.Exists (Name => Lnx_Dom_Pt),
              Message   => "Lnx domain table exists");
      Assert (Condition => not Ada.Directories.Exists (Name => Net_Dom_Pt),
              Message   => "Net domain table exists");
--  begin read only
   end Test_Write;
--  end read only

end VTd.Generator.Test_Data.Tests;
