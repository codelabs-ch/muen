--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Features.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Spec.Features.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  spec-features.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Write_No_IOMMUs is
         IOMMU_Spec : constant String := "obj/skp-iommu.ads";
         Policy     : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/features/iommu",
            Name  => "enabled",
            Value => "false");

         Write (Output_Dir => "obj",
                Policy     => Policy);
         Ada.Directories.Delete_File (Name => "obj/skp-features.ads");

         Assert (Condition => not Ada.Directories.Exists (Name => IOMMU_Spec),
                 Message   => "IOMMU spec exists");
      end Write_No_IOMMUs;

      ----------------------------------------------------------------------

      procedure Write_XApic is
         XApic_Spec : constant String := "obj/skp-features-xapic.ads";
         Policy     : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Muxml.Utils.Set_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/features/x2apic",
            Name  => "enabled",
            Value => "false");

         Write (Output_Dir => "obj",
                Policy     => Policy);
         Ada.Directories.Delete_File (Name => "obj/skp-features.ads");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => XApic_Spec,
                  Filename2 => "data/skp-features-xapic.ref"),
                 Message   => "xAPIC spec mismatch");
         Ada.Directories.Delete_File (Name => XApic_Spec);
      end Write_XApic;
   begin
      Write_No_IOMMUs;
      Write_XApic;
--  begin read only
   end Test_Write;
--  end read only

end Spec.Features.Test_Data.Tests;
