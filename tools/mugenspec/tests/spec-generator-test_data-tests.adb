--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Generator.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Spec.Generator.Test_Data.Tests is


--  begin read only
   procedure Test_Write (Gnattest_T : in out Test);
   procedure Test_Write_23ab15 (Gnattest_T : in out Test) renames Test_Write;
--  id:2.2/23ab1562ae4604fa/Write/1/0/
   procedure Test_Write (Gnattest_T : in out Test) is
   --  spec-generator.ads:26:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Sched_Spec  : constant String := "obj/skp-scheduling.ads";
      Intr_Spec   : constant String := "obj/skp-interrupts.ads";
      Kernel_Spec : constant String := "obj/skp-kernel.ads";
      Kernel_H    : constant String := "obj/policy.h";
      Subj_Spec   : constant String := "obj/skp-subjects.adb";
      Skp_Spec    : constant String := "obj/skp.ads";
      HW_Spec     : constant String := "obj/skp-hardware.ads";
      IOMMU_Spec  : constant String := "obj/skp-iommu.ads";
      Policy_GPR  : constant String := "obj/policy.gpr";

      ----------------------------------------------------------------------

      procedure Write_Specs
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => "obj",
                Policy     => Policy);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/skp-scheduling.ref",
                  Filename2 => Sched_Spec),
                 Message   => "Scheduling spec mismatch");
         Ada.Directories.Delete_File (Name => Sched_Spec);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Intr_Spec,
                  Filename2 => "data/skp-interrupts.ref"),
                 Message   => "Interrupt spec mismatch");
         Ada.Directories.Delete_File (Name => Intr_Spec);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Kernel_Spec,
                  Filename2 => "data/skp-kernel.ref"),
                 Message   => "Kernel spec mismatch");
         Ada.Directories.Delete_File (Name => Kernel_Spec);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Kernel_H,
                  Filename2 => "data/policy.h.ref"),
                 Message   => "Kernel header file mismatch");
         Ada.Directories.Delete_File (Name => Kernel_H);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Subj_Spec,
                  Filename2 => "data/skp-subjects.ref"),
                 Message   => "Subjects spec mismatch");
         Ada.Directories.Delete_File (Name => Subj_Spec);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Skp_Spec,
                  Filename2 => "data/skp.ref"),
                 Message   => "Skp spec mismatch");
         Ada.Directories.Delete_File (Name => Skp_Spec);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => HW_Spec,
                  Filename2 => "data/skp-hardware.ref"),
                 Message   => "Hardware spec mismatch");
         Ada.Directories.Delete_File (Name => HW_Spec);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => IOMMU_Spec,
                  Filename2 => "data/skp-iommu.ref"),
                 Message   => "IOMMU spec mismatch");
         Ada.Directories.Delete_File (Name => IOMMU_Spec);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Policy_GPR,
                  Filename2 => "data/policy.gpr.ref"),
                 Message   => "Policy project file mismatch");
         Ada.Directories.Delete_File (Name => Policy_GPR);
      end Write_Specs;

      ----------------------------------------------------------------------

      procedure Write_No_IRQs
      is
         Intr_Spec : constant String := "obj/skp-interrupts.ads";

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         declare
            Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='subject1']/devices");
            Tmp : DOM.Core.Node;
            pragma Unreferenced (Tmp);
         begin

            --  Remove all devices with IRQs.

            while DOM.Core.Nodes.Has_Child_Nodes (N => Node) loop
               Tmp := DOM.Core.Nodes.Remove_Child
                 (N         => Node,
                  Old_Child => DOM.Core.Nodes.First_Child (N => Node));
            end loop;

            Write (Output_Dir => "obj",
                   Policy     => Policy);

            Ada.Directories.Delete_File (Name => Sched_Spec);
            Ada.Directories.Delete_File (Name => Kernel_Spec);
            Ada.Directories.Delete_File (Name => Kernel_H);
            Ada.Directories.Delete_File (Name => Subj_Spec);
            Ada.Directories.Delete_File (Name => Skp_Spec);
            Ada.Directories.Delete_File (Name => HW_Spec);
            Ada.Directories.Delete_File (Name => IOMMU_Spec);
            Ada.Directories.Delete_File (Name => Policy_GPR);

            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => Intr_Spec,
                     Filename2 => "data/skp-interrupts_noirq.ref"),
                    Message   => "Interrupt spec mismatch");
            Ada.Directories.Delete_File (Name => Intr_Spec);
         end;
      end Write_No_IRQs;

      ----------------------------------------------------------------------

      procedure Write_No_IOMMUs
      is
         Policy : Muxml.XML_Data_Type;
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

         Ada.Directories.Delete_File (Name => Sched_Spec);
         Ada.Directories.Delete_File (Name => Intr_Spec);
         Ada.Directories.Delete_File (Name => Kernel_Spec);
         Ada.Directories.Delete_File (Name => Kernel_H);
         Ada.Directories.Delete_File (Name => Subj_Spec);
         Ada.Directories.Delete_File (Name => Skp_Spec);
         Ada.Directories.Delete_File (Name => HW_Spec);
         Ada.Directories.Delete_File (Name => Policy_GPR);

         Assert (Condition => not Ada.Directories.Exists (Name => IOMMU_Spec),
                 Message   => "IOMMU spec exists");
      end Write_No_IOMMUs;
   begin
      Write_Specs;
      Write_No_IRQs;
      Write_No_IOMMUs;
--  begin read only
   end Test_Write;
--  end read only

end Spec.Generator.Test_Data.Tests;
