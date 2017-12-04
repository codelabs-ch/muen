--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Spec.Skp_Interrupts.Test_Data.

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
package body Spec.Skp_Interrupts.Test_Data.Tests is

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
   --  spec-skp_interrupts.ads:25:4:Write
--  end read only

      pragma Unreferenced (Gnattest_T);

      Output_Dir : constant String := "obj";
      Spec       : constant String := Output_Dir & "/skp-interrupts.ads";
   begin
      declare
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_B,
                      File => "data/test_policy.xml");

         Write (Output_Dir => Output_Dir,
                Policy     => Policy);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Spec,
                  Filename2 => "data/skp-interrupts.ads"),
                 Message   => "Interrupt spec mismatch");
         Ada.Directories.Delete_File (Name => Spec);
      end;

      Write_No_IRQs :
      declare
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

            Write (Output_Dir => Output_Dir,
                   Policy     => Policy);
            Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => Spec,
                     Filename2 => "data/skp-interrupts_noirq.ref"),
                    Message   => "Interrupt spec mismatch");
            Ada.Directories.Delete_File (Name => Spec);
         end;
      end Write_No_IRQs;
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
end Spec.Skp_Interrupts.Test_Data.Tests;
