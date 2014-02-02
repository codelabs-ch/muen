--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Test_Utils;

with Muxml;

with Spec.Generator;

package body Generator_Tests
is

   use Ahven;
   use Spec;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Generator tests");
      T.Add_Test_Routine
        (Routine => Write_Specs'Access,
         Name    => "Write spec files");
      T.Add_Test_Routine
        (Routine => Write_No_IRQs'Access,
         Name    => "Write empty IRQ routing spec file");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_No_IRQs
   is
      Intr_Spec : constant String := "obj/skp-interrupts.ads";

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
           (List  => McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => "/system/subjects/subject[@name='subject1']/devices"),
            Index => 0);
         Tmp : DOM.Core.Node;
         pragma Unreferenced (Tmp);
      begin

         --  Remove all devices with IRQs.

         while DOM.Core.Nodes.Has_Child_Nodes (N => Node) loop
            Tmp := DOM.Core.Nodes.Remove_Child
              (N         => Node,
               Old_Child => DOM.Core.Nodes.First_Child (N => Node));
         end loop;

         Generator.Write (Output_Dir => "obj",
                          Policy     => Policy);

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => Intr_Spec,
                  Filename2 => "data/skp-interrupts_noirq.ref"),
                 Message   => "Interrupt spec mismatch");
      end;
   end Write_No_IRQs;

   -------------------------------------------------------------------------

   procedure Write_Specs
   is
      Policy : Muxml.XML_Data_Type;

      Sched_Spec  : constant String := "obj/skp-scheduling.ads";
      Intr_Spec   : constant String := "obj/skp-interrupts.ads";
      Kernel_Spec : constant String := "obj/skp-kernel.ads";
      Kernel_H    : constant String := "obj/policy.h";
      Subj_Spec   : constant String := "obj/skp-subjects.adb";
      Skp_Spec    : constant String := "obj/skp.ads";
      HW_Spec     : constant String := "obj/skp-hardware.ads";
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      Generator.Write (Output_Dir => "obj",
                       Policy     => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/skp-scheduling.ref",
               Filename2 => Sched_Spec),
              Message => "Scheduling spec mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Intr_Spec,
               Filename2 => "data/skp-interrupts.ref"),
              Message   => "Interrupt spec mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Kernel_Spec,
               Filename2 => "data/skp-kernel.ref"),
              Message   => "Kernel spec mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Kernel_H,
               Filename2 => "data/policy.h.ref"),
              Message   => "Kernel header file mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Subj_Spec,
               Filename2 => "data/skp-subjects.ref"),
              Message   => "Subjects spec mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Skp_Spec,
               Filename2 => "data/skp.ref"),
              Message   => "Skp spec mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => HW_Spec,
               Filename2 => "data/skp-hardware.ref"),
              Message   => "Hardware spec mismatch");
   end Write_Specs;

end Generator_Tests;
