--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Scheduling.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Mucfgcheck.Scheduling.Test_Data.Tests is


--  begin read only
   procedure Test_CPU_Element_Count (Gnattest_T : in out Test);
   procedure Test_CPU_Element_Count_9baa01 (Gnattest_T : in out Test) renames Test_CPU_Element_Count;
--  id:2.2/9baa01b30bb837f8/CPU_Element_Count/1/0/
   procedure Test_CPU_Element_Count (Gnattest_T : in out Test) is
   --  mucfgcheck-scheduling.ads:25:4:CPU_Element_Count
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      declare
         Node : DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/scheduling/majorFrame/cpu[@id='3']");
      begin
         Node := DOM.Core.Nodes.Remove_Child
           (N         => DOM.Core.Nodes.Parent_Node (N => Node),
            Old_Child => Node);
         pragma Unreferenced (Node);

         CPU_Element_Count (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "CPU element count 4 of major frame 2 invalid, active "
                    & "CPU count is 3",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_CPU_Element_Count;
--  end read only


--  begin read only
   procedure Test_Subject_References (Gnattest_T : in out Test);
   procedure Test_Subject_References_8828a8 (Gnattest_T : in out Test) renames Test_Subject_References;
--  id:2.2/8828a835f8aeaa87/Subject_References/1/0/
   procedure Test_Subject_References (Gnattest_T : in out Test) is
   --  mucfgcheck-scheduling.ads:28:4:Subject_References
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/"
         & "minorFrame[@subject='vt']",
         Name  => "subject",
         Value => "nonexistent");

      begin
         Subject_References (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'nonexistent' referenced in scheduling plan not"
                    & " found",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_References;
--  end read only


--  begin read only
   procedure Test_Subject_CPU_Affinity (Gnattest_T : in out Test);
   procedure Test_Subject_CPU_Affinity_49d212 (Gnattest_T : in out Test) renames Test_Subject_CPU_Affinity;
--  id:2.2/49d2127dae4039ab/Subject_CPU_Affinity/1/0/
   procedure Test_Subject_CPU_Affinity (Gnattest_T : in out Test) is
   --  mucfgcheck-scheduling.ads:31:4:Subject_CPU_Affinity
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/"
         & "minorFrame[@subject='vt']",
         Name  => "subject",
         Value => "linux");

      begin
         Subject_CPU_Affinity (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject 'linux' scheduled on wrong CPU 0,"
                    & " should be 1",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Subject_CPU_Affinity;
--  end read only


--  begin read only
   procedure Test_Major_Frame_Ticks (Gnattest_T : in out Test);
   procedure Test_Major_Frame_Ticks_88421f (Gnattest_T : in out Test) renames Test_Major_Frame_Ticks;
--  id:2.2/88421f8587a3f303/Major_Frame_Ticks/1/0/
   procedure Test_Major_Frame_Ticks (Gnattest_T : in out Test) is
   --  mucfgcheck-scheduling.ads:34:4:Major_Frame_Ticks
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");
      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/minorFrame[@ticks='60']",
         Name  => "ticks",
         Value => "42");

      begin
         Major_Frame_Ticks (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid CPU elements in scheduling plan, tick counts "
                    & "differ",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Major_Frame_Ticks;
--  end read only


--  begin read only
   procedure Test_Barrier_Size (Gnattest_T : in out Test);
   procedure Test_Barrier_Size_dba514 (Gnattest_T : in out Test) renames Test_Barrier_Size;
--  id:2.2/dba514408c2c4e9a/Barrier_Size/1/0/
   procedure Test_Barrier_Size (Gnattest_T : in out Test) is
   --  mucfgcheck-scheduling.ads:37:4:Barrier_Size
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Barrier_Size (XML_Data => Data);

      --  Set invalid barrier size (i.e. larger than CPU count).

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/scheduling/majorFrame/barriers/barrier",
            Name  => "size",
            Value => "42");

         Barrier_Size (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Minor frame barrier with invalid size 42, must not "
                    & "exceed 4",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Barrier_Size;
--  end read only

end Mucfgcheck.Scheduling.Test_Data.Tests;
