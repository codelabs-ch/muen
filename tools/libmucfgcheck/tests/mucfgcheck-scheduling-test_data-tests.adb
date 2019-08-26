--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Scheduling.Test_Data.

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
package body Mucfgcheck.Scheduling.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_CPU_Element_Count (Gnattest_T : in out Test);
   procedure Test_CPU_Element_Count_9baa01 (Gnattest_T : in out Test) renames Test_CPU_Element_Count;
--  id:2.2/9baa01b30bb837f8/CPU_Element_Count/1/0/
   procedure Test_CPU_Element_Count (Gnattest_T : in out Test) is
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
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive tests, must no raise an exception.

      Major_Frame_Ticks (XML_Data => Data);

      Muxml.Utils.Set_Attribute
        (Doc   => Data.Doc,
         XPath => "/system/scheduling/majorFrame/cpu/minorFrame"
         & "[@ticks='60'][1]",
         Name  => "ticks",
         Value => "42");

      begin
         Major_Frame_Ticks (XML_Data => Data);
         Assert (Condition => False,
                 Message   =>"Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "CPU 1 of major frame 0 specifies invalid tick count "
                    & "180, should be 162",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Major_Frame_Ticks;
--  end read only


--  begin read only
   procedure Test_Barrier_ID (Gnattest_T : in out Test);
   procedure Test_Barrier_ID_f855f1 (Gnattest_T : in out Test) renames Test_Barrier_ID;
--  id:2.2/f855f1f17e2f5819/Barrier_ID/1/0/
   procedure Test_Barrier_ID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Barrier_ID (XML_Data => Data);

      --  Set duplicate barrier ID.

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/scheduling/majorFrame/barriers/barrier[@id='3']",
            Name  => "id",
            Value => "2");

         Barrier_ID (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Major frame 0 has multiple barriers with ID 2",
                    Message   => "Exception mismatch");
      end;

      --  Set invalid barrier ID (i.e. larger than barrier count).

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/scheduling/majorFrame/barriers/barrier",
            Name  => "id",
            Value => "42");

         Barrier_ID (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Barrier of major frame 0 has invalid ID 42, must be in "
                    & "range 1 .. 4",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Barrier_ID;
--  end read only


--  begin read only
   procedure Test_Barrier_Size (Gnattest_T : in out Test);
   procedure Test_Barrier_Size_dba514 (Gnattest_T : in out Test) renames Test_Barrier_Size;
--  id:2.2/dba514408c2c4e9a/Barrier_Size/1/0/
   procedure Test_Barrier_Size (Gnattest_T : in out Test) is
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


--  begin read only
   procedure Test_Minor_Frame_Sync_Points (Gnattest_T : in out Test);
   procedure Test_Minor_Frame_Sync_Points_cb28c8 (Gnattest_T : in out Test) renames Test_Minor_Frame_Sync_Points;
--  id:2.2/cb28c86f203faa1a/Minor_Frame_Sync_Points/1/0/
   procedure Test_Minor_Frame_Sync_Points (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Minor_Frame_Sync_Points (XML_Data => Data);

      --  Set invalid barrier size.

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/scheduling/majorFrame/barriers/barrier[@size='3']",
            Name  => "size",
            Value => "2");

         Minor_Frame_Sync_Points (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Barrier 1 of major frame 0 has invalid size 2, should"
                    &" be 3",
                    Message   => "Exception mismatch");
      end;

      --  Remove barrier element.

      declare
         Barriers : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Data.Doc,
              XPath => "/system/scheduling/majorFrame/barriers");
      begin
         Muxml.Utils.Remove_Child
           (Node       => Barriers,
            Child_Name => "barrier");

         Minor_Frame_Sync_Points (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Major frame 0 has invalid barrier count 3, should be 4",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Minor_Frame_Sync_Points;
--  end read only


--  begin read only
   procedure Test_Minor_Frame_Barrier_Refs (Gnattest_T : in out Test);
   procedure Test_Minor_Frame_Barrier_Refs_7035a5 (Gnattest_T : in out Test) renames Test_Minor_Frame_Barrier_Refs;
--  id:2.2/7035a54e1ad8b7e4/Minor_Frame_Barrier_Refs/1/0/
   procedure Test_Minor_Frame_Barrier_Refs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => "data/test_policy.xml");

      --  Positive test, must not raise an exception.

      Minor_Frame_Barrier_Refs (XML_Data => Data);

      --  Change barrier reference.

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/scheduling/majorFrame/cpu/"
            & "minorFrame[@barrier='3'][1]",
            Name  => "barrier",
            Value => "4");

         Minor_Frame_Barrier_Refs (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "References to barrier 3 of major frame 0 do not match "
                    & "barrier size: 3 /= 4",
                    Message   => "Exception mismatch");
      end;

      --  Set barrier reference that is too large.

      begin
         Muxml.Utils.Set_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/scheduling/majorFrame/cpu/"
            & "minorFrame[@barrier='1']",
            Name  => "barrier",
            Value => "42");

         Minor_Frame_Barrier_Refs (XML_Data => Data);
         Assert (Condition => False,
                 Message   => "Exception expected");

      exception
         when E : Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Minor frame 0 of CPU 0 in major frame 0 references "
                    & "invalid barrier 42, must be less than 4",
                    Message   => "Exception mismatch");
      end;
--  begin read only
   end Test_Minor_Frame_Barrier_Refs;
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
end Mucfgcheck.Scheduling.Test_Data.Tests;
