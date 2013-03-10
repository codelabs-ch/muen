--
-- Copyright (c) 2007-2011 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--

with Ada.Strings;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Util.Measures;
with Util.Log.Loggers;

package body Ahven.Framework is
   use Ahven.AStrings;

   -- A few local procedures, so we do not need to duplicate code.
   procedure Free_Test is
      new Ada.Unchecked_Deallocation (Object => Test'Class,
                                      Name   => Test_Class_Access);

   generic
      with procedure Action is <>;
   procedure Execute_Internal
     (Test_Object     : in out Test'Class;
      Listener_Object : in out Listeners.Result_Listener'Class);
   -- Logic for Execute procedures. Action is specified by the caller.

   procedure Set_Up (T : in out Test) is
   begin
      null;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
   begin
      null;
   end Tear_Down;

   procedure Run (T         : in out Test;
                  Listener  : in out Listeners.Result_Listener'Class) is
   begin
      Run (T => Test'Class (T), Listener => Listener, Timeout => 0.0);
   end Run;

   procedure Run (T         : in out Test;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class) is
   begin
      Run (T         => Test'Class (T),
           Test_Name => Test_Name,
           Listener  => Listener,
           Timeout   => 0.0);
   end Run;

   procedure Execute_Internal
     (Test_Object     : in out Test'Class;
      Listener_Object : in out Listeners.Result_Listener'Class)
   is
      use Ahven.Listeners;
   begin
      -- This Start_Test here is called for Test_Suites and Test_Cases.
      -- Info includes only the name of the test suite/case.
      --
      -- There is a separate Start_Test/End_Test pair for test routines
      -- in the Run (T : in out Test_Case; ...) procedure.
      Listeners.Start_Test
        (Listener_Object,
         (Phase     => TEST_BEGIN,
          Test_Name => To_Bounded_String (Get_Name (Test_Object)),
          Test_Kind => CONTAINER));

      Action;

      -- Like Start_Test, only for Test_Suites and Test_Cases.
      Listeners.End_Test
        (Listener_Object,
         (Phase     => TEST_END,
          Test_Name => To_Bounded_String (Get_Name (Test_Object)),
          Test_Kind => CONTAINER));
   end Execute_Internal;

   procedure Execute (T        : in out Test'Class;
                      Listener : in out Listeners.Result_Listener'Class;
                      Timeout  :        Test_Duration) is
      procedure Run_Impl is
      begin
         Run (T, Listener, Timeout);
      end Run_Impl;

      procedure Execute_Impl is new Execute_Internal (Action => Run_Impl);
   begin
      Execute_Impl (Test_Object => T, Listener_Object => Listener);
   end Execute;

   procedure Execute (T           : in out Test'Class;
                      Test_Name   :        String;
                      Listener    : in out Listeners.Result_Listener'Class;
                      Timeout     :        Test_Duration) is
      procedure Run_Impl is
      begin
         Run (T         => T,
              Test_Name => Test_Name,
              Listener  => Listener,
              Timeout   => Timeout);
      end Run_Impl;

      procedure Execute_Impl is new Execute_Internal (Action => Run_Impl);
   begin
      Execute_Impl (Test_Object => T, Listener_Object => Listener);
   end Execute;

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Object_Test_Routine_Access;
                               Name    :        String)
   is
      Command : constant Test_Command :=
        (Command_Kind   => OBJECT,
         Name           => To_Bounded_String
                             (Source => Name,
                              Drop   => Ada.Strings.Right),
         Object_Routine => Routine);
   begin
      Test_Command_List.Append (T.Routines, Command);
   end Add_Test_Routine;

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Simple_Test_Routine_Access;
                               Name    :        String)
   is
      Command : constant Test_Command :=
        (Command_Kind   => SIMPLE,
         Name           => To_Bounded_String
                             (Source => Name,
                              Drop   => Ada.Strings.Right),
         Simple_Routine => Routine);
   begin
      Test_Command_List.Append (T.Routines, Command);
   end Add_Test_Routine;

   -- The heart of the package.
   -- Run one test routine (well, Command at this point) and
   -- store the result to the Result object.
   procedure Run_Command (Command  :        Test_Command;
                          Info     :        Listeners.Context;
                          Timeout  :        Test_Duration;
                          Listener : in out Listeners.Result_Listener'Class;
                          T        : in out Test_Case'Class) is
      use Ahven.Listeners;
      type Test_Status is
        (TEST_PASS, TEST_FAIL, TEST_ERROR, TEST_TIMEOUT, TEST_SKIP);

      protected type Test_Results is
         function Get_Status return Test_Status;
         procedure Set_Status (Value : Test_Status);

         function Get_Message return Bounded_String;
         procedure Set_Message (Value : Bounded_String);

         function Get_Long_Message return Bounded_String;
         procedure Set_Long_Message (Value : Bounded_String);
      private
         Status : Test_Status := Test_Error;
         Message : Bounded_String;
         Long_Message : Bounded_String;
      end Test_Results;

      protected body Test_Results is
         function Get_Status return Test_Status is
         begin
            return Status;
         end Get_Status;

         procedure Set_Status (Value : Test_Status) is
         begin
            Status := Value;
         end Set_Status;

         function Get_Message return Bounded_String is
         begin
            return Message;
         end Get_Message;

         procedure Set_Message (Value : Bounded_String) is
         begin
            Message := Value;
         end Set_Message;

         function Get_Long_Message return Bounded_String is
         begin
            return Long_Message;
         end Get_Long_Message;

         procedure Set_Long_Message (Value : Bounded_String) is
         begin
            Long_Message := Value;
         end Set_Long_Message;
      end Test_Results;

      Result : Test_Results;

      --  In order to collect all the performance measures in a same set, use the same
      --  measure set on the test running task and on the main task.  This allows us to
      --  save all the measures together in an XML result file at the end.
      Perf   : constant Util.Measures.Measure_Set_Access := Util.Measures.Get_Current;

      task type Command_Task is
         entry Start_Command;
         entry End_Command;
      end Command_Task;

      procedure Run_A_Command is
      begin
         Util.Measures.Set_Current (Perf);
         begin
            Run (Command, T);
            Result.Set_Status (TEST_PASS);
         exception
            when E : Assertion_Error =>
               Result.Set_Status (TEST_FAIL);
               Result.Set_Message (To_Bounded_String
                 (Source => Ada.Exceptions.Exception_Message (E),
                  Drop   => Ada.Strings.Right));
            when E : Test_Skipped_Error =>
               Result.Set_Status (TEST_SKIP);
               Result.Set_Message (To_Bounded_String
                 (Source => Ada.Exceptions.Exception_Message (E),
                  Drop   => Ada.Strings.Right));
            when E : others =>
               Result.Set_Status (TEST_ERROR);
               Result.Set_Message (To_Bounded_String
                 (Source => Ada.Exceptions.Exception_Name (E),
                  Drop   => Ada.Strings.Right));
               Result.Set_Long_Message (To_Bounded_String
                 (Source => Ada.Exceptions.Exception_Message (E)
                     & ASCII.LF & Util.Log.Loggers.Traceback (E),
                  Drop   => Ada.Strings.Right));
         end;
      end Run_A_Command;

      task body Command_Task is
      begin
         accept Start_Command;
         Run_A_Command;
         accept End_Command;
      end Command_Task;

      Status : Test_Status;

   begin
      if Timeout > 0.0 then
         declare
            Command_Runner : Command_Task;
         begin
            Command_Runner.Start_Command;
            select
               Command_Runner.End_Command;
            or
               delay Duration (Timeout);
               abort Command_Runner;
               Result.Set_Status (Test_Timeout);
            end select;
         end;
      else
         Run_A_Command;
      end if;
      Status := Result.Get_Status;

      case Status is
         when TEST_PASS =>
            Listeners.Add_Pass (Listener, Info);
         when TEST_FAIL =>
            Listeners.Add_Failure
              (Listener,
               (Phase        => TEST_RUN,
                Test_Name    => Info.Test_Name,
                Test_Kind    => CONTAINER,
                Routine_Name => Info.Routine_Name,
                Message      => Result.Get_Message,
                Long_Message => Null_Bounded_String));
         when TEST_ERROR =>
            Listeners.Add_Error
              (Listener,
               (Phase        => Listeners.TEST_RUN,
                Test_Name    => Info.Test_Name,
                Test_Kind    => CONTAINER,
                Routine_Name => Info.Routine_Name,
                Message      => Result.Get_Message,
                Long_Message => Result.Get_Long_Message));
         when TEST_TIMEOUT =>
            Listeners.Add_Error
              (Listener,
               (Phase        => Listeners.TEST_RUN,
                Test_Name    => Info.Test_Name,
                Test_Kind    => CONTAINER,
                Routine_Name => Info.Routine_Name,
                Message      => To_Bounded_String ("TIMEOUT"),
                Long_Message => Null_Bounded_String));
         when TEST_SKIP =>
            Listeners.Add_Skipped
              (Listener,
               (Phase        => TEST_RUN,
                Test_Name    => Info.Test_Name,
                Test_Kind    => CONTAINER,
                Routine_Name => Info.Routine_Name,
                Message      => Result.Get_Message,
                Long_Message => Null_Bounded_String));
      end case;
   end Run_Command;

   function Get_Name (T : Test_Case) return String is
   begin
      return To_String (T.Name);
   end Get_Name;

   procedure Run_Internal
     (T            : in out Test_Case;
      Listener     : in out Listeners.Result_Listener'Class;
      Command      :        Test_Command;
      Test_Name    :        String;
      Routine_Name :        String;
      Timeout      :        Test_Duration)
   is
      use Ahven.Listeners;
   begin
      Listeners.Start_Test
        (Listener,
         (Phase     => Ahven.Listeners.TEST_BEGIN,
          Test_Name => To_Bounded_String (Test_Name),
          Test_Kind => ROUTINE));
      Run_Command (Command  => Command,
                   Info     => (Phase        => Listeners.TEST_RUN,
                                Test_Name    => To_Bounded_String (Test_Name),
                                Test_Kind    => ROUTINE,
                                Routine_Name =>
                                  To_Bounded_String (Routine_Name),
                                Message      => Null_Bounded_String,
                                Long_Message => Null_Bounded_String),
                   Timeout  => Timeout,
                   Listener => Listener,
                   T        => T);
      Listeners.End_Test
        (Listener,
         (Phase          => Ahven.Listeners.TEST_END,
          Test_Name      => To_Bounded_String (Test_Name),
          Test_Kind      => ROUTINE));
   end Run_Internal;

   -- Run procedure for Test_Case.
   --
   -- Loops over the test routine list and executes the routines.
   procedure Run (T        : in out Test_Case;
                  Listener : in out Listeners.Result_Listener'Class;
                  Timeout  :        Test_Duration)
   is
      procedure Exec (Cmd : in out Test_Command) is
      begin
         Run_Internal (T            => T,
                       Listener     => Listener,
                       Command      => Cmd,
                       Timeout      => Timeout,
                       Test_Name    => Get_Name (T),
                       Routine_Name => To_String (Cmd.Name));
      end Exec;

      procedure Run_All is new Test_Command_List.For_Each
        (Action => Exec);
   begin
      Run_All (T.Routines);
   end Run;

   -- Purpose of the procedure is to run all
   -- test routines with name Test_Name.
   procedure Run (T         : in out Test_Case;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration)
   is
      procedure Exec (Cmd : in out Test_Command) is
      begin
         if To_String (Cmd.Name) = Test_Name then
            Run_Internal (T            => T,
                          Listener     => Listener,
                          Command      => Cmd,
                          Timeout      => Timeout,
                          Test_Name    => Get_Name (T),
                          Routine_Name => To_String (Cmd.Name));
         end if;
      end Exec;

      procedure Run_All is new Test_Command_List.For_Each
        (Action => Exec);
   begin
      Run_All (T.Routines);
   end Run;

   function Test_Count (T : Test_Case) return Test_Count_Type is
   begin
      return Test_Count_Type (Test_Command_List.Length (T.Routines));
   end Test_Count;

   function Test_Count (T : Test_Case; Test_Name : String)
     return Test_Count_Type
   is
      use Test_Command_List;

      Counter  : Test_Count_Type := 0;

      procedure Increase (Cmd : in out Test_Command) is
      begin
         if To_String (Cmd.Name) = Test_Name then
            Counter := Counter + 1;
         end if;
      end Increase;

      procedure Count_Commands is new
        Test_Command_List.For_Each (Action => Increase);
   begin
      Count_Commands (T.Routines);

      return Counter;
   end Test_Count;

   procedure Finalize (T : in out Test_Case) is
   begin
      Test_Command_List.Clear (T.Routines);
   end Finalize;

   procedure Set_Name (T : in out Test_Case; Name : String) is
   begin
      T.Name := To_Bounded_String (Source => Name, Drop => Ada.Strings.Right);
   end Set_Name;

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access is
   begin
      return
        new Test_Suite'
          (Ada.Finalization.Controlled with
           Suite_Name        => To_Bounded_String
                                  (Source => Suite_Name,
                                   Drop   => Ada.Strings.Right),
           Test_Cases        => Test_List.Empty_List,
           Static_Test_Cases => Indefinite_Test_List.Empty_List);
   end Create_Suite;

   function Create_Suite (Suite_Name : String)
     return Test_Suite is
   begin
      return (Ada.Finalization.Controlled with
              Suite_Name        => To_Bounded_String
                                     (Source => Suite_Name,
                                      Drop   => Ada.Strings.Right),
              Test_Cases        => Test_List.Empty_List,
              Static_Test_Cases => Indefinite_Test_List.Empty_List);
   end Create_Suite;

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access) is
   begin
      Test_List.Append (Suite.Test_Cases, (Ptr => T));
   end Add_Test;

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Suite_Access) is
   begin
      Add_Test (Suite, Test_Class_Access (T));
   end Add_Test;

   procedure Add_Static_Test
     (Suite : in out Test_Suite; T : Test'Class) is
   begin
      Indefinite_Test_List.Append (Suite.Static_Test_Cases, T);
   end Add_Static_Test;

   function Get_Name (T : Test_Suite) return String is
   begin
      return To_String (T.Suite_Name);
   end Get_Name;

   procedure Run (T        : in out Test_Suite;
                  Listener : in out Listeners.Result_Listener'Class;
                  Timeout  :        Test_Duration)
   is
      -- Some nested procedure exercises here.
      --
      -- Execute_Cases is for normal test list
      -- and Execute_Static_Cases is for indefinite test list.
      --
      -- Normal test list does not have For_Each procedure,
      -- so we need to loop manually.

      -- A helper procedure which runs Execute for the given test.
      procedure Execute_Test (Current : in out Test'Class) is
      begin
         Execute (Current, Listener, Timeout);
      end Execute_Test;

      procedure Execute_Test_Ptr (Current : in out Test_Class_Wrapper) is
      begin
         Execute (Current.Ptr.all, Listener, Timeout);
      end Execute_Test_Ptr;

      procedure Execute_Static_Cases is
        new Indefinite_Test_List.For_Each (Action => Execute_Test);
      procedure Execute_Cases is
        new Test_List.For_Each (Action => Execute_Test_Ptr);
   begin
      Execute_Cases (T.Test_Cases);
      Execute_Static_Cases (T.Static_Test_Cases);
   end Run;

   procedure Run (T         : in out Test_Suite;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration)
   is
      procedure Execute_Test (Current : in out Test'Class) is
      begin
         if Get_Name (Current) = Test_Name then
            Execute (T => Current, Listener => Listener, Timeout => Timeout);
         else
            Execute (T         => Current,
                     Test_Name => Test_Name,
                     Listener  => Listener,
                     Timeout   => Timeout);
         end if;
      end Execute_Test;

      procedure Execute_Test_Ptr (Current : in out Test_Class_Wrapper) is
      begin
         Execute_Test (Current.Ptr.all);
      end Execute_Test_Ptr;

      procedure Execute_Cases is
        new Test_List.For_Each (Action => Execute_Test_Ptr);

      procedure Execute_Static_Cases is
        new Indefinite_Test_List.For_Each (Action => Execute_Test);
   begin
      if Test_Name = To_String (T.Suite_Name) then
         Run (T, Listener, Timeout);
      else
         Execute_Cases (T.Test_Cases);
         Execute_Static_Cases (T.Static_Test_Cases);
      end if;
   end Run;

   function Test_Count (T : Test_Suite) return Test_Count_Type is
      Counter : Test_Count_Type := 0;

      procedure Inc_Counter (Test_Obj : in out Test'Class) is
      begin
         Counter := Counter + Test_Count (Test_Obj);
      end Inc_Counter;

      procedure Inc_Counter_Ptr (Wrapper : in out Test_Class_Wrapper) is
      begin
         Inc_Counter (Wrapper.Ptr.all);
      end Inc_Counter_Ptr;
   begin
      declare
         use Test_List;
         procedure Count_All is new For_Each (Action => Inc_Counter_Ptr);
      begin
         Count_All (T.Test_Cases);
      end;

      declare
         use Indefinite_Test_List;
         procedure Count_All is new For_Each (Action => Inc_Counter);
      begin
         Count_All (T.Static_Test_Cases);
      end;

      return Counter;
   end Test_Count;

   function Test_Count (T : Test_Suite; Test_Name : String)
     return Test_Count_Type is
      Counter : Test_Count_Type := 0;

      procedure Handle_Test (Test_Object : in out Test'Class) is
      begin
         if Get_Name (Test_Object) = Test_Name then
            Counter := Counter + Test_Count (Test_Object);
         else
            Counter := Counter + Test_Count (Test_Object, Test_Name);
         end if;
      end Handle_Test;

      procedure Handle_Test_Ptr (Obj : in out Test_Class_Wrapper) is
      begin
         Handle_Test (Obj.Ptr.all);
      end Handle_Test_Ptr;

      procedure Count_Static is
        new Indefinite_Test_List.For_Each (Action => Handle_Test);
      procedure Count_Tests is
        new Test_List.For_Each (Action => Handle_Test_Ptr);
   begin
      if Test_Name = To_String (T.Suite_Name) then
         return Test_Count (T);
      end if;

      Count_Tests (T.Test_Cases);
      Count_Static (T.Static_Test_Cases);

      return Counter;
   end Test_Count;

   procedure Adjust (T : in out Test_Suite) is
      use Test_List;

      New_List : List := Empty_List;

      procedure Create_Copy (Item : in out Test_Class_Wrapper) is
      begin
         Append (New_List, (Ptr => new Test'Class'(Item.Ptr.all)));
      end Create_Copy;

      procedure Copy_All is new For_Each (Action => Create_Copy);
   begin
      Copy_All (T.Test_Cases);

      T.Test_Cases := New_List;
   end Adjust;

   procedure Finalize  (T : in out Test_Suite) is
      use Test_List;

      procedure Free_Item (Item : in out Test_Class_Wrapper) is
      begin
         Free_Test (Item.Ptr);
      end Free_Item;

      procedure Free_All is new For_Each (Action => Free_Item);

   begin
      Free_All (T.Test_Cases);
      Clear (T.Test_Cases);
   end Finalize;

   procedure Release_Suite (T : Test_Suite_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Test_Suite,
                                        Name   => Test_Suite_Access);
      Ptr : Test_Suite_Access := T;
   begin
      Free (Ptr);
   end Release_Suite;

   procedure Run (Command : Test_Command; T : in out Test_Case'Class) is
   begin
      case Command.Command_Kind is
         when SIMPLE =>
            Command.Simple_Routine.all;
         when OBJECT =>
            Set_Up (T);
            begin
               Command.Object_Routine.all (T);
            exception
               when others =>
                  --  Make sure Tear_Down is called even if the test failed.
                  Tear_Down (T);
                  raise;
            end;
            Tear_Down (T);
      end case;
   end Run;

   package body Indefinite_Test_List is
      procedure Remove (Ptr : Node_Access) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Object => Node,
                                           Name   => Node_Access);
         My_Ptr : Node_Access := Ptr;
      begin
         Ptr.Next := null;
         Free_Test (My_Ptr.Data);
         My_Ptr.Data := null;
         Free (My_Ptr);
      end Remove;

      procedure Append (Target : in out List;
                        Node_Data : Test'Class) is
         New_Node : Node_Access  := null;
      begin
         New_Node := new Node'(Data => new Test'Class'(Node_Data),
                               Next => null);

         if Target.Last = null then
            Target.Last := New_Node;
            Target.First := New_Node;
         else
            Target.Last.Next := New_Node;
            Target.Last := New_Node;
         end if;
      end Append;

      procedure Clear (Target : in out List) is
         Current_Node : Node_Access := Target.First;
         Next_Node : Node_Access := null;
      begin
         while Current_Node /= null loop
            Next_Node := Current_Node.Next;
            Remove (Current_Node);
            Current_Node := Next_Node;
         end loop;

         Target.First := null;
         Target.Last := null;
      end Clear;

      procedure For_Each (Target : List) is
         Current_Node : Node_Access := Target.First;
      begin
         while Current_Node /= null loop
            Action (Current_Node.Data.all);
            Current_Node := Current_Node.Next;
         end loop;
      end For_Each;

      procedure Initialize (Target : in out List) is
      begin
         Target.Last := null;
         Target.First := null;
      end Initialize;

      procedure Finalize (Target : in out List) is
      begin
         Clear (Target);
      end Finalize;

      procedure Adjust (Target : in out List) is
         Target_Last : Node_Access := null;
         Target_First : Node_Access := null;
         Current : Node_Access := Target.First;
         New_Node : Node_Access;
      begin
         while Current /= null loop
            New_Node := new Node'(Data => new Test'Class'(Current.Data.all),
                                  Next => null);

            if Target_Last = null then
               Target_First := New_Node;
            else
               Target_Last.Next := New_Node;
            end if;
            Target_Last := New_Node;

            Current := Current.Next;
         end loop;
         Target.First := Target_First;
         Target.Last := Target_Last;
      end Adjust;
   end Indefinite_Test_List;
end Ahven.Framework;
