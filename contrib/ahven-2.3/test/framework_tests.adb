--
-- Copyright (c) 2007 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with Simple_Listener;
with Dummy_Tests;

pragma Elaborate_All (Dummy_Tests);

package body Framework_Tests is
   use Ahven;

   procedure Assert_Eq_Count is
     new Ahven.Assert_Equal (Data_Type => Framework.Test_Count_Type,
                             Image     => Framework.Test_Count_Type'Image);

   procedure Assert_Eq_Nat is
     new Ahven.Assert_Equal (Data_Type => Natural,
                             Image     => Natural'Image);

   procedure Assert_Eq_Int is
     new Ahven.Assert_Equal (Data_Type => Integer,
                             Image     => Integer'Image);

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Simple_Listener.Listener,
      Name   => Simple_Listener.Listener_Access);

   procedure Initialize (T : in out Test) is
      use Framework;
   begin
      Set_Name (T, "Ahven.Framework");

      Add_Test_Routine (T, Test_Set_Up'Access, "Test_Case: Set_Up");
      T.Value := INITIALIZED;
      Add_Test_Routine (T, Test_Tear_Down'Access,
                                  "Test_Case: Tear_Down");
      Add_Test_Routine (T, Test_Test_Case_Run'Access,
                                  "Test_Case: Run");
      Add_Test_Routine (T, Test_Test_Case_Run_Empty'Access,
                                  "Test_Case: Run (Empty)");
      Add_Test_Routine (T, Test_Test_Case_Run_1s_Timeout'Access,
                                  "Test_Case: 1s Timeout");
      Add_Test_Routine (T, Test_Test_Case_Run_Break_Infinite_Loop'Access,
                                  "Test_Case: Break infinite loop");
      Add_Test_Routine (T, Test_Test_Case_Test_Count'Access,
                                  "Test_Case: Test_Count");
      Add_Test_Routine (T, Test_Test_Case_Truncate_Name'Access,
                        "Test_Case: Truncate Name");
      Add_Test_Routine (T, Test_Call_End_Test'Access,
                                  "Test_Case: Run (Call End_Test)");
      Add_Test_Routine (T, Test_Test_Suite_Run'Access,
                                  "Test_Suite: Run");
      Add_Test_Routine (T, Test_Test_Suite_Static_Run'Access,
                                  "Test_Suite: Run (Static)");
      Add_Test_Routine (T, Test_Test_Suite_Name_Run'Access,
                                  "Test_Suite: Run (Name)");
      Add_Test_Routine (T, Test_Test_Suite_Inside_Suite'Access,
                                  "Test_Suite: Suite inside another");
      Add_Test_Routine (T, Test_Test_Suite_Test_Count'Access,
                                  "Test_Suite: Test Count");
      Add_Test_Routine (T, Test_Test_Suite_Test_Static_Count'Access,
                                  "Test_Suite: Test Count (Static)");
      Add_Test_Routine (T, Test_Test_Suite_Test_Name_Count'Access,
                                  "Test_Suite: Test Count (Name)");
      Add_Test_Routine (T, Test_Test_Suite_Cleanup'Access,
                                  "Test_Suite: Cleanup");
   end Initialize;

   procedure Set_Up (T : in out Test) is
   begin
      T.Value := SETUP_DONE;
   end Set_Up;

   procedure Tear_Down (T : in out Test) is
   begin
      T.Value := TEARDOWN_DONE;
   end Tear_Down;

   procedure Test_Set_Up (T : in out Ahven.Framework.Test_Case'Class) is
   begin
      Assert (Test (T).Value = SETUP_DONE, "Set_Up not called!");
   end Test_Set_Up;

   procedure Test_Tear_Down is
      use type Dummy_Tests.Test_State;

      My_Test : Dummy_Tests.Test;
      My_Listener : Simple_Listener.Listener;
   begin
      Dummy_Tests.Run (My_Test, My_Listener);
      Assert (My_Test.State = Dummy_Tests.DOWN, "Tear_Down not called!");
   end Test_Tear_Down;

   procedure Test_Test_Case_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Test : Dummy_Tests.Test;
   begin
      Dummy_Tests.Run (My_Test, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, Dummy_Test_Count,
              "Start_Test calls");

      Free (My_Listener);
   end Test_Test_Case_Run;

   type Empty_Test_Case is new Ahven.Framework.Test_Case with null record;

   procedure Test_Test_Case_Run_Empty is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener;
      My_Test : Empty_Test_Case;
   begin
      Run (My_Test, My_Listener);

      Assert_Eq_Nat (My_Listener.Passes, 0, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, 0, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, 0, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, 0,
              "Start_Test calls");
   end Test_Test_Case_Run_Empty;

   procedure Test_Test_Case_Run_1s_Timeout is
      use Dummy_Tests;
      use Ahven.Framework;
      use type Ada.Calendar.Time;

      My_Test : Dummy_Tests.Test;
      My_Listener : Simple_Listener.Listener;
      Before : Ada.Calendar.Time;
      After : Ada.Calendar.Time;
   begin
      Add_Test_Routine
         (My_Test, Dummy_Tests.This_Test_Takes_12_Seconds'Access,
           "Takes 12 seconds");
      Before := Ada.Calendar.Clock;
      Ahven.Framework.Run (Ahven.Framework.Test_Case (My_Test),
        My_Listener, 1.0);
      After := Ada.Calendar.Clock;

      -- Timing might not be 100% accurate, so measuring
      -- less than 2.0 seconds should give us accetable result
      Assert (After - Before < 2.0, "Test took too long");
   end Test_Test_Case_Run_1s_Timeout;

   procedure Test_Test_Case_Run_Break_Infinite_Loop is
      use Dummy_Tests;
      use Ahven.Framework;
      use type Ada.Calendar.Time;

      My_Test : Dummy_Tests.Test;
      My_Listener : Simple_Listener.Listener;
      Before : Ada.Calendar.Time;
      After : Ada.Calendar.Time;
   begin
      Skip ("Does not work with most Ada compilers.");
      Add_Test_Routine
         (My_Test, Dummy_Tests.This_Test_Has_Infinite_Loop'Access,
           "Has infinite loop");
      Before := Ada.Calendar.Clock;
      Ahven.Framework.Run (Ahven.Framework.Test_Case (My_Test),
        My_Listener, 0.2);
      After := Ada.Calendar.Clock;

      -- Timing might not be 100% accurate, so measuring
      -- less than 1.0 seconds should give us accetable result
      Assert (After - Before < 1.0, "Test took too long");
   end Test_Test_Case_Run_Break_Infinite_Loop;

   procedure Test_Test_Case_Test_Count is
      use type Framework.Test_Count_Type;

      Dummy_Test  : Dummy_Tests.Test;
   begin
      Assert_Eq_Count (Dummy_Tests.Test_Count (Dummy_Test),
                       Dummy_Tests.Dummy_Test_Count,
                       "Test Count");
   end Test_Test_Case_Test_Count;

   procedure Test_Test_Case_Truncate_Name is
      Over_Max    : constant := 180;
      Dummy_Test  : Dummy_Tests.Test;
      Name        : constant String (1..Over_Max) := (others => 'a');
   begin
      Dummy_Tests.Set_Name (Dummy_Test, Name);
   end Test_Test_Case_Truncate_Name;

   procedure Test_Test_Suite_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Suite : Framework.Test_Suite_Access;
   begin
      My_Suite := Framework.Create_Suite ("My suite");
      Framework.Add_Test (My_Suite.all, new Dummy_Tests.Test);

      Framework.Run (My_Suite.all, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 1),
              "Start_Test calls");

      Free (My_Listener);
      Framework.Release_Suite (My_Suite);
   end Test_Test_Suite_Run;

   procedure Test_Test_Suite_Static_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener;
      My_Suite : Framework.Test_Suite := Framework.Create_Suite ("My suite");
      Dummy_Test : Dummy_Tests.Test;
   begin
      Framework.Add_Static_Test (My_Suite, Dummy_Test);

      Framework.Run (My_Suite, My_Listener);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 1),
              "Start_Test calls");
   end Test_Test_Suite_Static_Run;

   procedure Test_Test_Suite_Name_Run is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener;
      My_Suite : Framework.Test_Suite := Framework.Create_Suite ("My suite");
      Dummy_Test : Dummy_Tests.Test;
   begin
      Framework.Add_Static_Test (My_Suite, Dummy_Test);

      Framework.Run (My_Suite, "Failure", My_Listener);

      Assert_Eq_Nat (My_Listener.Passes, 0, "Pass count");
      Assert_Eq_Nat (My_Listener.Errors, 0, "Error count");
      Assert_Eq_Nat (My_Listener.Failures, Dummy_Failures, "Failure count");
      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Failures + 1),
              "Start_Test calls");
   end Test_Test_Suite_Name_Run;

   procedure Test_Call_End_Test is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      My_Test     : Dummy_Tests.Test;
   begin
      Dummy_Tests.Run (My_Test, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Level, 0, "Listener.Level");
      Assert_Eq_Nat (My_Listener.End_Calls, Dummy_Test_Count,
              "End_Test calls");

      Free (My_Listener);
   end Test_Call_End_Test;

   procedure Test_Test_Suite_Inside_Suite is
      use Dummy_Tests;

      My_Listener : Simple_Listener.Listener_Access :=
        new Simple_Listener.Listener;
      Child       : Framework.Test_Suite_Access;
      Parent      : Framework.Test_Suite;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Test (Child.all, new Dummy_Tests.Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Test (Parent, Child);

      Framework.Run (Parent, My_Listener.all);

      Assert_Eq_Nat (My_Listener.Passes, Dummy_Passes, "Amount of passes.");
      Assert_Eq_Nat (My_Listener.Errors, Dummy_Errors, "Amount of errors.");
      Assert_Eq_Nat
        (My_Listener.Failures, Dummy_Failures, "Amount of failures.");
      Assert_Eq_Nat (My_Listener.Level, 0, "Start_Test /= End_Test");
      Assert_Eq_Nat (My_Listener.Start_Calls, (Dummy_Test_Count + 2),
              "Start_Test calls");

      Free (My_Listener);
   end Test_Test_Suite_Inside_Suite;

   -- Test that Test_Count works for dynamic test cases
   procedure Test_Test_Suite_Test_Count is
      use Dummy_Tests;

      Child       : Framework.Test_Suite_Access;
      Parent      : Framework.Test_Suite;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Test (Child.all, new Dummy_Tests.Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Test (Parent, Child);

      Assert_Eq_Count
        (Framework.Test_Count (Parent), Dummy_Test_Count, "Test Count");
   end Test_Test_Suite_Test_Count;

   -- Test that Test_Count works for static test cases
   procedure Test_Test_Suite_Test_Static_Count is
      use Dummy_Tests;
      use type Framework.Test_Count_Type;

      Child       : Framework.Test_Suite;
      Parent      : Framework.Test_Suite;
      Dummy_Test  : Dummy_Tests.Test;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Static_Test (Child, Dummy_Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Static_Test (Parent, Child);

      Assert_Eq_Count
        (Framework.Test_Count (Parent), Dummy_Test_Count, "Test Count");
   end Test_Test_Suite_Test_Static_Count;

   procedure Test_Test_Suite_Test_Name_Count is
      use Dummy_Tests;
      use type Framework.Test_Count_Type;

      Child       : Framework.Test_Suite;
      Parent      : Framework.Test_Suite;
      GParent     : Framework.Test_Suite;
      Dummy_Test  : Dummy_Tests.Test;
   begin
      Child := Framework.Create_Suite ("Child suite");
      Framework.Add_Static_Test (Child, Dummy_Test);

      Framework.Add_Test (Child, new Dummy_Tests.Test);

      Parent := Framework.Create_Suite ("Parent suite");
      Framework.Add_Static_Test (Parent, Child);

      GParent := Framework.Create_Suite ("GParent suite");
      Framework.Add_Static_Test (GParent, Parent);

      Assert_Eq_Count
        (Framework.Test_Count (GParent, "Failure"), 2, "Test Count");
      Assert_Eq_Count
        (Actual => Framework.Test_Count (GParent, "GParent suite"),
         Expected => Dummy_Test_Count * 2,
         Message => "GParent suite: Test Count");
   end Test_Test_Suite_Test_Name_Count;

   -- We test that Test_Suites do their cleanup properly
   -- even if we have mixed static and dynamic test cases
   -- or have nested test suites.
   procedure Test_Test_Suite_Cleanup is
      Initial_Count : Integer;
   begin
      Dummy_Tests.Reset_Instance_Count;
      Initial_Count := Dummy_Tests.Get_Instance_Count;
      --## rule off Long_Blocks
      declare
         use Dummy_Tests;

         Child       : Framework.Test_Suite;
         Parent      : Framework.Test_Suite;
         GParent     : Framework.Test_Suite;
         Dummy_Test  : Dummy_Tests.Test;

         Test_Instance_Count : Natural := 1;
      begin
         Child := Framework.Create_Suite ("Child suite");
         Framework.Add_Static_Test (Child, Dummy_Test);
         Test_Instance_Count := Test_Instance_Count + 1; -- 2
         Assert_Eq_Int (Expected => Test_Instance_Count,
                        Actual   => Dummy_Tests.Get_Instance_Count,
                        Message  => "Dummy_Tests instance count");

         Framework.Add_Test (Child, new Dummy_Tests.Test);
         Test_Instance_Count := Test_Instance_Count + 1; -- 3
         Assert_Eq_Int (Expected => Test_Instance_Count,
                        Actual   => Dummy_Tests.Get_Instance_Count,
                        Message  => "Dummy_Tests instance count");

         Parent := Framework.Create_Suite ("Parent suite");
         Framework.Add_Static_Test (Parent, Child);
         Test_Instance_Count := Test_Instance_Count + 2; -- 1 + 2 + 2 = 5
         Assert_Eq_Int (Expected => Test_Instance_Count,
                        Actual   => Dummy_Tests.Get_Instance_Count,
                        Message  => "Dummy_Tests instance count");

         Framework.Add_Test (Parent, new Dummy_Tests.Test);
         Framework.Add_Static_Test (Parent, Dummy_Test);

         GParent := Framework.Create_Suite ("GParent suite");
         Framework.Add_Test (GParent, new Dummy_Tests.Test);
         Framework.Add_Static_Test (GParent, Dummy_Test);
         Framework.Add_Static_Test (GParent, Parent);
      end;

      Assert_Eq_Int (Expected => Initial_Count,
                     Actual   => Dummy_Tests.Get_Instance_Count,
                     Message  => "Not all tests freed");
      --## rule on Long_Blocks
   end Test_Test_Suite_Cleanup;
end Framework_Tests;
