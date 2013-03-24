--
-- Copyright (c) 2007-2009 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Finalization;

with Ahven;
with Ahven.Listeners;
with Ahven.SList;
with Ahven.AStrings;

pragma Elaborate_All (Ahven);
pragma Elaborate_All (Ahven.SList);

package Ahven.Framework is

   Three_Hours : constant := 10800.0;

   subtype Test_Duration is Duration range 0.0 .. Three_Hours;

   type Test_Count_Type is new Natural;
   -- Type for the test count. This effectively
   -- limits the amount tests to whatever Natural is.
   --
   -- Although, in practice when adding tests the limit
   -- is not checked.

   type Test is abstract new Ada.Finalization.Controlled with null record;
   -- A type, which provides the base for Test_Case and
   -- Test_Suite types.

   type Test_Class_Access is access all Test'Class;

   procedure Set_Up (T : in out Test);
   -- Set_Up is called before executing the test procedure.
   --
   -- By default, the procedure does nothing, but derived
   -- types can overwrite this method and add their own
   -- customisations.
   --
   -- One should not call this explicitly by herself.
   -- The framework calls it when necessary.

   procedure Tear_Down (T : in out Test);
   -- Tear_Down is called after the test procedure is executed.
   --
   -- By default, the procedure does nothing, but derived
   -- types can overwrite this method and add their own
   -- customisations.
   --
   -- One should not call this explicitly by herself.
   -- The framework calls it when necessary.

   function Get_Name (T : Test) return String is abstract;
   -- Return the name of the test.
   --
   -- Types derived from the Test type are required to overwrite
   -- this procedure.

   procedure Run (T         : in out Test;
                  Listener  : in out Listeners.Result_Listener'Class);
   -- Run the test and place the test result to Result.
   --
   -- Calls Run (T, Listener, Timeout) with Timeout value 0.0.

   procedure Run (T         : in out Test;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration)
     is abstract;
   -- Run the test and place the test result to Result.
   -- Timeout specifies the maximum runtime for a single test.
   --
   -- Types derived from the Test type are required to overwrite
   -- this procedure.

   procedure Run (T         : in out Test;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class);
   -- Run the test and place the test result to Result.
   --
   -- Calls Run (T, Test_Name, Listener, Timeout) with Timeout value 0.0.

   procedure Run (T         : in out Test;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration)
     is abstract;
   -- Run the test with given name and place the test result to Result.
   -- Timeout specifies the maximum runtime for a single test.
   -- Notice: If multiple tests have same name this might call all of
   -- them.
   --
   -- Types derived from the Test type are required to overwrite
   -- this procedure.

   function Test_Count (T : Test) return Test_Count_Type is abstract;
   -- Return the amount of tests (test routines) which will be executed when
   -- the Run (T) procedure is called.

   function Test_Count (T : Test; Test_Name : String)
     return Test_Count_Type is abstract;
   -- Return the amount of tests (test routines) which will be executed when
   -- the Run (T, Test_Name) procedure is called.

   procedure Execute (T        : in out Test'Class;
                      Listener : in out Listeners.Result_Listener'Class;
                      Timeout  :        Test_Duration);
   -- Call Test class' Run method and place the test outcome to Result.
   -- The procedure calls Start_Test of every listener before calling
   -- the Run procedure and End_Test after calling the Run procedure.
   --
   -- This procedure is meant to be called from Runner package(s).
   -- There should be no need for other to use this.

   procedure Execute (T         : in out Test'Class;
                      Test_Name :        String;
                      Listener  : in out Listeners.Result_Listener'Class;
                      Timeout   :        Test_Duration);
   -- Same as Execute above, but call the Run procedure which
   -- takes Test_Name parameter.

   type Test_Case is abstract new Test with private;
   -- The base type for other test cases.

   function Get_Name (T : Test_Case) return String;
   -- Return the name of the test case.

   procedure Run (T        : in out Test_Case;
                  Listener : in out Listeners.Result_Listener'Class;
                  Timeout  :        Test_Duration);
   -- Run Test_Case's test routines.

   procedure Run (T         : in out Test_Case;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration);
   -- Run Test_Case's test routine which matches to the Name.

   function Test_Count (T : Test_Case) return Test_Count_Type;
   -- Implementation of Test_Count (T : Test).

   function Test_Count (T : Test_Case; Test_Name : String)
     return Test_Count_Type;
   -- Implementation of Test_Count (T : Test, Test_Name : String).

   procedure Finalize (T : in out Test_Case);
   -- Finalize procedure of the Test_Case.

   procedure Set_Name (T : in out Test_Case; Name : String);
   -- Set Test_Case's name.
   --
   -- If longer than 160 characters, the name is truncated
   -- to 160 characters.

   type Object_Test_Routine_Access is
     access procedure (T : in out Test_Case'Class);
   -- A pointer to a test routine which takes Test_Case'Class object
   -- as an argument.
   --
   -- For this kind of test routines, the framework will
   -- call Set_Up and Tear_Down routines before and after
   -- test routine execution.

   type Simple_Test_Routine_Access is access procedure;
   -- A pointer to a test routine which does not take arguments.

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Object_Test_Routine_Access;
                               Name    :        String);
   -- Register a test routine to the Test_Case object.
   --
   -- The routine must have signature
   --  "procedure R (T : in out Test_Case'Class)".

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Simple_Test_Routine_Access;
                               Name    :        String);
   -- Register a simple test routine to the Test_Case.
   --
   -- The routine must have signature
   --  "procedure R".

   type Test_Suite is new Test with private;
   -- A collection of Tests.
   --
   -- You can either fill a Test_Suite object with Test_Case objects
   -- or nest multiple Test_Suite objects. You can even mix
   -- Test_Case and Test_Suite objects, if necessary.

   type Test_Suite_Access is access all Test_Suite;

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access;
   -- Create a new Test_Suite.
   -- Caller must free the returned Test_Suite using Release_Suite.

   function Create_Suite (Suite_Name : String)
     return Test_Suite;
   -- Create a new Test_Suite. The suite and its children are
   -- released automatically.

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access);
   -- Add a Test to the suite. The suite frees the Test automatically
   -- when it is no longer needed.

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Suite_Access);
   -- Add a Test suite to the suite. The suite frees the Test automatically
   -- when it is no longer needed.
   --
   -- This is a helper function, which internally calls
   -- Add_Test (Suite : in out Test_Suite; T : Test_Class_Access).

   procedure Add_Static_Test
     (Suite : in out Test_Suite; T : Test'Class);
   -- Add a Test to the suite. This procedure is meant for statically
   -- allocated Test_Case objects.
   --
   -- Please note, that a copy of the Test'Class object is saved to
   -- the suite. Original test object is not modified and changes
   -- made to it after adding the test are not propagated to
   -- the added object.

   function Get_Name (T : Test_Suite) return String;
   -- Return the name of Test_Suite.

   procedure Run (T      : in out Test_Suite;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration);
   -- Run Test_Suite's Test_Cases.

   procedure Run (T         : in out Test_Suite;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration);
   -- Run test suite's child which matches to the given name.

   function Test_Count (T : Test_Suite) return Test_Count_Type;
   -- Implementation of Test_Count (T : Test).

   function Test_Count (T : Test_Suite; Test_Name : String)
     return Test_Count_Type;
   -- Implementation of Test_Count (T : Test, Test_Name : String).

   procedure Adjust (T : in out Test_Suite);
   -- Adjust procedure of Test_Suite.
   -- Handles the copying of the structure properly

   procedure Finalize (T : in out Test_Suite);
   -- Finalize procedure of Test_Suite. Frees all added Tests.

   procedure Release_Suite (T : Test_Suite_Access);
   -- Release the memory of Test_Suite.
   -- All added tests are released automatically.

private
   type Command_Object_Enum is (SIMPLE, OBJECT);

   type Test_Command (Command_Kind : Command_Object_Enum := SIMPLE) is record
      Name : AStrings.Bounded_String;
      case Command_Kind is
         when SIMPLE =>
            Simple_Routine : Simple_Test_Routine_Access;
         when OBJECT =>
            Object_Routine : Object_Test_Routine_Access;
      end case;
   end record;
   -- Name attribute tells the name of the test routine.

   procedure Run (Command : Test_Command; T : in out Test_Case'Class);
   -- Run the specified command.
   -- Calls Set_Up and Tear_Down if necessary.

   package Test_Command_List is
     new Ahven.SList (Element_Type => Test_Command);

   type Test_Case is abstract new Test with record
      Routines : Test_Command_List.List  := Test_Command_List.Empty_List;
      Name     : AStrings.Bounded_String := AStrings.Null_Bounded_String;
   end record;
   -- Our test case type. It holds a list of test routines
   -- (test command objects) and the name of the test case.

   procedure Run_Command (Command  :        Test_Command;
                          Info     :        Listeners.Context;
                          Timeout  :        Test_Duration;
                          Listener : in out Listeners.Result_Listener'Class;
                          T        : in out Test_Case'Class);
   -- Handle dispatching to the right Run (Command : Test_Command)
   -- procedure and record test routine result to the Result object.
   --
   -- Timeout parameter defines the longest time the test is allowed
   -- to run. Value 0.0 means infinite time.

   type Test_Class_Wrapper is record
      Ptr : Test_Class_Access;
   end record;

   package Test_List is
     new Ahven.SList (Element_Type => Test_Class_Wrapper);

   package Indefinite_Test_List is
      type List is new Ada.Finalization.Controlled with private;

      Empty_List : constant List;

      procedure Append (Target : in out List; Node_Data : Test'Class);
      -- Append an element at the end of the list.

      procedure Clear (Target : in out List);
      -- Remove all elements from the list.

      generic
         with procedure Action (T : in out Test'Class) is <>;
      procedure For_Each (Target : List);
      -- A generic procedure for walk through every item
      -- in the list and call Action procedure for them.

   private
      type Node;
      type Node_Access is access Node;

      procedure Remove (Ptr : Node_Access);
      -- A procedure to release memory pointed by Ptr.

      type Node is record
         Next : Node_Access       := null;
         Data : Test_Class_Access := null;
      end record;

      type List is new Ada.Finalization.Controlled with record
         First : Node_Access := null;
         Last  : Node_Access := null;
      end record;

      procedure Initialize (Target : in out List);
      procedure Finalize   (Target : in out List);
      procedure Adjust     (Target : in out List);

      Empty_List : constant List :=
        (Ada.Finalization.Controlled with First => null,
                                          Last  => null);
   end Indefinite_Test_List;

   type Test_Suite is new Test with record
      Suite_Name : AStrings.Bounded_String := AStrings.Null_Bounded_String;
      Test_Cases : Test_List.List := Test_List.Empty_List;
      Static_Test_Cases : Indefinite_Test_List.List :=
        Indefinite_Test_List.Empty_List;
   end record;
   -- A suite type which holds a list of test cases and the name
   -- of the suite.

end Ahven.Framework;
