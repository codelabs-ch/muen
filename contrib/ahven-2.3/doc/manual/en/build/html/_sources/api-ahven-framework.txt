:mod:`Ahven.Framework` -- Package
=================================

.. ada:module:: Ahven.Framework
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>
.. highlight:: ada


-----
Types
-----

Test_Count_Type
'''''''''''''''

.. .. xada:type:: type Test_Count_Type is new Natural;

::

     type Test_Count_Type is new Natural;

Type for the test count. This effectively
limits the amount tests to whatever Natural is.

Although, in practice when adding tests the limit
is not checked.

Test_Duration
'''''''''''''

::

     subtype Test_Duration is Duration range 0.0 .. Three_Hours;

Subtype for the test timeouts. Limited to 3 hours, which
should be enough for unit tests. Timeout value 0.0 means infinite time.


Test
''''

::

    type Test is abstract new Ada.Finalization.Controlled with null record;

.. .. xada:type:: type Test is abstract new Ada.Finalization.Controlled with null record;

   A type, which provides the base for Test_Case and Test_Suite types.

Test_Class_Access
'''''''''''''''''

::

    type Test_Class_Access is access all Test'Class;

An access type for Test'Class.

Test_Case
'''''''''

::

    type Test_Case is abstract new Test with private;

The base type for other test cases.

Object_Test_Routine_Access
''''''''''''''''''''''''''

::

    type Object_Test_Routine_Access is
      access procedure (T : in out Test_Case'Class);

A pointer to a test routine which takes Test_Case'Class object
as an argument.

For this kind of test routines, the framework will
call Set_Up and Tear_Down routines before and after
test routine execution.

Simple_Test_Routine_Access
''''''''''''''''''''''''''

::

    type Simple_Test_Routine_Access is access procedure;

A pointer to a test routine which does not take arguments.

Test_Suite
''''''''''

::

   type Test_Suite is new Test with private;

A collection of Tests.

You can either fill a Test_Suite object with Test_Case objects
or nest multiple Test_Suite objects. You can even mix
Test_Case and Test_Suite objects, if necessary.

Test_Suite_Access
'''''''''''''''''

::

   type Test_Suite_Access is access all Test_Suite;

An access type for Test_Suite.



------------------------
Procedures and functions
------------------------

Set_Up
''''''

::

   procedure Set_Up (T : in out Test);

.. .. xada:procedure:: procedure Set_Up (T : in out Test);

   Set_Up is called before executing the test procedure.

   :param T: Test to be set up.

Tear_Down
'''''''''

::

   procedure Tear_Down (T : in out Test);

Tear_Down is called after the test procedure is executed.

Get_Name
''''''''

::

   function Get_Name (T : Test) return String is abstract;
   
.. .. xada:function:: function Get_Name (T : Test) return String is abstract;

   Return the name of the test.

   :param T: The test object.

Run
'''

::

   procedure Run (T         : in out Test;
                  Listener  : in out Listeners.Result_Listener'Class);
   
.. .. xada:procedure:: procedure Run (T : in out Test; Listener : in out Listeners.Result_Listener'Class);

   Run the test and place the test result to Result. Infinite timeout.

   :param T: The test object to run.
   :param Listener: The listener which will be called during the test execution.

Run
'''

::

   procedure Run (T         : in out Test;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration)
     is abstract;
   
.. .. xada:procedure:: procedure Run (T : in out Test; Listener : in out Listeners.Result_Listener'Class);

   Run the test and place the test result to Result.

   :param T: The test object to run.
   :param Listener: The listener which will be called during the test execution.
   :param Timeout: Time limit for the test.


Run
'''

::

   procedure Run (T         : in out Test;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class);

Run the test with given name and place the test result to Result.
Notice: If multiple tests have same name this might call all of
them.


Run
'''

::

   procedure Run (T         : in out Test;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration)
     is abstract;

Run the test with given name and place the test result to Result.
Notice: If multiple tests have same name this might call all of
them. Timeout specifies maximum execution time for the tests.


   :param T: The test object to run.
   :param Test_Name: The name of the test which will be run.
   :param Listener: The listener which will be called during the test execution.
   :param Timeout: Time limit for the test.


Test_Count
''''''''''

::

   function Test_Count (T : Test) return Test_Count_Type is abstract;

Return the amount of tests (test routines) which will be executed when
the Run (T) procedure is called.

Test_Count
''''''''''

::

   function Test_Count (T : Test; Test_Name : String)
     return Test_Count_Type is abstract;

Return the amount of tests (test routines) which will be executed when
the Run (T, Test_Name) procedure is called.

Execute
'''''''

::

   procedure Execute (T        : in out Test'Class;
                      Listener : in out Listeners.Result_Listener'Class);

Call Test class' Run method and place the test outcome to Result.
The procedure calls Start_Test of every listener before calling
the Run procedure and End_Test after calling the Run procedure.

Execute
'''''''

::

   procedure Execute (T        : in out Test'Class;
                      Listener : in out Listeners.Result_Listener'Class;
                      Timeout  :        Test_Duration);

Call Test class' Run method and place the test outcome to Result.
The procedure calls Start_Test of every listener before calling
the Run procedure and End_Test after calling the Run procedure.
Timeout specifies the maximum execution time for a test.


Execute
'''''''

::

   procedure Execute (T         : in out Test'Class;
                      Test_Name :        String;
                      Listener  : in out Listeners.Result_Listener'Class);

Same as Execute above, but call the Run procedure which
takes Test_Name parameter.

Execute
'''''''

::

   procedure Execute (T         : in out Test'Class;
                      Test_Name :        String;
                      Listener  : in out Listeners.Result_Listener'Class;
                      Timeout  :        Test_Duration);

Same as Execute above, but call the Run procedure which
takes Test_Name parameter. Timeout specifies the maximum execution
time for a test.


Get_Name
''''''''

::

   function Get_Name (T : Test_Case) return String;

Return the name of the test case.

Run
'''

::

   procedure Run (T        : in out Test_Case;
                  Listener : in out Listeners.Result_Listener'Class);

Run Test_Case's test routines.

Run
'''

::

   procedure Run (T        : in out Test_Case;
                  Listener : in out Listeners.Result_Listener'Class;
                  Timeout  :        Test_Duration);

Run Test_Case's test routines with timeout value.


Run
'''

::

   procedure Run (T         : in out Test_Case;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class);

Run Test_Case's test routine which matches to the Name.

Run
'''

::

   procedure Run (T         : in out Test_Case;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout   :        Test_Duration);

Run Test_Case's test routine which matches to the Name, with timeout value.


Test_Count
''''''''''

::

   function Test_Count (T : Test_Case) return Test_Count_Type;

Implementation of Test_Count (T : Test).

Test_Count
''''''''''

::

   function Test_Count (T : Test_Case; Test_Name : String)
     return Test_Count_Type;

Implementation of Test_Count (T : Test, Test_Name : String).

Finalize
''''''''

::

   procedure Finalize (T : in out Test_Case);

Finalize procedure of the Test_Case.

Set_Name
''''''''

::

   procedure Set_Name (T : in out Test_Case; Name : String);

Set Test_Case's name.

Add_Test_Routine
''''''''''''''''

::

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Object_Test_Routine_Access;
                               Name    :        String);

Register a test routine to the Test_Case object.

Add_Test_Routine
''''''''''''''''

::

   procedure Add_Test_Routine (T       : in out Test_Case'Class;
                               Routine :        Simple_Test_Routine_Access;
                               Name    :        String);

Register a simple test routine to the Test_Case.

Create_Suite
''''''''''''

::

   function Create_Suite (Suite_Name : String)
     return Test_Suite_Access;

Create a new Test_Suite.
Caller must free the returned Test_Suite using Release_Suite.

Create_Suite
''''''''''''

::

   function Create_Suite (Suite_Name : String)
     return Test_Suite;

Create a new Test_Suite. The suite and its children are
released automatically.

Add_Test
''''''''

::

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Class_Access);

Add a Test to the suite. The suite frees the Test automatically
when it is no longer needed.

Add_Test
''''''''

::

   procedure Add_Test (Suite : in out Test_Suite; T : Test_Suite_Access);

Add a Test suite to the suite. The suite frees the Test automatically
when it is no longer needed.

Add_Static_Test
'''''''''''''''

::

   procedure Add_Static_Test
     (Suite : in out Test_Suite; T : Test'Class);

Add a Test to the suite. This procedure is meant for statically
allocated Test_Case objects.

Get_Name
''''''''

::

   function Get_Name (T : Test_Suite) return String;

Return the name of Test_Suite.

Run
'''

::

   procedure Run (T        : in out Test_Suite;
                  Listener : in out Listeners.Result_Listener'Class);

Run Test_Suite's Test_Cases.

Run
'''

::

   procedure Run (T        : in out Test_Suite;
                  Listener : in out Listeners.Result_Listener'Class;
                  Timeout  :        Test_Duration);

Run Test_Suite's Test_Cases with timeout value.


Run
'''

::

   procedure Run (T         : in out Test_Suite;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class);

Run test suite's child which matches to the given name.

Run
'''

::

   procedure Run (T         : in out Test_Suite;
                  Test_Name :        String;
                  Listener  : in out Listeners.Result_Listener'Class;
                  Timeout  :        Test_Duration);

Run test suite's child which matches to the given name, with timeout value.


Test_Count
''''''''''

::

   function Test_Count (T : Test_Suite) return Test_Count_Type;

Implementation of Test_Count (T : Test).

Test_Count
''''''''''

::

   function Test_Count (T : Test_Suite; Test_Name : String)
     return Test_Count_Type;

Implementation of Test_Count (T : Test, Test_Name : String).

Adjust
''''''

::

   procedure Adjust (T : in out Test_Suite);

Adjust procedure of Test_Suite.
Handles the copying of the structure properly

Finalize
''''''''

::

   procedure Finalize (T : in out Test_Suite);

Finalize procedure of Test_Suite. Frees all added Tests.

Release_Suite
'''''''''''''

::

   procedure Release_Suite (T : Test_Suite_Access);

Release the memory of Test_Suite.
All added tests are released automatically.

