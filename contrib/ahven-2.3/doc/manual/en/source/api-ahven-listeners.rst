:mod:`Ahven.Listeners` -- Package
=================================

.. ada:module:: Ahven.Listeners
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>


-----
Types
-----

Test_Phase
''''''''''

::

   type Test_Phase is (TEST_BEGIN, TEST_RUN, TEST_END);

What is test doing right now?

Test_Type
'''''''''

::

   type Test_Type is (CONTAINER, ROUTINE);

Context
'''''''

::

   type Context (Phase : Test_Phase) is record
      Test_Name : AStrings.Bounded_String;
      Test_Kind : Test_Type;
      case Phase is
         when TEST_BEGIN | TEST_END =>
            null;
         when TEST_RUN =>
            Routine_Name : AStrings.Bounded_String;
            Message      : AStrings.Bounded_String;
            Long_Message : AStrings.Bounded_String;
      end case;
   end record;

Result_Listener
'''''''''''''''

::

   type Result_Listener is
     abstract new Ada.Finalization.Limited_Controlled with null record;

Result_Listener is a listener for test results.
Whenever a test is run, the framework calls
registered listeners and tells them the result of the test.

Result_Listener_Class_Access
''''''''''''''''''''''''''''

::

   type Result_Listener_Class_Access is access all Result_Listener'Class;


------------------------
Procedures and Functions
------------------------

Add_Pass
''''''''

::

   procedure Add_Pass (Listener : in out Result_Listener;
                       Info     :        Context) is abstract;

Called after test passes.

Add_Failure
'''''''''''

::

   procedure Add_Failure (Listener : in out Result_Listener;
                          Info     :        Context) is abstract;

Called after test fails.

Add_Error
'''''''''

::

   procedure Add_Error (Listener : in out Result_Listener;
                        Info     :        Context) is abstract;

Called after there is an error in the test.

Start_Test
''''''''''

::

   procedure Start_Test (Listener : in out Result_Listener;
                         Info     :        Context) is abstract;

Called before the test begins. This is called before Add_* procedures.

End_Test
''''''''

::

   procedure End_Test (Listener : in out Result_Listener;
                       Info     :        Context) is abstract;

Called after the test ends. Add_* procedures are called before this.

