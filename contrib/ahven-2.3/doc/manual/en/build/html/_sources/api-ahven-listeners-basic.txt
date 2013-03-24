:mod:`Ahven.Listeners.Basic` -- Package
=======================================

.. ada:module:: Ahven.Listeners.Basic
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

-----
Types
-----

Result_Type
'''''''''''

::

   type Result_Type is (NO_RESULT, PASS_RESULT, FAILURE_RESULT, ERROR_RESULT);


Basic_Listener
''''''''''''''

::

   type Basic_Listener is new Result_Listener with record
      Main_Result       : aliased Result_Collection;
      Current_Result    : Result_Collection_Access;
      Last_Test_Result  : Result_Type := NO_RESULT;
      Last_Info         : Result_Info := Empty_Result_Info;
      Capture_Output    : Boolean     := False;
      Output_File       : Temporary_Output.Temporary_File;
      Start_Time        : Ada.Calendar.Time;
   end record;


------------------------
Procedures and Functions
------------------------

Add_Pass
''''''''

::

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Info     :        Context);

New implementation for Listeners.Add_Pass

Add_Failure
'''''''''''

::

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Info     :        Context);

New implementation for Listeners.Add_Failure

Add_Error
'''''''''

::

   procedure Add_Error (Listener : in out Basic_Listener;
                        Info     :        Context);

New implementation for Listeners.Add_Error

Start_Test
''''''''''

::

   procedure Start_Test (Listener : in out Basic_Listener;
                         Info     :        Context);

New implementation for Listeners.Start_Test

End_Test
''''''''

::

   procedure End_Test (Listener : in out Basic_Listener;
                       Info     :        Context);

New implementation for Listeners.End_Test

Set_Output_Capture
''''''''''''''''''

::

   procedure Set_Output_Capture (Listener : in out Basic_Listener;
                                 Capture  :        Boolean);

Enable or disable Ada.Text_IO output capturing

Get_Output_Capture
''''''''''''''''''

::

   function Get_Output_Capture (Listener : Basic_Listener)
     return Boolean;

Capture the Ada.Text_IO output?

