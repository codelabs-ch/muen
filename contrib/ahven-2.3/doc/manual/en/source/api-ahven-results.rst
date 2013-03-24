:mod:`Ahven.Results` -- Package
===============================

.. ada:module:: Ahven.Results
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

-----
Types
-----


Result_Info
'''''''''''

::

   type Result_Info is private;

A type which holds a test result for one test.


Result_Collection
'''''''''''''''''

::

   type Result_Collection is limited private;

A collection of Result_Info objects.
Contains also child collections.

Result_Collection_Access
''''''''''''''''''''''''

::

   type Result_Collection_Access is access Result_Collection;

Access type for Result_Collection.



Result_Info_Cursor
''''''''''''''''''

::

   type Result_Info_Cursor is private;

A cursor type for Pass, Failure and Error results.

Result_Collection_Cursor
''''''''''''''''''''''''

::

   type Result_Collection_Cursor is private;

Cursor for iterating over a set of Result_Collection access objects.


---------
Constants
---------

Empty_Result_Info
'''''''''''''''''

::

   Empty_Result_Info : constant Result_Info;

Result_Info object which holds no result. It can be used
to initialize a new Result_Info object.


------------------------
Procedures and Functions
------------------------

Set_Test_Name
'''''''''''''

::

   procedure Set_Test_Name (Info : in out Result_Info;
                            Name :        Bounded_String);

Set a test name for the result.

Set_Routine_Name
''''''''''''''''

::

   procedure Set_Routine_Name (Info : in out Result_Info;
                               Name :        Bounded_String);

Set a routine name for the result.

Set_Message
'''''''''''

::

   procedure Set_Message (Info : in out Result_Info;
                          Message : Bounded_String);

Set a message for the result.

Set_Test_Name
'''''''''''''

::

   procedure Set_Test_Name (Info : in out Result_Info; Name : String);

A helper function, which calls Set_Test_Name (.. ; Bounded_String).

Set_Routine_Name
''''''''''''''''

::

   procedure Set_Routine_Name (Info : in out Result_Info; Name : String);

A helper function, which calls Set_Routine_Name (.. ; Bounded_String).

Set_Message
'''''''''''

::

   procedure Set_Message (Info : in out Result_Info; Message : String);

A helper function, which calls Set_Message (.. ; Bounded_String).

Set_Long_Message
''''''''''''''''

::

   procedure Set_Long_Message (Info    : in out Result_Info;
                               Message :        Bounded_String);

Set a long message for the result.

Set_Long_Message
''''''''''''''''

::

   procedure Set_Long_Message (Info : in out Result_Info; Message : String);

A helper function, which calls Set_Long_Message (.. ; Bounded_String).

Set_Execution_Time
''''''''''''''''''

::

   procedure Set_Execution_Time (Info         : in out Result_Info;
                                 Elapsed_Time :        Duration);

Set the execution time of the result info (test).

Set_Output_File
'''''''''''''''

::

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        Bounded_String);

Set the name of the test output file.

Set_Output_File
'''''''''''''''

::

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        String);

Set the name of the test output file.

Get_Test_Name
'''''''''''''

::

   function Get_Test_Name (Info : Result_Info) return String;

Return the test name of the result info.

Get_Routine_Name
''''''''''''''''

::

   function Get_Routine_Name (Info : Result_Info) return String;

Return the routine name of the result info.

Get_Message
'''''''''''

::

   function Get_Message (Info : Result_Info) return String;

Return the message of the result info. The message will
be something what has been given to the Assert call.

Get_Long_Message
''''''''''''''''

::

   function Get_Long_Message (Info : Result_Info) return String;

Return the long message of the result info.
The long message usually contains the backtrace or other info
related to the test failure.

Get_Execution_Time
''''''''''''''''''

::

   function Get_Execution_Time (Info : Result_Info) return Duration;

Return the execution time of the result info.

Get_Output_File
'''''''''''''''

::

   function Get_Output_File (Info : Result_Info) return Bounded_String;

Return the name of the output file.
Empty string is returned in case there is no output file.

Add_Child
'''''''''

::

   procedure Add_Child (Collection : in out Result_Collection;
                        Child      :        Result_Collection_Access);

Add a child collection to the collection.
Ownership of Child is transferred to the Collection container
and Child will be automatically freed when Collection is destroyed.

Add_Error
'''''''''

::

   procedure Add_Error (Collection : in out Result_Collection;
                        Info       :        Result_Info);

Add a test error to the collection.

Add_Skipped
'''''''''''

::

   procedure Add_Skipped (Collection : in out Result_Collection;
                          Info       :        Result_Info);

Add a skipped test to the collection.

Add_Failure
'''''''''''

::

   procedure Add_Failure (Collection : in out Result_Collection;
                          Info       :        Result_Info);

Add a test failure to the collection.

Add_Pass
''''''''

::

   procedure Add_Pass (Collection : in out Result_Collection;
                       Info       :        Result_Info);

Add a passed test to the collection

Release
'''''''

::

   procedure Release (Collection : in out Result_Collection);

Release resourced held by the collection.
Frees also all children added via Add_Child.

Set_Name
''''''''

::

   procedure Set_Name (Collection : in out Result_Collection;
                       Name       :        Bounded_String);

Set a test name for the collection.

Set_Parent
''''''''''

::

   procedure Set_Parent (Collection : in out Result_Collection;
                         Parent     :        Result_Collection_Access);

Set a parent collection to the collection.

Test_Count
''''''''''

::

   function Test_Count (Collection : Result_Collection) return Natural;

Return the amount of tests in the collection.
Tests in child collections are included.

Direct_Test_Count
'''''''''''''''''

::

   function Direct_Test_Count (Collection : Result_Collection) return Natural;

Return the amount of tests in the collection.
The tests in the child collections are NOT included.

Pass_Count
''''''''''

::

   function Pass_Count (Collection : Result_Collection) return Natural;

Return the amount of passed tests in the collection.
Tests in child collections are included.

Error_Count
'''''''''''

::

   function Error_Count (Collection : Result_Collection) return Natural;

Return the amount of test errors in the collection.
Tests in child collections are included.

Failure_Count
'''''''''''''

::

   function Failure_Count (Collection : Result_Collection) return Natural;

Return the amount of test errors in the collection.
Tests in child collections are included.

Skipped_Count
'''''''''''''

::

   function Skipped_Count (Collection : Result_Collection) return Natural;

Return the amount of skipped tests in the collection.
Tests in child collections are included.

Get_Test_Name
'''''''''''''

::

   function Get_Test_Name (Collection : Result_Collection)
     return Bounded_String;

Return the name of the collection's test.

Get_Parent
''''''''''

::

   function Get_Parent (Collection : Result_Collection)
     return Result_Collection_Access;

Return the parent of the collection.

Get_Execution_Time
''''''''''''''''''

::

   function Get_Execution_Time (Collection : Result_Collection)
     return Duration;

Return the execution time of the whole collection.

First_Pass
''''''''''

::

   function First_Pass (Collection : Result_Collection)
     return Result_Info_Cursor;

Get the first pass from the collection.

First_Failure
'''''''''''''

::

   function First_Failure (Collection : Result_Collection)
     return Result_Info_Cursor;

Get the first failure from the collection.

First_Error
'''''''''''

::

   function First_Error (Collection : Result_Collection)
     return Result_Info_Cursor;

Get the first error from the collection.

Next
''''

::

   function Next (Position: Result_Info_Cursor) return Result_Info_Cursor;

Get the next pass/failure/error.

Data
''''

::

   function Data (Position: Result_Info_Cursor) return Result_Info;

Get the data behind the cursor.

Is_Valid
''''''''

::

   function Is_Valid (Position: Result_Info_Cursor) return Boolean;

Is the cursor still valid?

First_Child
'''''''''''

::

   function First_Child (Collection : in Result_Collection)
     return Result_Collection_Cursor;

Get the first child of the collection.

Next
''''

::

   function Next (Position: Result_Collection_Cursor)
     return Result_Collection_Cursor;

Get the next child.

Is_Valid
''''''''

::

   function Is_Valid (Position: Result_Collection_Cursor) return Boolean;

Is the cursor still valid?

Data
''''

::

   function Data (Position: Result_Collection_Cursor)
     return Result_Collection_Access;

Get the data (Result_Collection_Access) behind the cursor.

Child_Depth
'''''''''''

::

   function Child_Depth (Collection : Result_Collection) return Natural;

Return the maximum depth of children. (a child of a child, etc.)

