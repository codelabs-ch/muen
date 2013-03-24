:mod:`Ahven.Temporary_Output` -- Package
========================================

.. ada:module:: Ahven.Temporary_Output
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

-----
Types
-----

Temporary_File
''''''''''''''

::

   type Temporary_File is limited private;


------------------------
Procedures and Functions
------------------------

Create_Temp
'''''''''''

::

   procedure Create_Temp (File : out Temporary_File);

Create a new temporary file. Exception Temporary_File_Error
is raised if the procedure cannot create a new temp file.

Get_Name
''''''''

::

   function Get_Name (File : Temporary_File) return String;

Return the name of the file.

Redirect_Output
'''''''''''''''

::

   procedure Redirect_Output (To_File : in out Temporary_File);

Redirect the standard output to the file.
To_File must be opened using Create_Temp.

Restore_Output
''''''''''''''

::

   procedure Restore_Output;

Restore the standard output to its default settings.

Remove_Temp
'''''''''''

::

   procedure Remove_Temp (File : in out Temporary_File);

Remove the temporary file. File can be either open or closed.

Close_Temp
''''''''''

::

   procedure Close_Temp (File : in out Temporary_File);

Close the temporary file.

