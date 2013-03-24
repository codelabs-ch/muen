:mod:`Ahven.XML_Runner` -- Package
==================================

.. ada:module:: Ahven.XML_Runner
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

.. versionadded:: 1.2


------------------------
Procedures and Functions
------------------------


Run
'''

::

   procedure Run (Suite : in out Framework.Test_Suite'Class);

Run the suite and write the results to a file.

Run
'''

::

   procedure Run (Suite : Framework.Test_Suite_Access);

Run the suite and write the results to a file. The routine is
identical to the Run (Suite : in out Framework.Test_Suite'Class) procedure,
but takes an access parameter to a test suite.

Report_Results
''''''''''''''

::

   procedure Report_Results (Result : Results.Result_Collection;
                             Dir    : String);

Write the test results to the given directory. This is called
automatically during the execution of either of Run procedures.

