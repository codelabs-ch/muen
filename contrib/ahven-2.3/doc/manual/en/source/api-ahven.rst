:mod:`Ahven` -- Package
=======================

.. ada:module:: Ahven
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

----------
Exceptions
----------

.. _assertion-error: ahven-assertion_error

**Assertion_Error**

    Exception, raised when Assert fails.

------------------------
Procedures and Functions
------------------------

.. _ahven-assert:

Assert
''''''

::

    procedure Assert (Condition : Boolean; Message : String);

If Condition is false, Assert raises Assertion_Error
with given Message.

.. _ahven-assert_equal:

Assert_Equal
''''''''''''

.. versionadded:: 1.4


::

    generic
       type Data_Type is private;
       with function Image (Item : Data_Type) return String is <>;
    procedure Assert_Equal (Actual : Data_Type; Expected : Data_Type; Message : String);

If Expected /= Actual, Assert raises Assertion_Error
with given Message. Function Image is used to convert
Actual and Expected parameters into String.

Example::

    declare
       procedure Assert_Eq_Nat is
         new Ahven.Assert_Equal (Data_Type => Natural,
                                 Image     => Natural'Image);

    begin
       Assert_Eq_Nat (Actual   => Test_Count,
                      Expected => 4,
		      "test count");
    end;

.. _ahven-fail:

Fail
''''

::

    procedure Fail (Message : String);

Fail always raises Assertion_Error with given Message.

.. _ahven-skip:

Skip
''''

.. versionadded:: 2.0

::

    procedure Skip (Message : String);

Skip always raises Test_Skipped_Error with given Message.
In practice, this means that the execution of tests stops
there and the test is marked as 'skipped'.
