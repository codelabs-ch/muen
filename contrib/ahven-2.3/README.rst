=========================================================
Ahven - Unit Testing Library for Ada Programming Language
=========================================================

.. contents::
   :depth: 1

Ahven is a simple unit test library (or a framework) for Ada programming language. It is loosely modelled after `JUnit`_ and some ideas are taken from AUnit.

Ahven is free software distributed under permissive ISC license and should work with any Ada 95 or 2005 compiler.


Features
--------

* Simple API
* Small size (Ahven 2.2 has 2.4K SLOC; 588 statements; 1228 declarations)
* JUnit-compatible test results in XML format;
  this allows integration with tools like `Jenkins`_ or CruiseControl.
* Strict coding style (enforced by AdaControl)
* Plain Ada 95 code, no Ada 2005 features used,
  but can be compiled as Ada 2005 code if needed
* Portable across different compilers and operating systems
* Permissive Open Source license (ISC)

See also
''''''''

* The project page at http://sourceforge.net/projects/ahven/
* Author's blog at http://tero.stronglytyped.org/tag/ahven.html

Platforms
---------

Ahven 2.3 compiles and passes its test suite on following platforms

+-----------------------+-------+-------------------------+
| OS                    |  Arch | Compiler                |
+=======================+========+========================+
| Fedora Linux 18       | x86_64 | FSF GCC 4.7.2          |
+-----------------------+--------+------------------------+
| Debian GNU/Linux 6.0  | i386   | FSF GCC 4.4            |
+-----------------------+--------+------------------------+
| Debian GNU/Linux 6.0  | x86_64 | FSF GCC 4.4            |
+-----------------------+--------+------------------------+
| Windows 7             | x86_64 | Janus/Ada 3.1.2beta    |
+-----------------------+--------+------------------------+
| Windows 7             | x86_64 | GNAT GPL 2012          |
+-----------------------+--------+------------------------+
| Wine 1.5 on Linux     | i386   | Janus/Ada 3.1.2beta    |
+-----------------------+--------+------------------------+
| Windows XP            | i386   | Irvine ICC Ada 9.0beta |
+-----------------------+--------+------------------------+

News
----

Ahven 2.3 (2013-01-24)
''''''''''''''''''''''

This is a minor feature release.

Starting from this release, the exception backtraces are now
stored to the test results and printed out along with the results.
In addition, the documentation received some improvements and
output of multiline messages from TAP_Runner was fixed.


Ahven 2.2 (2012-03-05)
''''''''''''''''''''''

This is a bug fix release.

The release fixes the reporting of skipped tests in Ahven.XML_Runner.
Also, support for GNAT 3.15p was removed. Documentation generation
tool was changed from Adabrowse to `Sphinx`_.


Ahven 2.1 (2011-09-24)
''''''''''''''''''''''

This is a bug fix release.

The release fixes the skipped test reporting in Ahven.Text_Runner.

Ahven 2.0 (2011-09-23)
''''''''''''''''''''''

This is a feature release.

The release adds possibility to stop tests after certain amount of time
and programmatically skip tests. In addition, the README document is
now in reStructured text format, like the manual.

Ahven 1.9 (2011-04-19)
''''''''''''''''''''''


This is a bug fix release.

The release includes new HTML documentation generated from reStructured text using Python-Sphinx and fixes compilation problems with GNAT GPL 2010.

Ahven 1.8 (2010-06-02)
''''''''''''''''''''''

This is a bug fix release.

Changes include a fix for double free when mixing dynamic test cases with static test suites, removal of some unfinished features like TAP 1.3 and Janus/Ada 3.1.1d support, and code cleanups.

Ahven website location changed again (2009-11-30)
'''''''''''''''''''''''''''''''''''''''''''''''''

The website location of Ahven changed once more. This time the change should be the last one for a while. At the same time, the layout was reverted to the older version, which is more friendly to the bandwidth.

Technical detail which should be interesting: The new website is running on Debian and Ada Web Server.

Ahven 1.7 (2009-09-14)
''''''''''''''''''''''

This is a bug fix release.

Changes include a fix for Constraint_Error with long test names and
special character filtering from the test names when generating XML results.
In addition, PDF report generation example was added to the contrib directory
and some internal code cleanups were done.

Mercurial repository, part 2 (2009-06-25)
'''''''''''''''''''''''''''''''''''''''''

Sourceforge.net has had some problems with their Mercurial repositories,
so now the previously unofficial Bitbucket Mercurial repository as
the official Mercurial repository for Ahven.

Also, bug reports are now at Bitbucket.

Mercurial repository (2009-03-17)
'''''''''''''''''''''''''''''''''

Sourceforge.net added support for Mercurial and now Ahven's source code repository is migrated from CVS to Mercurial.

Ahven 1.6 (2009-02-28)
''''''''''''''''''''''

This release fixes GNAT installation issues.

Ahven 1.5 (2009-02-23)
''''''''''''''''''''''

This is first release at SourceForge. The release includes only some build system changes.

SourceForge.net (2009-02-18)
''''''''''''''''''''''''''''

Ahven project is now hosted by SourceForge.

Ahven 1.4 (2009-01-22)
''''''''''''''''''''''

This release introduces Test Anything Protocol (TAP) reporter, a new API for stack-based test cases, and improved Janus/Ada support. Also, some API changes were done, but they should affect you only if you have extented the framework.

Ahven 1.3 (2008-08-13)
''''''''''''''''''''''

A bug fix release. The major change is support for Janus/Ada.
Web site layout changes (2008-06-30)

The web site layout was changed to be "less boring". The new blueish theme should work better on different types of monitors. (Some low quality monitors and graphics cards didn't show light brown colors properly.)

Ahven 1.2 (2008-05-12)
''''''''''''''''''''''

A major new feature in this release is support for JUnit-compatible XML-based test result format. The release also includes bug fixes and code cleanups.

Ahven 1.1 (2008-01-30)
''''''''''''''''''''''

Incremental release including bug fixes and new features.

Ahven 1.0 (2007-10-24)
''''''''''''''''''''''

Initial release. (See `News`_ for details.) 


Download
--------

Ahven is distributed in source code format only.
Please see the download page at
http://sourceforge.net/projects/ahven/files/ for details.

You can download the latest development source code from
Ahven's Mercurial repository:
https://bitbucket.org/tkoskine/ahven/

Debian package
''''''''''''''

Debian stable (6.0) provides Ahven 1.7 as libahven17.0 and libahven1-dev packages.

One can install the packages with command *apt-get install libahven17 libahven-dev*.

Installation
------------

For building Ahven source code you need Ada 95
compiler, for example GNAT, Janus/Ada, or ObjectAda.

Optionally, you need AdaBrowse to build the documentation
and AdaControl to run coding style checks.

The default Makefile compiles code using gnatmake.
Internally, gnatmake is given a GNAT project file,
which works with GNAT GPL series and relatively
recent FSF GNAT. If you plan to compile Ahven
with GNAT 3.15p, you need to modify the project
file slightly and remove incompatible compiler flags.

If you use another compiler, you need to customize
the Makefile by yourself. Please note, that 'src'
directory has platform specific subdirectories 'unix'
and 'windows. You need to select the sources from one
of them also.

Installation: GNAT
''''''''''''''''''

When using GNAT, simple *make* will compile the library
and the unit tests.

Command *make check* will run the unit tests.

If you want to build the API documentation, you
need AdaBrowse tool. Command 'make docs' will
build the API documentation.

Installation happens by typing *make install*
or *make PREFIX=/my/ada/code install*. Alternatively,
you can simply copy the source code directory ('src')
to your project.

Installation: Janus/Ada
'''''''''''''''''''''''

Build scripts for Janus/Ada are located in the 'janusada' directory.
To compile the source code, you need to tweak file 'prepare.bat'
and then run 'prepare.bat', 'update.bat', and 'compile.bat' from
the top level directory. That is the same directory where this README.rst
file is located.

Example:

::

  janusada\prepare.bat
  janusada\update.bat
  janusada\compile.bat
  
When compilation is finished, you have tap_test.exe in the 'test_obj'
directory.

Documentation
-------------

* The API documentation (for Ahven 2.1):
  http://ahven.stronglytyped.org/api-2.1/index.html
* The API documentation (for Ahven 1.8):
  http://ahven.stronglytyped.org/api/index.html
* Tutorial:
  http://ahven.stronglytyped.org/tutorial.html

Author
------

Tero Koskinen <tero.koskinen@iki.fi>

.. image:: http://ahven.stronglytyped.org/ahven.png

.. _`Jenkins`: http://www.jenkins-ci.org/
.. _`JUnit`: http://www.junit.org/
.. _`News`: http://ahven.stronglytyped.org/NEWS
.. _`Sphinx`: http://sphinx.pocoo.org/
