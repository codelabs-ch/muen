.. _Using_the_library:

*****************
Using the library
*****************

XML/Ada is a library. When compiling an application that uses it, you
thus need to specify where the spec files are to be found, as well
as where the libraries are installed.

There are several ways to do it:

.. index:: xmlada-config

* The simplest is to use the *xmlada-config* script, and let it
  provide the list of switches for *gnatmake*. This is more
  convenient on Unix systems, where you can simply compile your application
  with::

    gnatmake main.adb `xmlada-config`
    
  Note the use of backticks. This means that *xmlada-config* is
  first executed, and then the command line is replaced with the output of
  the script, thus finally executing something like::

    gnatmake main.adb -Iprefix/include/xmlada -largs -Lprefix/lib \\
      -lxmlada_input_sources -lxmlada_sax -lxmlada_unicode -lxmlada_dom
    

  Unfortunately, this behavior is not available on Windows (unless of course
  you use a Unix shell). The simplest in that case is to create a
  :file:`Makefile`, to be used with the *make* command, and copy-paste
  the output of *xmlada-config* into it.

  *xmlada-config* has several switches that might be useful:


  * *--sax*: If you this flag, your application will not be
    linked against the DOM module. This might save some space, particularly
    if linking statically.

  * *--static*: Return the list of flags to use to link your
    application statically against Xml/Ada. Your application is then
    standalone, and you don't need to distribute XMl/Ada at the same time.

  * *--static_sax*: Combines both of the above flags.


  If you are working on a big project, particularly one that includes
  sources in languages other than Ada, you generally have to run the three
  steps of the compilation process separately (compile, bind and then link).
  *xmlada-config* can also be used, provided you use one of the
  following switches:


  * *--cflags*: This returns the compiler flags only, to be used
    for instance with *gcc*.

  * *--libs*: This returns the linker flags only, to be used for
    instance with *gnatlink*.


  This *xmlada-config* method doesn't provide access to the
  :file:`xml_gtk` module, which is only available when using project files
  (see below).

  .. index:: project files
  .. index:: xmlada.gpr

* The preferred method, however, is to use the GNAT project files.
  See the GNAT user's guide for more information on the project files and
  how to create them for your application.

  Basically, a project file contains the description of your build
  environment (source directories, object directories, libraries,...).

  The very simple case is when you have all your sources in the same
  directory (say :file:`src/`), and the object files are all generated in the
  :file:`obj/` directory.

  .. highlight:: ada

  In this case, your project file would look like::

       with "xmlada";
       project Default is
          for Source_Dirs use ("src/");
          for Object_Dir use "obj/";
       end Default;

  and you build your application with::

       gnatmake -Pdefault main.adb

  Note in the project file the first line, which indicates that your
  application requires XML/Ada to build. This will automatically set the
  appropriate compiler and linker switches to use XML/Ada. Your application
  will be linker against all modules of XML/Ada (DOM, SAX, ...).

  If your application doesn't use DOM, you can replace the first line with
  something like::

       with "xmlada_sax";

  which will reduce the number of libraries that your application is
  linked with.

  WHen you are using project files, you need to let GNAT know where to find
  the project files. This is done by setting the `ADA_PROJECT_PATH`
  environment variable, by adding to it the installation directory of
  XML/Ada, ie the one that contains xmlada.gpr

  If the installation prefix is the same as your GNAT installation, and you
  are using GNAT more recent than 5.03a, then it will automatically find
  XML/Ada's project files.

  Check the :file:`dom/test` directory in the XML/Ada package, which contains
  both code examples and project files that you can use as a basic for your
  own code.

The default type of library depends on the way you installed XML/Ada. In all
cases, and assuming you installed both static and shared libraries, you can
choose among the two by setting the environment variable::

  LIBRARY_TYPE=static

or::

  LIBRARY_TYPE=relocatable

Whatever method you used to build your application, you might have to change,
at least one UNIX systems, the environment variable `LD_LIBRARY_PATH` so that
it contains the :file:`lib/` directory in the XML/Ada installation, so that the
dynamic libraries are correctly found.

This is not needed if you build XML/Ada as a static directory.

Running on VxWorks
==================

On VxWorks, XML Ada processing might require more stack space than what is
typically available from the VxWorks shell, the tasks spawned from there with
"sp", or Ada tasks with no or a too small Storage_Size value attached.

Such stack overflow conditions are typically characterized by non-deterministic
erratic behavior and can be cured by allocating more stack space for the tasks
involved.

