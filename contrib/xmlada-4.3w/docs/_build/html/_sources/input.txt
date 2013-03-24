.. _The_Input_module:

****************
The Input module
****************

This module provides a set of packages with a common interface to access the
characters contained in a stream. Various implementations are provided to
access files and manipulate standard Ada strings.

A top-level tagged type is provided that must be extended for the various
streams. It is assumed that the pointer to the current character in the stream
can only go forward, and never backward. As a result, it is possible to
implement this package for sockets or other strings where it isn't even
possible to go backward. This also means that one doesn't have to provide
buffers in such cases, and thus that it is possible to provide memory-efficient
readers.

Two predefined readers are available, namely `String_Input` to read characters
from a standard Ada string, and `File_Input` to read characters from a standard
text file.

They all provide the following primite operations:

`Open`

  Although this operation isn't exactly overriden, since its parameters
  depend on the type of stream you want to read from, it is nice to
  use a standard name for this constructor.

`Close`
  This terminates the stream reader and free any associated memory. It
  is no longer possible to read from the stream afterwards.

`Next_Char`
  Return the next Unicode character in the stream. Note this character doesn't
  have to be associated specifically with a single byte, but that it depends on
  the encoding chosen for the stream (see the unicode module documentation for
  more information).

  The next time this function is called, it returns the following character
  from the stream.

`Eof`
  This function should return True when the reader has already returned the
  last character from the stream. Note that it is not guarantee that a second
  call to Eof will also return True.

It is the responsability of this stream reader to correctly call the decoding
functions in the unicode module so as to return one single valid unicode
character. No further processing is done on the result of `Next_Char`. Note
that the standard `File_Input` and `String_Input` streams can automatically
detect the encoding to use for a file, based on a header read directly from the
file.

Based on the first four bytes of the stream (assuming this is valid XML), they
will automatically detect whether the file was encoded as Utf8, Utf16,... If
you are writing your own input streams, consider adding this automatic
detection as well.

However, it is always possible to override the default through a call to
`Set_Encoding`. This allows you to specify both the character set (Latin1, ...)
and the character encoding scheme (Utf8,...).

The user is also encouraged to set the identifiers for the stream they are
parsing, through calls to `Set_System_Id` and `Set_Public_Id`. These are used
when reporting error messages.

