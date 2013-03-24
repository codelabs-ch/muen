------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  <description>
--  This package provides a hierarchy of objects that return characters
--  that can then be used for different tasks.
--  It is not possible to go backward, nor to previous characters. This
--  interface is intentionally kept minimal, so that it can easily be used
--  with files, sockets, ...
--
--  Input sources should try to automatically detect the appropriate encoding
--  to use, for instance by using the byte order mark, if present, of the
--  unicode stream (16#FFFE# or 16#FEFF#).
--  </description>

with Unicode;
with Unicode.CES;
with Unicode.CES.Basic_8bit;
with Unicode.CCS;

package Input_Sources is

   type Input_Source is abstract tagged limited private;
   --  General object for reading characters, one at a time.

   type Input_Source_Access is access all Input_Source'Class;

   procedure Next_Char
     (From : in out Input_Source;
      C    : out Unicode.Unicode_Char) is abstract;
   --  Return a single character from From.
   --  This also increments the internal index, so that the nex time this
   --  function is called the next character in the stream is returned.

   function Eof (From : Input_Source) return Boolean is abstract;
   --  Return True if there is no more character to read on the stream

   function Prolog_Size (From : Input_Source) return Natural;
   --  Return the number of characters that were ignored at the beginning
   --  of the stream (for instance because they indicated the encoding used
   --  in the file).

   procedure Set_Encoding
     (Input : in out Input_Source;
      Es    : Unicode.CES.Encoding_Scheme);
   --  Set the encoding associated with the input stream.
   --  This can be used to convert from any type of encoding for the byte
   --  sequence (Utf8, Utf16, ..) and any character set (Latin-1, Unicode,..)
   --  to unicode characters.
   --  Input_Sources are encouraged to guess the encoding whenever possible,
   --  but you can override that default at any time.

   function Get_Encoding (Input : Input_Source)
      return Unicode.CES.Encoding_Scheme;
   --  Return the encoding scheme associated with the input

   procedure Set_Character_Set
     (Input : in out Input_Source;
      Cs    : Unicode.CCS.Character_Set);
   --  Set the character set associated with the stream.
   --  It isn't possible to get the character set automatically for a stream.
   --  As a result, the default one is always considered to be Unicode

   function Get_Character_Set (Input : Input_Source)
      return Unicode.CCS.Character_Set;
   --  Return the character set associated with the input.

   procedure Set_Stream_Encoding
     (Input    : in out Input_Sources.Input_Source'Class;
      Encoding : String);
   --  Set the encoding and the character set for the stream associated with
   --  Parser.
   --  Invalid_Encoding is raised if Encoding is unknown.
   --  Encoding should have the form given in an XML file in the "encoding="
   --  parameter, for instance "UTF-8", "UTF-16", "ISO-8859-1",...

   procedure Set_System_Id
     (Input : in out Input_Source;
      Id    : Unicode.CES.Byte_Sequence);
   --  Set the system ID associated with the input source.
   --  Although this is optional, it is still useful since it can be used to
   --  resolve relative URI's from documents. In most cases, this is set
   --  automatically when you Open the input, and you can override it after the
   --  call to Open.

   function Get_System_Id (Input : Input_Source)
      return Unicode.CES.Byte_Sequence;
   --  Return the system Id.

   procedure Set_Public_Id
     (Input : in out Input_Source;
      Id    : Unicode.CES.Byte_Sequence);
   --  This will be provided as part of the location information, if it is
   --  given. In most cases, this is done automatically when you Open the
   --  input, and you can override it after the call to Open.

   function Get_Public_Id (Input : Input_Source)
      return Unicode.CES.Byte_Sequence;
   --  Return the public Id.

   procedure Close (Input : in out Input_Source);
   --  Free the memory allocated in the input.

private
   type Input_Source is abstract tagged limited record
      Prolog_Size : Natural := 0;
      Es          : Unicode.CES.Encoding_Scheme :=
        Unicode.CES.Basic_8bit.Basic_8bit_Encoding;
      Cs          : Unicode.CCS.Character_Set :=
        Unicode.CCS.Unicode_Character_Set;
      Public_Id   : Unicode.CES.Byte_Sequence_Access;
      System_Id   : Unicode.CES.Byte_Sequence_Access;
   end record;
end Input_Sources;
