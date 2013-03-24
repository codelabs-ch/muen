------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2007-2012, AdaCore                     --
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

--  This package provides an example of an Ada stream that reads or
--  writes to an in-memory string. This can be used when printing a
--  DOM tree for instance (see tostring.adb)

with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package String_Stream is

   type String_Stream_Type is new Root_Stream_Type with record
      Str        : Unbounded_String;
      Read_Index : Natural := 1;
   end record;
   procedure Read
     (Stream : in out String_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);
   procedure Write
      (Stream : in out String_Stream_Type;
       Item   : Stream_Element_Array);
   procedure Open
      (Stream : in out String_Stream_Type'Class;
       Str    : String);
   --  Declare a new stream type to write strings in memory

end String_Stream;
