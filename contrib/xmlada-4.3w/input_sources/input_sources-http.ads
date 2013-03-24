------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with Unicode;
with Unicode.CES;

package Input_Sources.Http is

   Http_Error : exception;

   type Http_Input is new Input_Source with private;
   type Http_Input_Access is access all Http_Input'Class;
   --  A special implementation of a reader, that reads from http location.

   procedure Open
     (Hostname : String;
      Port     : Positive := 80;
      Filename : String;
      Input    : out Http_Input);
   --  Open a new file for reading from http://Hostname:Port/Filename.
   --  Raise Http_Error if the URL could not be parsed.

   procedure Open (URL : String; Input : out Http_Input);
   --  Same as above, but processes directly a URL

   procedure Close (Input : in out Http_Input);
   --  Free the memory

   procedure Next_Char
     (From : in out Http_Input;
      C    : out Unicode.Unicode_Char);
   --  Return the next character in the buffer.

   function Eof (From : Http_Input) return Boolean;
   --  True if From is past the last character in the buffer.

private
   type Http_Input is new Input_Source with record
      Index  : Natural;
      Buffer : Unicode.CES.Byte_Sequence_Access;
   end record;
end Input_Sources.Http;
