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

--  This example shows how an XML tree can be converted to a string
--  in memory, without going through a temporary file on the disk

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with DOM.Core.Nodes;        use DOM.Core.Nodes;
with DOM.Readers;           use DOM.Readers;
with Input_Sources.File;    use Input_Sources.File;
with String_Stream;         use String_Stream;

procedure ToString is
   Input  : File_Input;
   Reader : Tree_Reader;
   Output : aliased String_Stream_Type;
begin
   Open ("test.xml", Input);
   Parse (Reader, Input);
   Close (Input);

   Open (Output, "");
   Write (Output'Access, Get_Tree (Reader));
   Put_Line (To_String (Output.Str));
end ToString;
