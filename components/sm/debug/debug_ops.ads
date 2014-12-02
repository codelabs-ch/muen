--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with SK;

package Debug_Ops
is

   --  Output given string.
   procedure Put_String (Item : String);

   --  Output given string and append a new line.
   procedure Put_Line (Item : String);

   --  Output value in hex with given prepended message and newline.
   procedure Put_Value8
     (Message : String;
      Value   : SK.Byte);

   --  Output value in hex with given prepended message and newline.
   procedure Put_Value16
     (Message : String;
      Value   : SK.Word16);

   --  Output value in hex with given prepended message and newline.
   procedure Put_Value32
     (Message : String;
      Value   : SK.Word32);

   --  Output value in hex with given prepended message and newline.
   procedure Put_Value64
     (Message : String;
      Value   : SK.Word64);

   --  Dump subject state before Halt.
   procedure Dump_State;

end Debug_Ops;
