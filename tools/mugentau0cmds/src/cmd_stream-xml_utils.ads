--
--  Copyright (C) 2019  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2019  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;

with Muxml;

package Cmd_Stream.XML_Utils
is

   --  Create command stream XML document boilerplate.
   procedure Create_Stream_Boilerplate (Stream_Doc : out Muxml.XML_Data_Type);

   --  Command stream command attribute, value pair.
   type Attribute_Type is record
      Attr, Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Attribute_Array is array (Positive range <>) of Attribute_Type;

   Null_Attrs : constant Attribute_Array;

   --  Append command with given name and attributes to the specified command
   --  stream document.
   procedure Append_Command
     (Stream_Doc : Muxml.XML_Data_Type;
      Name       : String;
      Attrs      : Attribute_Array := Null_Attrs);

private

   Null_Attr : constant Attribute_Type
     := (Attr  => Ada.Strings.Unbounded.Null_Unbounded_String,
         Value => Ada.Strings.Unbounded.Null_Unbounded_String);

   Null_Attrs : constant Attribute_Array (1 .. 0)
     := (others => Null_Attr);

end Cmd_Stream.XML_Utils;
