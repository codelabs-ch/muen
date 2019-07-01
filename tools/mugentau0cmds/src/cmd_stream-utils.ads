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

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Text_IO;
with Ada.Strings.Unbounded;

with Interfaces;

package Cmd_Stream.Utils
is

   --  Command stream document type
   type Stream_Document_Type is limited private;

   --  Create command stream document with given filename.
   procedure Create
     (Stream_Doc : out Stream_Document_Type;
      Filename   :     String);

   --  Command stream command attribute, value pair.
   type Attribute_Type is record
      Attr, Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   Null_Attr : constant Attribute_Type;

   type Attribute_Array is array (Positive range <>) of Attribute_Type;

   Null_Attrs : constant Attribute_Array;

   type Command_Buffer_Type is private;

   --  Reverse order of commands in the given buffer.
   procedure Reverse_Commands (Buffer : in out Command_Buffer_Type);

   --  Append command with given name and attributes to the specified command
   --  stream document.
   procedure Append_Command
     (Stream_Doc : in out Stream_Document_Type;
      Name       :        String;
      Attrs      :        Attribute_Array := Null_Attrs);

   --  Append command with given name and attributes to the specified command
   --  buffer which is part of the designated stream document.
   procedure Append_Command
     (Buffer : in out Command_Buffer_Type;
      Name   :        String;
      Attrs  :        Attribute_Array := Null_Attrs);

   --  Append commands from given buffer to the specified command stream
   --  document.
   procedure Append_Commands
     (Stream_Doc : in out Stream_Document_Type;
      Buffer     :        Command_Buffer_Type);

   --  Generate command stream to clear memory region specified by base address
   --  and size.
   procedure Clear_Region
     (Stream_Doc   : in out Stream_Document_Type;
      Base_Address :        Interfaces.Unsigned_64;
      Size         :        Interfaces.Unsigned_64);

   --  Write closing tags to given command stream and close file descriptor.
   procedure Close (Stream_Doc : in out Stream_Document_Type);

   IO_Error : exception;

private

   use type Ada.Strings.Unbounded.Unbounded_String;

   package Cmd_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Ada.Strings.Unbounded.Unbounded_String);

   type Command_Buffer_Type is record
      Cmds : Cmd_List.List;
   end record;

   Null_Attr : constant Attribute_Type
     := (Attr  => Ada.Strings.Unbounded.Null_Unbounded_String,
         Value => Ada.Strings.Unbounded.Null_Unbounded_String);

   Null_Attrs : constant Attribute_Array (1 .. 0)
     := (others => Null_Attr);

   type Stream_Document_Type is limited record
      File : Ada.Text_IO.File_Type;
   end record;

end Cmd_Stream.Utils;
