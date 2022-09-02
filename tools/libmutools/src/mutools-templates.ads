--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
private with Ada.Strings.Unbounded;
private with Ada.Streams.Stream_IO;

package Mutools.Templates
is

   --  Template using an unbounded string buffer. For large replacement data
   --  use the Stream_Template_Type below.
   type Template_Type is private;

   --  Create template from given string.
   function Create (Content  : String) return Template_Type;

   --  Replace pattern occurrences in template with given content.
   procedure Replace
     (Template : in out Template_Type;
      Pattern  :        String;
      Content  :        String);

   --  Write (processed) template to given file.
   procedure Write
     (Template : Template_Type;
      Filename : String);

   --  IO stream based template for efficient writing of large datasets.
   type Stream_Template_Type is limited private;

   --  Create stream template from given string. The Filename argument denotes
   --  the output of the stream.
   function Create
     (Content  : String;
      Filename : String)
      return Stream_Template_Type;

   --  Stream static template content until a pattern (__X__) is recognized. The
   --  pattern string is skipped. Use this procedure to process the complete
   --  template data, write intermediate data with the Write() procedure below.
   procedure Stream (Template : in out Stream_Template_Type);

   --  Write given string to current stream position.
   procedure Write
     (Template : in out Stream_Template_Type;
      Item     :        String);

   --  Close stream template.
   procedure Close (Template : in out Stream_Template_Type);

   Pattern_Not_Found : exception;
   IO_Error          : exception;

private

   use Ada.Strings.Unbounded;

   type Template_Type is record
      Data : Unbounded_String;
   end record;

   package USLP is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Unbounded_String);

   type Stream_Template_Type is limited record
      Tokens : USLP.List;
      Pos    : USLP.Cursor;
      Fd     : Ada.Streams.Stream_IO.File_Type;
   end record;

   --  Tokenize given string (separator: __X__ patterns).
   procedure Tokenize
     (Data :     String;
      List : out USLP.List);

end Mutools.Templates;
