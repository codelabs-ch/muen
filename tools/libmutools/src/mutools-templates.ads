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

private with Ada.Strings.Unbounded;

package Mutools.Templates
is

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

   Pattern_Not_Found : exception;
   IO_Error          : exception;

private

   type Template_Type is record
      Data : Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Mutools.Templates;
