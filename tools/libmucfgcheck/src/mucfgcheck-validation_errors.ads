--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Mucfgcheck.Validation_Errors
is

   --  Insert validation into error box. If Fatal is True, a Validation_Error
   --  exception is raised right away.
   procedure Insert
     (Msg   : String;
      Fatal : Boolean := False);

   --  Check whether the error box is empty. Raises validation error if it
   --  contains validation error messages.
   procedure Check;

   --  Return error message composed of errors currently in the error box.
   function Get_Error_Message return String;

   --  Clear validation error messages from message box.
   procedure Clear;

   --  Returns True if the error message box contains the given error message.
   function Contains (Msg : String) return Boolean;

   --  Returns True if the error message box is empty.
   function Is_Empty return Boolean;

   --  Raised if the validation error box is non-empty on Check.
   Validation_Error : exception;

private

   package List_Of_Ustring_Pkg is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Element_Type => Ada.Strings.Unbounded.Unbounded_String,
        "="          => Ada.Strings.Unbounded."=");

   type List_Of_Ustring is new List_Of_Ustring_Pkg.List with null record;

   Errors : List_Of_Ustring;

   function Is_Empty return Boolean is (Errors.Is_Empty);

end Mucfgcheck.Validation_Errors;
