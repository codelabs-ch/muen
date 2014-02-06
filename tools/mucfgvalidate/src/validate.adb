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

with Ada.Containers.Doubly_Linked_Lists;

with Mulog;

with Validate.Command_Line;

package body Validate
is

   package Validator_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Validation_Procedure);

   Validators : Validator_Package.List;

   -------------------------------------------------------------------------

   procedure Register (Validator : Validation_Procedure)
   is
   begin
      Validators.Append (New_Item => Validator);
   end Register;

   -------------------------------------------------------------------------

   procedure Run
   is
      Data        : Muxml.XML_Data_Type;
      Pos         : Validator_Package.Cursor := Validators.First;
      Policy_File : constant String          := Command_Line.Get_Policy;
   begin
      Mulog.Log (Msg => "Validating policy '" & Policy_File & "'");
      Mulog.Log (Msg => "Registered validators" & Validators.Length'Img);

      Muxml.Parse (Data => Data,
                   File => Policy_File);
      while Validator_Package.Has_Element (Position => Pos) loop
         Validator_Package.Element (Position => Pos) (XML_Data => Data);
         Validator_Package.Next (Position => Pos);
      end loop;

      Mulog.Log (Msg => "Successfully validated policy '" & Policy_File & "'");
   end Run;

end Validate;
