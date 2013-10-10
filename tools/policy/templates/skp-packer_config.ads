--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with SK;

--  Generated definitions may be longer than 79 characters. Switch off this
--  check.
pragma Style_Checks ("-m");

package Skp.Packer_Config is

   type Binary_Spec_Type is record
      Name             : Unbounded_String;
      Path             : Unbounded_String;
      Physical_Address : SK.Word64;
   end record;

   type Binary_Spec_Array is array (Subject_Id_Type) of Binary_Spec_Type;

   Binary_Specs : constant Binary_Spec_Array := (
__binaries__);

end Skp.Packer_Config;
