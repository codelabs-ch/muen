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

with System;

package body Knl_States_Interface
is

   Descriptors : SK.Subject_State_Array (Skp.Subject_Id_Type);
   for Descriptors'Address use System'To_Address (16#1fe000#);

   -------------------------------------------------------------------------

   function Get_Subject_State
     (Id : Skp.Subject_Id_Type)
      return SK.Subject_State_Type
   is
      State : SK.Subject_State_Type;
   begin
      State := Descriptors (Id);
      return State;
   end Get_Subject_State;

   -------------------------------------------------------------------------

   procedure Set_Subject_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type)
   is
   begin
      Descriptors (Id) := State;
   end Set_Subject_State;

end Knl_States_Interface;
