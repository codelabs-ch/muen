--
--  Copyright (C) 2020  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2020  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with Interfaces;

package Ctrlr.Lifecycle
is

   --  Update state of managed subject specified by ID and perform transition
   --  if possible.
   procedure Process (ID : Managed_Subjects_Range);

private

   type Subject_Lifecycle_State_Type is record
      Current_State : Run_State_Type;
      Current_Epoch : Interfaces.Unsigned_64;
      Current_Era   : Interfaces.Unsigned_64;
   end record;

   type Lifecycle_States_Type is
     array (Managed_Subjects_Range) of Subject_Lifecycle_State_Type;

   States : Lifecycle_States_Type
     := (others => (Current_State => FSM_Start,
                    Current_Epoch => 0,
                    Current_Era   => 0));

end Ctrlr.Lifecycle;
