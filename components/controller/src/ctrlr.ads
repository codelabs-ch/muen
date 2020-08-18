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

with Controller_Component.Memory_Arrays;

package Ctrlr
is

   type Subjects_Range is range
     0 .. Controller_Component.Memory_Arrays.Control_Element_Count;

   No_Subject : constant Subjects_Range := Subjects_Range'First;

   subtype Managed_Subjects_Range is Subjects_Range
    range 1 .. Subjects_Range'Last;

   Page_Size : constant := 4096;

   --  Various run states a managed subject can be in.
   type Run_State_Type is
     (FSM_Start, FSM_Initial, FSM_Syncing, FSM_Erasing, FSM_Preparing,
      FSM_Validating, FSM_Running, FSM_Finished, FSM_Resetting, FSM_Error,
      FSM_Self_Control);

   --  Run controller.
   procedure Run;

end Ctrlr;
