--
--  Copyright (C) 2015, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with X86_64;

with Skp;

package SK.FPU
with
   Abstract_State => State,
   Initializes    => State
is

   --  Check validity of initial FPU state.
   function Has_Valid_State return Boolean
   with
      Global => (Input => X86_64.State),
      Volatile_Function;

   --  Enable floating-point unit by initializing and configuring the
   --  FPU-related hardware registers.
   procedure Enable
   with
      Global  => (In_Out => X86_64.State),
      Depends => (X86_64.State =>+ null);

   --  Save current FPU state to save area of subject specified by ID.
   procedure Save_State (ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (Input  => X86_64.State,
                  In_Out => State),
      Depends => (State  =>+ (ID, X86_64.State));

   --  Restore FPU state from save area of subject specified by ID.
   procedure Restore_State (ID : Skp.Global_Subject_ID_Type)
   with
     Global  => (Input  => State,
                 In_Out => X86_64.State),
     Depends => (X86_64.State =>+ (ID, State));

   --  Clear FPU state of subject with given ID.
   procedure Clear_State (ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ ID);

end SK.FPU;
