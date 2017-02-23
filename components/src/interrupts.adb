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

package body Interrupts
with
   Refined_State => (State => Manager)
is

   Manager : SK.Interrupt_Tables.Manager_Type;

   Component_Interrupt_Stack_Address : constant := 16#5000#;

   -------------------------------------------------------------------------

   procedure Initialize
   with
      Refined_Global => (Input  => SK.Interrupt_Tables.State,
                         Output => Manager,
                         In_Out => X86_64.State)
   is
   begin
      SK.Interrupt_Tables.Initialize
        (Manager    => Manager,
         Stack_Addr => Component_Interrupt_Stack_Address);
   end Initialize;

end Interrupts;
