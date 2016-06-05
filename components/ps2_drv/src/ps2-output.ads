--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Input;

package PS2.Output
with
   Abstract_State => (State with External => Async_Readers)
is

   --  Initialize output channel.
   procedure Init
   with
      Global  => (Output => State),
      Depends => (State => null);

   --  Forward input event by writing it into output channel.
   procedure Write (Event : Input.Input_Event_Type)
   with
      Global  => (In_Out => (State, X86_64.State)),
      Depends => (State        =>+ Event,
                  X86_64.State =>+ null);

end PS2.Output;
