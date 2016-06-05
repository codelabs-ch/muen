--
--  Copyright (C) 2013, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK;

with PS2.Output;

package PS2.Keyboard
with
   Abstract_State => State,
   Initializes    => State
is

   --  Process keyboard data.
   procedure Process (Data : SK.Byte)
   with
      Global  => (In_Out => (State, Output.State, X86_64.State)),
      Depends => ((State, Output.State, X86_64.State) =>+ (Data, State));

end PS2.Keyboard;
