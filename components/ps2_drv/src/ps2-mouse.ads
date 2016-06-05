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

with SK;

with Log;

with PS2.Output;

package PS2.Mouse
with
   Abstract_State => State,
   Initializes    => State
is

   --  Initialize mouse device.
   procedure Init
   with
      Global => (In_Out => (Log.Text_IO.State, X86_64.State));

   --  Process mouse data.
   procedure Process (Data : SK.Byte)
   with
      Global  => (In_Out => (State, Output.State, X86_64.State)),
      Depends => ((State, Output.State, X86_64.State) =>+ (Data, State));

end PS2.Mouse;
