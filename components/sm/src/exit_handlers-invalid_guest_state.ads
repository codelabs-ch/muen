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

with X86_64;

with Musinfo.Instance;

with Subject_Info;
with Types;

package Exit_Handlers.Invalid_Guest_State
is

   --  Handle exit due to invalid guest state.
   procedure Process (Action : out Types.Subject_Action_Type)
   with
      Pre    => Musinfo.Instance.Is_Valid,
      Global => (Input  => Musinfo.Instance.State,
                 In_Out => (Subject_Info.State, X86_64.State));

end Exit_Handlers.Invalid_Guest_State;
