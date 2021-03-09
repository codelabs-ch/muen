--
--  Copyright (C) 2013, 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Subject_Info;
with Types;
with Musinfo.Instance;

package Exit_Handlers.RDTSC
is

   use type Types.Subject_Action_Type;

   --  Emulate RDTSC instruction.
   procedure Process (Action : out Types.Subject_Action_Type)
   with
      Global  => (Proof_In => Musinfo.Instance.State,
                  Input    => Musinfo.Instance.Scheduling_Info,
                  In_Out   => Subject_Info.State),
      Depends => (Action             => null,
                  Subject_Info.State =>+ Musinfo.Instance.Scheduling_Info),
      Pre     => Musinfo.Instance.Is_Valid,
      Post    => Action = Types.Subject_Continue;

end Exit_Handlers.RDTSC;
