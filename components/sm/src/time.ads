--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mutime.Info;
with Musinfo.Instance;

package Time
is

   --  Initialize time. Halts the CPU if the sinfo data is not valid.
   procedure Initialize
   with
      Global  => (Input  => (Musinfo.Instance.State, Mutime.Info.State),
                  In_Out => X86_64.State),
      Depends => (X86_64.State =>+ Musinfo.Instance.State,
                  null         => Mutime.Info.State),
      Post    => Musinfo.Instance.Is_Valid;

   --  Return current date and time.
   function Get_Date_Time return Mutime.Date_Time_Type
   with
      Global => (Proof_In => Musinfo.Instance.State,
                 Input    => (Mutime.Info.State,
                              Musinfo.Instance.Scheduling_Info)),
      Pre    => Musinfo.Instance.Is_Valid,
      Volatile_Function;

end Time;
