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

with Mutime.Info;
with Musinfo.Instance;

with Types;
with Subject_Info;

package Devices.RTC
with
   Abstract_State => State,
   Initializes    => State
is

   use type Types.Subject_Action_Type;

   --  Emulate CMOS RTC I/O access.
   procedure Emulate
     (Info   :     Types.IO_Info_Type;
      Action : out Types.Subject_Action_Type)
   with
      Global  => (Proof_In => (Mutime.Info.Valid, Musinfo.Instance.State),
                  Input    => (Mutime.Info.State,
                               Musinfo.Instance.Scheduling_Info),
                  In_Out   => (State, Subject_Info.State)),
      Depends =>
         (State              =>+ (Info, Mutime.Info.State, Subject_Info.State,
                                  Musinfo.Instance.Scheduling_Info),
          Subject_Info.State =>+ (Info, State),
          Action             =>  null),
      Pre     => Musinfo.Instance.Is_Valid and Mutime.Info.Is_Valid,
      Post    => Action = Types.Subject_Continue;

end Devices.RTC;
