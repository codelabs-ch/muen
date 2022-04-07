--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Debuglog.Client;

with Mutime.Info;
with Musinfo.Instance;

with Subject_Info;
with Devices.RTC;
with Devices.UART8250;
with Types;

package Exit_Handlers.IO_Instruction
is

   --  Emulate I/O port access.
   procedure Process (Action : out Types.Subject_Action_Type)
   with
      Global => (Proof_In => Musinfo.Instance.State,
                 Input    => (Mutime.Info.State,
                              Musinfo.Instance.Scheduling_Info),
                 In_Out   => (Subject_Info.State, Devices.UART8250.State,
                              Devices.RTC.State, Debuglog.Client.State,
                              X86_64.State)),
      Depends =>
       (Subject_Info.State       =>+ (Devices.RTC.State, Devices.UART8250.State,
                                      Mutime.Info.State,
                                      Musinfo.Instance.Scheduling_Info),
        (Debuglog.Client.State,
         Devices.UART8250.State,
         X86_64.State)           =>+ (Subject_Info.State,
                                      Devices.UART8250.State),
        Devices.RTC.State        =>+ (Subject_Info.State, Mutime.Info.State,
                                      Musinfo.Instance.Scheduling_Info),
        Action                   =>  Subject_Info.State),
      Pre     => Musinfo.Instance.Is_Valid;

end Exit_Handlers.IO_Instruction;
