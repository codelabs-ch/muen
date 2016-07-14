--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Time;
with Subject_Info;
with Devices.RTC;
with Devices.UART8250;

package Exit_Handlers.IO_Instruction
is

   --  Emulate I/O port access.
   procedure Process (Halt : out Boolean)
   with
      Global  => (Input  => (Time.State, Mutime.Info.State),
                  In_Out => (Subject_Info.State, Devices.UART8250.State,
                             Devices.RTC.State, Debuglog.Client.State,
                             X86_64.State)),
      Depends =>
       (Subject_Info.State =>+ (Time.State, Mutime.Info.State,
                                Devices.RTC.State, Devices.UART8250.State),
        (Debuglog.Client.State,
         Devices.UART8250.State,
         X86_64.State) =>+ (Subject_Info.State,
                            Devices.UART8250.State),
        Devices.RTC.State =>+ (Subject_Info.State, Time.State,
                               Mutime.Info.State),
        Halt => (Subject_Info.State, Time.State, Mutime.Info.State,
                 Devices.RTC.State));

end Exit_Handlers.IO_Instruction;
