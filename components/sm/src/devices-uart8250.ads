--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Debuglog.Client;

with Types;
with Subject_Info;

package Devices.UART8250
with
   Abstract_State => State,
   Initializes    => State
is

   --  Emulated COM port ranges.
   subtype Com1_Port_Range is SK.Word16 range 16#03f8# .. 16#03ff#;

   --  Emulate UART 8250 controller COM1.
   procedure Emulate
     (Info :     Types.IO_Info_Type;
      Halt : out Boolean)
   with
      Global  => (In_Out => (State, Subject_Info.State,
                             Debuglog.Client.State, X86_64.State)),
      Depends =>
        ((State, Subject_Info.State, Debuglog.Client.State) =>+
           (State, Info, Subject_Info.State),
         X86_64.State =>+ (State, Info),
         Halt         => null),
      Post    => Halt = False;

end Devices.UART8250;
