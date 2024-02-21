--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.CPU;
with SK.Interrupt_Tables;

with Log;

with Vt_Component.Memory;

with Interrupt_Handler;
pragma Unreferenced (Interrupt_Handler);

with Mux.Terminals;

procedure VT
is
   use type SK.Word64;
begin
   Log.Text_IO.Init (Epoch => 1);
   SK.Interrupt_Tables.Initialize
     (Stack_Addr => Vt_Component.Memory.Interrupt_Stack_Address +
        Vt_Component.Memory.Interrupt_Stack_Size);
   Mux.Terminals.Initialize;

   SK.CPU.Sti;

   Mux.Terminals.Run;
end VT;
