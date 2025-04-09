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

with SK.UART;
private with SK.Constants;

generic

   with package UART_Pkg is new SK.UART (<>);

package SK.Console_Serial
is
   --  Init UART.
   procedure Init;

   --  Write newline.
   procedure New_Line;

   --  Write character.
   procedure Put_Char (Item : Character);

private

   State : UART_Pkg.State_Type
   with
      Linker_Section => Constants.Global_Data_Section;

end SK.Console_Serial;
