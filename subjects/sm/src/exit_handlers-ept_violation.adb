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

with Subject.Text_IO;

with Subject_Info;

package body Exit_Handlers.EPT_Violation
is

   -------------------------------------------------------------------------

   procedure Process (Halt : out Boolean)
   is
   begin
      Subject.Text_IO.Put_String
        (Item => "EPT Violation at guest physical address ");
      Subject.Text_IO.Put_Word64 (Item => Subject_Info.State.Guest_Phys_Addr);
      Subject.Text_IO.New_Line;

      Halt := True;
   end Process;

end Exit_Handlers.EPT_Violation;
