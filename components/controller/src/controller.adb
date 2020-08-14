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

with Debuglog.Client;

with Ctrlr;

procedure Controller
is
begin
   Debuglog.Client.Init (Epoch => 1);
   Debuglog.Client.Put_Line (Item => "Controller running");
   pragma Annotate
     (GNATprove, False_Positive,
      """Client.State"" might not be initialized after elaboration of main",
      "Explicitly initialized with prior call to Client.Init.");

   loop
      Ctrlr.Run;
   end loop;
end Controller;
