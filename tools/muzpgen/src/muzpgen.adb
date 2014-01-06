--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Mulog;

with Mugen.Command_Line;

procedure Muzpgen
is
begin
   Mugen.Command_Line.Init
     (Description => "Generate Linux zero-page structures according to given "
      & "system policy");

   Mulog.Log (Msg => "Using output directory '"
              & Mugen.Command_Line.Get_Output_Dir & "'");
   Mulog.Log (Msg => "Processing policy '"
              & Mugen.Command_Line.Get_Policy & "'");
end Muzpgen;
