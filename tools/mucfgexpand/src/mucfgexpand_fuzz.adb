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

with Ada.Command_Line;
with Ada.Exceptions;

with GNAT.Exception_Actions;

with Mulog;
with Muxml;

with Mutools.Cmd_Line.Infile_Outfile;
with Mucfgcheck;

with Expand;
with Expanders;

procedure Mucfgexpand_Fuzz
is
begin
   Mutools.Cmd_Line.Infile_Outfile.Init
     (Description => "Muen policy expander (Fuzzing)");
   Mutools.Cmd_Line.Infile_Outfile.Run
     (Kind    => Muxml.Format_Src,
      Process => Expand.Run'Access);

exception
   when Mutools.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when Muxml.XML_Input_Error
      | Muxml.Validation_Error
      | Expanders.Expansion_Error
      | Mucfgcheck.Validation_Error =>
      null;
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      GNAT.Exception_Actions.Core_Dump (Occurrence => E);
end Mucfgexpand_Fuzz;
