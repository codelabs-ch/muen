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

with Mulog;
with Muxml;
with Mucfgcheck;

with Pack.Pre_Checks;
with Pack.Post_Checks;
with Pack.Cmd_Line;

procedure Mupack
is
begin
   Pack.Cmd_Line.Init (Description => "Muen system image packager");
   Pack.Run (Policy_File    => Pack.Cmd_Line.Get_Policy,
             Input_Dir      => Pack.Cmd_Line.Get_Input_Dir,
             Output_Dir     => Pack.Cmd_Line.Get_Output_Dir,
             Output_Imgname => Pack.Cmd_Line.Get_Output_Imgname,
             Dry_Run        => Pack.Cmd_Line.Is_Dry_Run);

exception
   when Pack.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Muxml.XML_Input_Error
      | Muxml.Validation_Error
      | Mucfgcheck.Validation_Error
      | Pack.Pack_Error
      | Pack.Pre_Checks.Check_Error
      | Pack.Post_Checks.Check_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Processing failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mupack;
