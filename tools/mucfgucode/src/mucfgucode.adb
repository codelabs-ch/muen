--
--  Copyright (C) 2023  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2023  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Exceptions;
with Ada.Command_Line;

with Mulog;
with Muxml;
with Mutools.OS;

with Ucode.Cmd_Line;

procedure Mucfgucode
is
begin
   Ucode.Cmd_Line.Init
     (Description => "Muen ucode tool for Intel microcode updates");
   Ucode.Run (Policy     => Ucode.Cmd_Line.Get_Policy,
              Ucode_Dir  => Ucode.Cmd_Line.Get_Ucode_Dir,
              Output_Dir => Ucode.Cmd_Line.Get_Output_Dir);

exception
   when Ucode.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Muxml.XML_Input_Error
      | Muxml.Validation_Error
      | Mutools.OS.Command_Failed =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Ucode processing failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mucfgucode;
