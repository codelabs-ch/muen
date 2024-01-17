--
--  Copyright (C) 2023 secunet Security Networks AG
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
with Ada.IO_Exceptions;

with Mulog;
with Muxml;
with Muxml.Grammar_Tools;

with Xmlfilter.Cmd_Line;

procedure Muxmlfilter
is
begin
   Xmlfilter.Cmd_Line.Init
      (Description => "Muen XML filtering tool");
   Xmlfilter.Run
     (Input_Xml_Path     => Xmlfilter.Cmd_Line.Get_Input_Xml_Path,
      Input_Schema_Name  => Xmlfilter.Cmd_Line.Get_Input_Schema_Name,
      Output_Xml_Path    => Xmlfilter.Cmd_Line.Get_Output_Xml_Path,
      Output_Schema_Name => Xmlfilter.Cmd_Line.Get_Output_Schema_Name,
      Output_Schema_Path => Xmlfilter.Cmd_Line.Get_Output_Schema_Path);

exception
   when Xmlfilter.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Muxml.XML_Input_Error
     | Muxml.Validation_Error
     | Muxml.Grammar_Tools.Not_Implemented
     | Ada.IO_Exceptions.Name_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Reading or parsing of files failed. Aborting.");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);

   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Muxmlfilter;
