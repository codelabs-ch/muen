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

with Mulog;
with Muxml;
with Mutools.Utils;

with Vres_Alloc;
with Vres_Alloc.Cmd_Line;

procedure Mucfgvresalloc
is
begin
   Vres_Alloc.Cmd_Line.Init
      (Description     => "Muen virtual address allocator");
   Vres_Alloc.Run
     (Policy_Joined    => Vres_Alloc.Cmd_Line.Get_Policy_Joined,
      Output_File_Name => Vres_Alloc.Cmd_Line.Get_Output_Filename);

exception
   when Vres_Alloc.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Muxml.XML_Input_Error
      | Mutools.Utils.File_Not_Found
      | Muxml.Validation_Error
      | Vres_Alloc.Validation_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Atomatic allocation of virtual addresses failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Mucfgvresalloc;
