--
--  Copyright (C) 2018  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Mutools.Files;

with Ptwalk.Cmd_Line;

procedure Muwalkpt
is
begin
   Ptwalk.Cmd_Line.Init (Description => "Muen page table walker");
   Ptwalk.Run (Table_File      => Ptwalk.Cmd_Line.Get_PT_File,
               Table_Type      => Ptwalk.Cmd_Line.Get_PT_Type,
               Table_Pointer   => Ptwalk.Cmd_Line.Get_PT_Pointer,
               Virtual_Address => Ptwalk.Cmd_Line.Get_Virtual_Address);

exception
   when Ptwalk.Cmd_Line.Invalid_Cmd_Line =>
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : Mutools.Files.IO_Error =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Pagetable walk failed, aborting");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Message (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
   when E : others =>
      Mulog.Log (Level => Mulog.Error,
                 Msg   => "Unexpected exception");
      Mulog.Log (Level => Mulog.Error,
                 Msg   => Ada.Exceptions.Exception_Information (X => E));
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Muwalkpt;
