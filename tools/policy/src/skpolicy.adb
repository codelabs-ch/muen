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

with Ada.Text_IO;
with Ada.Command_Line;

with Skp.Xml;
with Skp.Writers;
with Skp.Templates;
with Skp.Validators;

procedure Skpolicy
is

   --  Print tool usage.
   procedure Print_Usage;
   procedure Print_Usage
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Command_Line.Command_Name & " <schema> <templates_dir> <policy>");
   end Print_Usage;

   Data    : Skp.Xml.XML_Data_Type;
   Policy  : Skp.Policy_Type;
   Inc_Dir : constant String := "include";
   Pac_Dir : constant String := "pack";
begin
   if Ada.Command_Line.Argument_Count /= 3 then
      Print_Usage;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Skp.Xml.Parse (Data   => Data,
                  File   => Ada.Command_Line.Argument (Number => 3),
                  Schema => Ada.Command_Line.Argument (Number => 1));

   Policy := Skp.Xml.To_Policy (Data => Data);
   Skp.Validators.Validate_Policy (P => Policy);

   Skp.Templates.Set_Template_Dir
     (Path => Ada.Command_Line.Argument (Number => 2));

   Skp.Writers.Write_Kernel (Dir_Name => Inc_Dir,
                             Policy   => Policy);
   Skp.Writers.Write_Subjects (Dir_Name => Inc_Dir,
                               Policy   => Policy);
   Skp.Writers.Write_System (Dir_Name => Inc_Dir,
                             Policy   => Policy);
   Skp.Writers.Write_Hardware (Dir_Name => Inc_Dir,
                               Policy   => Policy);
   Skp.Writers.Write_Scheduling (Dir_Name => Inc_Dir,
                                 Policy   => Policy);
   Skp.Writers.Write_Interrupts (Dir_Name => Inc_Dir,
                                 Policy   => Policy);
   Skp.Writers.Write_Binaries (Dir_Name => Pac_Dir,
                               Policy   => Policy);

   Ada.Text_IO.Put_Line (Item => "Policy compilation successful");
   Ada.Text_IO.Put_Line (Item => "  * Include directory: " & Inc_Dir);
   Ada.Text_IO.Put_Line (Item => "  * Packer  directory: " & Pac_Dir);
end Skpolicy;
