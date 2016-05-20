--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with GNAT.Strings;

with Mutools.Cmd_Line;

package body Cspec.Cmd_Line
is

   function S
     (Source : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   function Get_Component_Name return String
   is
   begin
      return S (Component_Name);
   end Get_Component_Name;

   -------------------------------------------------------------------------

   function Get_Output_Dir return String
   is
   begin
      return S (Output_Dir);
   end Get_Output_Dir;

   -------------------------------------------------------------------------

   function Get_Policy return String
   is
   begin
      return S (Policy);
   end Get_Policy;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline     : Mutools.Cmd_Line.Config_Type;
      Policy_Path : aliased GNAT.Strings.String_Access;
      Component   : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <output_dir>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Policy_Path'Access,
         Switch      => "-p:",
         Long_Switch => "--policy:",
         Help        => "System policy");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Component'Access,
         Switch      => "-c:",
         Long_Switch => "--component-name:",
         Help        => "Component name");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         if Policy_Path'Length /= 0 then
            Policy := U (Policy_Path.all);
         end if;
         GNAT.Strings.Free (X => Policy_Path);

         if Component'Length /= 0 then
            Component_Name := U (Component.all);
         end if;
         GNAT.Strings.Free (X => Component);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Output_Dir := U (GNAT.Command_Line.Get_Argument (Parser => Parser));

      if Output_Dir = Null_Unbounded_String
        or Component_Name = Null_Unbounded_String
        or Policy = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Cspec.Cmd_Line;
