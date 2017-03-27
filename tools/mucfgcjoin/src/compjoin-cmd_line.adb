--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body Compjoin.Cmd_Line
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Get_Input_File return String
   is (To_String (Source => Input_File));

   -------------------------------------------------------------------------

   function Get_Output_File return String
   is (To_String (Source => Output_File));

   -------------------------------------------------------------------------

   function Get_Component_List return String
   is (To_String (Source => Component_List));

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline : Mutools.Cmd_Line.Config_Type;
      In_File, Out_File, C_List : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options]",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => In_File'Access,
         Switch      => "-i:",
         Long_Switch => "--input-file:",
         Help        => "Input system policy");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Out_File'Access,
         Switch      => "-o:",
         Long_Switch => "--output-file:",
         Help        => "Output system policy");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => C_List'Access,
         Switch      => "-c:",
         Long_Switch => "--component-specs:",
         Help        => "Comma-separated list of component XML specs");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         if In_File'Length /= 0 then
            Input_File := To_Unbounded_String (In_File.all);
         end if;
         if Out_File'Length /= 0 then
            Output_File := To_Unbounded_String (Out_File.all);
         end if;
         if C_List'Length /= 0 then
            Component_List := To_Unbounded_String (C_List.all);
         end if;

         GNAT.Strings.Free (X => In_File);
         GNAT.Strings.Free (X => Out_File);
         GNAT.Strings.Free (X => C_List);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      if Input_File = Null_Unbounded_String
        or Output_File = Null_Unbounded_String
        or Component_List = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Compjoin.Cmd_Line;
