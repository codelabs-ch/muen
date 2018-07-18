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

   function Get_Package_Name return String
   is (S (Package_Name));

   -------------------------------------------------------------------------

   function Get_Include_Path return String
   is (S (Include_Path));

   -------------------------------------------------------------------------

   function Get_Input_Spec return String
   is (S (Input_Spec));

   -------------------------------------------------------------------------

   function Get_Output_Dir return String
   is (S (Output_Dir));

   -------------------------------------------------------------------------

   function Get_Output_Spec return String
   is (S (Output_Spec));

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline  : Mutools.Cmd_Line.Config_Type;
      In_Spec  : aliased GNAT.Strings.String_Access;
      Inc_Dir  : aliased GNAT.Strings.String_Access;
      Out_Spec : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <output_dir>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => In_Spec'Access,
         Switch      => "-i:",
         Long_Switch => "--input-spec:",
         Help        => "Path to input component specification");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Out_Spec'Access,
         Switch      => "-o:",
         Long_Switch => "--output-spec:",
         Help        => "Processed component specification path");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Inc_Dir'Access,
         Switch      => "-I:",
         Long_Switch => "--include-path:",
         Help        => "Colon-separated list of include paths");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         if In_Spec'Length /= 0 then
            Input_Spec := U (In_Spec.all);
         end if;
         if Inc_Dir'Length /= 0 then
            Include_Path := To_Unbounded_String (Inc_Dir.all);
         end if;
         if Out_Spec'Length /= 0 then
            Output_Spec := To_Unbounded_String (Out_Spec.all);
         end if;

         GNAT.Strings.Free (X => In_Spec);
         GNAT.Strings.Free (X => Inc_Dir);
         GNAT.Strings.Free (X => Out_Spec);

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
        or Input_Spec = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Cspec.Cmd_Line;
