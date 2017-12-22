--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Ada.Directories;

with GNAT.Strings;

with Mutools.Cmd_Line;

package body Bin_Split.Cmd_Line
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

   function Get_Binary return String
   is (S (Binary));

   -------------------------------------------------------------------------

   function Get_Output_Spec return String
   is (S (Output_Spec));

   -------------------------------------------------------------------------

   function Get_Spec return String
   is (S (Spec));

   -------------------------------------------------------------------------

   function Get_Output_Dir return String
   is (S (Output_Dir));

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline  : Mutools.Cmd_Line.Config_Type;
      In_Spec  : aliased GNAT.Strings.String_Access;
      In_Bin   : aliased GNAT.Strings.String_Access;
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
         Output      => In_Bin'Access,
         Switch      => "-b:",
         Long_Switch => "--input-binary:",
         Help        => "Path to input component binary");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => Out_Spec'Access,
         Switch      => "-o:",
         Long_Switch => "--output-spec:",
         Help        => "Processed component specification path");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");
      begin
         GNAT.Command_Line.Getopt
           (Config      => Cmdline.Data,
            Parser      => Parser);
         if In_Spec'Length /= 0 then
            Spec := U (In_Spec.all);
         end if;
         if In_Bin'Length /= 0 then
            Binary := U (In_Bin.all);
         end if;
         if Out_Spec'Length /= 0 then
            Output_Spec := U (Out_Spec.all);
         elsif In_Spec'Length /= 0 then
            Output_Spec
              := U (Ada.Directories.Simple_Name (Name => In_Spec.all));
         end if;

         GNAT.Strings.Free (X => In_Spec);
         GNAT.Strings.Free (X => In_Bin);
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

      if Spec = Null_Unbounded_String
        or Binary = Null_Unbounded_String
        or Output_Spec = Null_Unbounded_String
        or Output_Dir = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Bin_Split.Cmd_Line;
