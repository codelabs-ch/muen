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

package body Memhashes.Cmd_Line
is

   use Ada.Strings.Unbounded;

   -------------------------------------------------------------------------

   function Get_Input_Dir return String
   is
   begin
      return To_String (Input_Dir);
   end Get_Input_Dir;

   -------------------------------------------------------------------------

   function Get_Policy_Input return String
   is
   begin
      return To_String (Policy_In);
   end Get_Policy_Input;

   -------------------------------------------------------------------------

   function Get_Policy_Output return String
   is
   begin
      return To_String (Policy_Out);
   end Get_Policy_Output;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      Cmdline : Mutools.Cmd_Line.Config_Type;
      In_Dir  : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <input_file> <output_file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => In_Dir'Access,
         Switch      => "-i:",
         Long_Switch => "--input-directory:",
         Help        => "Colon-separated list of input paths");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);
         if In_Dir'Length /= 0 then
            Input_Dir := To_Unbounded_String (In_Dir.all);
         end if;
         GNAT.Strings.Free (X => In_Dir);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line =>
            raise Invalid_Cmd_Line;
         when GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Policy_In := To_Unbounded_String
        (GNAT.Command_Line.Get_Argument
           (Parser => Parser));
      Policy_Out := To_Unbounded_String
        (GNAT.Command_Line.Get_Argument
           (Parser => Parser));
      if Policy_In = Null_Unbounded_String
        or else Policy_Out = Null_Unbounded_String
        or else Input_Dir = Null_Unbounded_String
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Memhashes.Cmd_Line;
