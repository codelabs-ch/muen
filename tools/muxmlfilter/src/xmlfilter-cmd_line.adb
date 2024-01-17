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

with GNAT.Strings;

with Mutools.Cmd_Line;

package body Xmlfilter.Cmd_Line
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

   function Get_Input_Schema_Name return String
   is
   begin
      return S (Input_Schema_Name);
   end Get_Input_Schema_Name;

   -------------------------------------------------------------------------

   function Get_Input_Xml_Path return String
   is
   begin
      return S (Input_Xml_Path);
   end Get_Input_Xml_Path;

   -------------------------------------------------------------------------

   function Get_Output_Schema_Name return String
   is
   begin
      return S (Output_Schema_Name);
   end Get_Output_Schema_Name;

   -------------------------------------------------------------------------

   function Get_Output_Schema_Path return String
   is
   begin
      return S (Output_Schema_Path);
   end Get_Output_Schema_Path;

   -------------------------------------------------------------------------

   function Get_Output_Xml_Path return String
   is
   begin
      return S (Output_Xml_Path);
   end Get_Output_Xml_Path;

   -------------------------------------------------------------------------

   procedure Init (Description : String)
   is
      use Ada.Strings.Unbounded;

      Cmdline       : Mutools.Cmd_Line.Config_Type;
      I_Schema_Name : aliased GNAT.Strings.String_Access;
      O_Schema_Name : aliased GNAT.Strings.String_Access;
      O_Schema_Path : aliased GNAT.Strings.String_Access;
   begin
      GNAT.Command_Line.Set_Usage
        (Config => Cmdline.Data,
         Usage  => "[options] <input_file> <output_file>",
         Help   => Description);
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Switch      => "-h",
         Long_Switch => "--help",
         Help        => "Display usage and exit");

      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => I_Schema_Name'Access,
         Switch      => "-isn:",
         Long_Switch => "--input-schema-name:",
         Help        => "Name of schema to be used for input validation."
           & " Required.");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => O_Schema_Name'Access,
         Switch      => "-osn:",
         Long_Switch => "--output-schema-name:",
         Help        => "Name of schema to be used for filtering."
           & " Required if osp is not given.");
      GNAT.Command_Line.Define_Switch
        (Config      => Cmdline.Data,
         Output      => O_Schema_Path'Access,
         Switch      => "-osp:",
         Long_Switch => "--output-schema-path:",
         Help        => "Path to xsd-schema definition to be used for filtering."
           & "Required if osn is not given. "
           & "In this case the output is not validated.");
      begin
         GNAT.Command_Line.Getopt
           (Config => Cmdline.Data,
            Parser => Parser);

         if I_Schema_Name'Length /= 0 then
            Input_Schema_Name := U (I_Schema_Name.all);
         end if;
         GNAT.Strings.Free (X => I_Schema_Name);

         if O_Schema_Name'Length /= 0 then
            Output_Schema_Name := U (O_Schema_Name.all);
         end if;
         GNAT.Strings.Free (X => O_Schema_Name);

         if O_Schema_Path'Length /= 0 then
            Output_Schema_Path := U (O_Schema_Path.all);
         end if;
         GNAT.Strings.Free (X => O_Schema_Path);

      exception
         when GNAT.Command_Line.Invalid_Switch |
              GNAT.Command_Line.Exit_From_Command_Line |
              GNAT.Command_Line.Invalid_Parameter =>
            GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
            raise Invalid_Cmd_Line;
      end;

      Input_Xml_Path := U (GNAT.Command_Line.Get_Argument (Parser => Parser));
      Output_Xml_Path := U (GNAT.Command_Line.Get_Argument (Parser => Parser));

      if Input_Xml_Path = Null_Unbounded_String
        or Output_Xml_Path = Null_Unbounded_String
        or Input_Schema_Name = Null_Unbounded_String
        or (Output_Schema_Name = Null_Unbounded_String
              and Output_Schema_Path = Null_Unbounded_String)
        or (Output_Schema_Name /= Null_Unbounded_String
              and Output_Schema_Path /= Null_Unbounded_String)
      then
         GNAT.Command_Line.Display_Help (Config => Cmdline.Data);
         raise Invalid_Cmd_Line;
      end if;
   end Init;

end Xmlfilter.Cmd_Line;
