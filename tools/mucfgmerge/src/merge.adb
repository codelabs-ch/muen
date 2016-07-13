--
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

with Ada.Strings.Unbounded;

with GNAT.Directory_Operations;

with Muxml.Utils;
with Mulog;
with Mutools.System_Config;

with Mergers;
with Merge.Checks;
with Merge.Conditionals;
with Merge.Expressions;
with Merge.Utils;

package body Merge
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   procedure Run
     (Config_File  : String;
      Output_File  : String;
      Include_Path : String)
   is
      Config : Muxml.XML_Data_Type;
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing system config '" & Config_File & "'");
      Muxml.Parse (Data => Config,
                   Kind => Muxml.System_Config,
                   File => Config_File);
      Checks.Required_Config_Values (Policy => Config);

      declare
         use type Utils.String_Array;

         Policy_File  : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "system");
         Inc_Path_Str : constant String
           := Include_Path & (if Include_Path'Length > 0 then ":" else "")
           & GNAT.Directory_Operations.Dir_Name (Path => Policy_File);
      begin
         Mulog.Log (Msg => "Using policy file '" & Policy_File & "'");
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => Policy_File);
         Muxml.Utils.Merge (Left      => Policy.Doc,
                            Right     => Config.Doc,
                            List_Tags => (1 => U ("boolean"),
                                          2 => U ("integer"),
                                          3 => U ("string")));

         Mulog.Log (Msg => "Using include path '" & Inc_Path_Str & "'");
         Mergers.Merge_XIncludes
           (Policy       => Policy,
            Include_Dirs => Utils.Tokenize (Str => Inc_Path_Str));
      end;

      declare
         Hardware_File : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "hardware");
      begin
         Mulog.Log (Msg => "Using hardware file '" & Hardware_File & "'");
         Mergers.Merge_Hardware
           (Policy        => Policy,
            Hardware_File => Hardware_File);
      end;

      if Mutools.System_Config.Has_String
        (Data => Config,
         Name => "additional_hardware")
      then
         declare
            Additional_Hw_File : constant String
              := Mutools.System_Config.Get_Value
                (Data => Config,
                 Name => "additional_hardware");
         begin
            Mulog.Log (Msg => "Using additional hardware file '"
                       & Additional_Hw_File & "'");
            Mergers.Merge_Hardware
              (Policy        => Policy,
               Hardware_File => Additional_Hw_File);
         end;
      end if;

      declare
         Platform_File : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "platform");
      begin
         Mulog.Log (Msg => "Using platform file '" & Platform_File & "'");
         Mergers.Merge_Platform
           (Policy        => Policy,
            Platform_File => Platform_File);
      end;

      Checks.Expression_Config_Var_Refs (Policy => Policy);
      Checks.Expression_Integer_Values (Policy => Policy);
      Checks.Expression_Boolean_Values (Policy => Policy);

      Expressions.Expand (Policy => Policy);
      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/expressions");

      --  Check conditional references after expression evaluation.

      Checks.Conditional_Config_Var_Refs (Policy => Policy);
      Conditionals.Expand (Policy => Policy);

      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_Src,
         Data => Policy);
      Mulog.Log (Msg => "Successfully created policy '" & Output_File & "'");
   end Run;

end Merge;
