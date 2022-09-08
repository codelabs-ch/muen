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

with GNAT.Directory_Operations;

with Muxml.Utils;
with Mulog;
with Mutools.System_Config;
with Mutools.Strings;
with Mutools.XML_Utils;
with Mutools.XML_Templates;
with Mutools.Amend;
with Mutools.Expressions;
with Mutools.Conditionals;
with Mutools.Substitutions;
with Mutools.Xmldebuglog;
with Mucfgcheck.Config;
with Mucfgcheck.Templates;
with Mucfgcheck.Validation_Errors;
with Mergers;
with Merge.Checks;
with Merge.Utils;

package body Merge
is

   -------------------------------------------------------------------------

   procedure Run
     (Config_File  : String;
      Output_File  : String;
      Include_Path : String;
      Debug_Level  : Debug_Level_Type)
   is
      Local_Include_Path : constant String
         := Include_Path & (if Include_Path'Length > 0 then ":" else "") & ".";
      Policy, Config  : Muxml.XML_Data_Type;

      Debug_Active    : constant Boolean
          := (if Debug_Level = NONE then False else True);
   begin
      Mulog.Log (Msg => "Processing system config '" & Config_File & "'");

      -- cannot validate if Debug_Active is true.
      -- Hence, validation is separate.
      declare
         Config_tmp : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Config_tmp,
                      Kind => Muxml.System_Config,
                      File => Config_File);
         Checks.Required_Config_Values (Policy => Config_tmp);
         Mucfgcheck.Validation_Errors.Check;
      end;

      Muxml.Parse (Data         => Config,
                   Kind         => Muxml.None,
                   File         => Config_File,
                   Add_Location => Debug_Active);

      Mulog.Log (Msg => "Using include path '" & Local_Include_Path & "'");

      declare
         Policy_File     : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "system");
         Policy_Filename : constant String
           := Utils.Lookup_File (Name => Policy_File,
                                 Dirs => Local_Include_Path);
         Inc_Path_Str    : constant String
           := Include_Path & (if Include_Path'Length > 0 then ":" else "")
              & GNAT.Directory_Operations.Dir_Name (Path => Policy_File);
      begin
         Mulog.Log (Msg => "Using policy file '" & Policy_Filename & "'");
         Muxml.Parse (Data         => Policy,
                      Kind         => Muxml.None,
                      File         => Policy_Filename,
                      Add_Location => Debug_Active);
         Mergers.Merge_Config (Policy => Policy,
                               Config => Config);

         Mutools.XML_Utils.Merge_XIncludes
           (Policy       => Policy,
            Include_Dirs => Mutools.Strings.Tokenize (Str => Inc_Path_Str),
            Add_Location => Debug_Active);
      end;

      declare
         Hardware_File     : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "hardware");
         Hardware_Filename : constant String
           := Utils.Lookup_File (Name => Hardware_File,
                                 Dirs => Local_Include_Path);
      begin
         Mulog.Log (Msg => "Using hardware file '" & Hardware_Filename & "'");
         Mergers.Merge_Hardware
           (Policy        => Policy,
            Hardware_File => Hardware_Filename,
            Add_Location  => Debug_Active);
      end;

      if Mutools.System_Config.Has_String
        (Data => Config,
         Name => "additional_hardware")
      then
         declare
            Additional_Hw_File     : constant String
              := Mutools.System_Config.Get_Value
                (Data => Config,
                 Name => "additional_hardware");
            Additional_Hw_Filename : constant String
              := Utils.Lookup_File (Name => Additional_Hw_File,
                                    Dirs => Local_Include_Path);
         begin
            Mulog.Log (Msg => "Using additional hardware file '"
                       & Additional_Hw_Filename & "'");
            Mergers.Merge_Hardware
              (Policy        => Policy,
               Hardware_File => Additional_Hw_Filename,
               Add_Location  => Debug_Active);
         end;
      end if;

      declare
         Platform_File     : constant String
           := Mutools.System_Config.Get_Value
             (Data => Config,
              Name => "platform");
         Platform_Filename : constant String
           := Utils.Lookup_File (Name => Platform_File,
                                 Dirs => Local_Include_Path);
      begin
         Mulog.Log (Msg => "Using platform file '" & Platform_Filename & "'");
         Mergers.Merge_Platform
           (Policy        => Policy,
            Platform_File => Platform_Filename,
            Add_Location  => Debug_Active);
      end;
      Mergers.Merge_Platform_Config (Policy => Policy);

      Mucfgcheck.Config.Name_Uniqueness (XML_Data => Policy);
      Mucfgcheck.Config.Expression_Config_Var_Refs (XML_Data => Policy);
      Mucfgcheck.Config.Expression_Integer_Values (XML_Data => Policy);
      Mucfgcheck.Config.Expression_Boolean_Values (XML_Data => Policy);
      Mucfgcheck.Templates.Name_Uniqueness (XML_Data => Policy);
      Mucfgcheck.Templates.Template_Integrity (XML_Data => Policy);
      Mucfgcheck.Validation_Errors.Check;

      Mulog.Log (Msg => "Processing templates");
      Mutools.XML_Templates.Expand (XML_Data     => Policy,
                                    Debug_Active => Debug_Active);

      if Debug_Active then
         Mutools.Xmldebuglog.Move_Origin_To_Log (Doc => Policy.Doc);
      end if;

      -- Check uniqueness of variable names in config block as well as
      -- names of expression again (templates may introduce duplicates)

      Mucfgcheck.Config.Name_Uniqueness (XML_Data => Policy);
      Mucfgcheck.Validation_Errors.Check;

      Mulog.Log (Msg => "Processing expressions");
      Mutools.Expressions.Expand (Policy       => Policy,
                                  Debug_Active => Debug_Active);

      if Debug_Active then
         Mutools.Xmldebuglog.Remove_Log_Of_Subtree
            (Node  => Policy.Doc,
             XPath => "/system/expressions");
      end if;

      Muxml.Utils.Remove_Elements
        (Doc   => Policy.Doc,
         XPath => "/system/expressions");

      -- Check values of config variables after expansion of expressions
      -- to make sure that $-references within <config> have been resolved.

      Mucfgcheck.Config.Config_Boolean_Values (XML_Data => Policy);
      Mucfgcheck.Config.Config_Integer_Values (XML_Data => Policy);
      Mucfgcheck.Validation_Errors.Check;

      Mulog.Log (Msg => "Processing attributes");
      Mutools.Substitutions.Process_Attributes (Data         => Policy,
                                                Debug_Active => Debug_Active);

      --  Check conditional references after expression evaluation as
      --  conditionals may refer to expressions

      Mucfgcheck.Config.Conditional_Config_Var_Refs (XML_Data => Policy);
      Mucfgcheck.Validation_Errors.Check;

      Mulog.Log (Msg => "Processing conditionals");
      Mutools.Conditionals.Expand (Policy       => Policy,
                                   Debug_Active => Debug_Active);

      --  Amend statements must be evaluated after conditionals as <amend> might
      --  be inside of a conditional 'C' and have effects outside of C;

      begin
         Mulog.Log (Msg => "Processing amend");
         Mutools.Amend.Expand (XML_Data     => Policy,
                               Debug_Active => Debug_Active);
      exception
         -- for exceptions during amends, the current state of Policy
         -- is helpful for debugging. Hence, we write it.

         when others =>
            if Debug_Active then
               Muxml.Write
                  (File => Output_File & "_Policy_Before_Amend_Exception.xml",
                   Kind => Muxml.None,
                   Data => Policy);
            end if;
            raise;
      end;

      if Debug_Level = VERBOSE_OUTPUT then
         Mutools.Xmldebuglog.Add_Transaction_Log_As_Comment (Doc => Policy.Doc);
         Mutools.Xmldebuglog.Add_Debug_Infos_As_Comments (Doc => Policy.Doc);
      end if;

      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_Src,
         Data => Policy);
      Mulog.Log (Msg => "Successfully created policy '" & Output_File & "'");
   end Run;

end Merge;
