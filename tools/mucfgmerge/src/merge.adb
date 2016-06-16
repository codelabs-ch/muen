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

with Muxml;
with Mulog;

with Mergers;
with Merge.Checks;

package body Merge
is

   -------------------------------------------------------------------------

   procedure Run
     (Config_File        : String;
      Policy_File        : String;
      Platform_File      : String;
      Hardware_File      : String;
      Additional_Hw_File : String;
      Output_File        : String)
   is
      Config : Muxml.XML_Data_Type;
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Processing system config '" & Config_File & "'");
      Muxml.Parse (Data => Config,
                   Kind => Muxml.None,
                   File => Config_File);
      Checks.Required_Config_Values (Policy => Config);

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.None,
                   File => Policy_File);

      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");
      Mergers.Merge_XIncludes
        (Policy  => Policy,
         Basedir => GNAT.Directory_Operations.Dir_Name (Path => Policy_File));

      Mulog.Log (Msg => "Using hardware file '" & Hardware_File & "'");
      Mergers.Merge_Hardware
        (Policy        => Policy,
         Hardware_File => Hardware_File);
      if Additional_Hw_File'Length > 0 then
         Mulog.Log (Msg => "Using additional hardware file '"
                    & Additional_Hw_File & "'");
         Mergers.Merge_Hardware
           (Policy        => Policy,
            Hardware_File => Additional_Hw_File);
      end if;

      Mulog.Log (Msg => "Using platform file '" & Platform_File & "'");
      Mergers.Merge_Platform
        (Policy        => Policy,
         Platform_File => Platform_File);

      Muxml.Write
        (File => Output_File,
         Kind => Muxml.Format_Src,
         Data => Policy);
      Mulog.Log (Msg => "Successfully created policy '" & Output_File & "'");
   end Run;

end Merge;
