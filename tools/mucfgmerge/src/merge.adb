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

with Muxml;
with Mulog;

with Mergers;

package body Merge
is

   -------------------------------------------------------------------------

   procedure Run
     (Policy_File   : String;
      Platform_File : String;
      Output_File   : String)
   is
      Policy : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Using platform file '" & Platform_File & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");

      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => Policy_File);

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
