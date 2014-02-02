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

with Mulog;

with Mugen.Command_Line;

package body Mugen.Generators
is

   -------------------------------------------------------------------------

   procedure Run
     (Process : not null access procedure
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type))
   is
      Data        : Muxml.XML_Data_Type;
      Out_Dir     : constant String := Command_Line.Get_Output_Dir;
      Policy_File : constant String := Command_Line.Get_Policy;
   begin
      Mulog.Log (Msg => "Using output directory '" & Out_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy_File & "'");
      Muxml.Parse (Data => Data,
                   Kind => Muxml.Format_B,
                   File => Policy_File);
      Process (Output_Dir => Out_Dir,
               Policy     => Data);
   end Run;

end Mugen.Generators;
