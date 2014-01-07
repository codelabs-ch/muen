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

package Mugen.Generators
is

   --  The Run procedure uses the Command_Line package to read the output
   --  directory and the path of the policy file. It then parses the policy and
   --  passes the output directory and XML data on to the given generator
   --  procedure.
   procedure Run
     (Process : not null access procedure
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type));

end Mugen.Generators;
