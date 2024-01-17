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

package Xmlfilter
is
   --  The main procedure that
   --  parses the input file, deletes all nodes which must not be present
   --  according to Target_Schema_Name and writes the result to Output_Xml_Path.
   procedure Run
     (Input_Xml_Path     : String;
      Input_Schema_Name  : String;
      Output_Xml_Path    : String;
      Output_Schema_Name : String;
      Output_Schema_Path : String);

   Validation_Error : exception;

end Xmlfilter;
