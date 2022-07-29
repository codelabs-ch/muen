--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Merge
is
   -- Debugging levels:
   -- NONE ............ Normal error messages without xml-traceback (i.e., why
   --                   individual xml-nodes were created)
   -- VERBOSE_ERRORS .. adds xml-traceback information to error messages
   -- VERBOSE_OUTPUT .. VERBOSE_ERROR + xml-traceback information as comments in
   --                   output file
   type Debug_Level_Type is (NONE, VERBOSE_ERRORS, VERBOSE_OUTPUT);

   --  Start the merge process.
   procedure Run
     (Config_File  : String;
      Output_File  : String;
      Include_Path : String;
      Debug_Level  : Debug_Level_Type);

end Merge;
