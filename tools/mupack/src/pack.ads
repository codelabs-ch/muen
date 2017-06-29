--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Pack
is

   --  Start packaging of the system specified by given policy. Files
   --  referenced in the policy are expected to be found in the given input
   --  directory, while intermediate files and the resulting system image are
   --  created in the given output directory.
   procedure Run
     (Policy_File    : String;
      Input_Dir      : String;
      Output_Dir     : String;
      Output_Imgname : String;
      Dry_Run        : Boolean);

   Pack_Error : exception;

end Pack;
