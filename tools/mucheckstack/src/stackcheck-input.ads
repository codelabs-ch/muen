--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Stackcheck.Types;

package Stackcheck.Input
is

   --  Parse specified data and return corresponding subprogram node. Valid is
   --  set to True if the given data contains correct subprogram node info.
   procedure Parse_Node
     (Data       :     String;
      Valid      : out Boolean;
      Subprogram : out Types.Subprogram_Type);

end Stackcheck.Input;
