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

with GNAT.OS_Lib;

with Mutools.Strings;
with Mutools.Utils;

package body Merge.Utils
is

   -------------------------------------------------------------------------

   function Lookup_File
     (Name : String;
      Dirs : String)
      return String
   is
   begin
      if GNAT.OS_Lib.Is_Absolute_Path (Name => Name) then
         return Name;
      end if;

      return Mutools.Utils.Lookup_File
        (Filename    => Name,
         Directories => Mutools.Strings.Tokenize (Str => Dirs));
   end Lookup_File;

end Merge.Utils;
