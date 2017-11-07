--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Interfaces;

with Mutools.Constants;

package Bin_Split.Utils
is

   --  Rounds Address upwards to nearest multiple of To (defaults to page
   --  size).
   function Round_Up
     (Address : Interfaces.Unsigned_64;
      To      : Interfaces.Unsigned_64 := Mutools.Constants.Page_Size)
      return Interfaces.Unsigned_64;

   --  Creates specified output directory if it does not exist.
   --
   --  A Bin_Split_Error exception is raised if directory cannot be created.
   procedure Make_Output_Directory (Dir_Name : String);

end Bin_Split.Utils;
