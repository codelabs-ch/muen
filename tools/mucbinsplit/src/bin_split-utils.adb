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

with Ada.Directories;
with Ada.Exceptions;

with Interfaces;
use type Interfaces.Unsigned_64;

with Mutools.Constants;

package body Bin_Split.Utils is

   procedure Make_Output_Directory (Dir_Name : String)
   is
      use type Ada.Directories.File_Kind;
   begin
      if Dir_Name = "" then
         return;
      elsif Ada.Directories.Exists (Name => Dir_Name) then
         if Ada.Directories.Kind (Dir_Name) /= Ada.Directories.Directory then
            raise Bin_Split_Error
              with "Cannot create output directory '" & Dir_Name
                     & "': File exists";
         end if;
      else
         Ada.Directories.Create_Directory (New_Directory => Dir_Name);
      end if;

   exception
      when E : Ada.Directories.Use_Error =>
         raise Bin_Split_Error with Ada.Exceptions.Exception_Message (X => E);
   end Make_Output_Directory;

   --------------------------------------------------------------------------

   function Round_To_Page
     (Address : Interfaces.Unsigned_64)
      return Interfaces.Unsigned_64
   is
      P : constant Interfaces.Unsigned_64 := Mutools.Constants.Page_Size;
   begin
      return ((Address + P - 1) / P) * P;
   end Round_To_Page;

end Bin_Split.Utils;
