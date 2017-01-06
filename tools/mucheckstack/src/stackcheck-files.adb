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

with GNATCOLL.Projects;
with GNATCOLL.VFS;

package body Stackcheck.Files
is

   use Ada.Strings.Unbounded;

   Error_Message : Unbounded_String;

   --  Store given error message for later retrieval.
   procedure Save_Error_Msg (Msg : String);

   -------------------------------------------------------------------------

   function Get_Object_Dirs (GPR_File : String) return Path_Names
   is
      use GNATCOLL.VFS;

      Tree : GNATCOLL.Projects.Project_Tree;
   begin
      begin
         Error_Message := Null_Unbounded_String;
         Tree.Load (Root_Project_Path => Create (Full_Filename => +GPR_File),
                    Errors            => Save_Error_Msg'Access);

      exception
         when others =>
            Tree.Unload;
            raise IO_Error with To_String (Error_Message);
      end;

      declare
         VFS_Dirs : constant File_Array
           := Tree.Root_Project.Object_Path (Recursive => True);
         Result : Path_Names (VFS_Dirs'Range);
      begin
         for I in VFS_Dirs'Range loop
            Result (I) := To_Unbounded_String (VFS_Dirs (I).Display_Full_Name);
         end loop;

         Tree.Unload;

         return Result;
      end;
   end Get_Object_Dirs;

      ----------------------------------------------------------------------

   procedure Save_Error_Msg (Msg : String)
   is
      Msg_End : constant Natural
        := (if Msg (Msg'Last) = ASCII.LF then Msg'Last - 1 else Msg'Last);
   begin
      Error_Message := To_Unbounded_String (Msg (Msg'First .. Msg_End));
   end Save_Error_Msg;

end Stackcheck.Files;
