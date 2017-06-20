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

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;

with GNATCOLL.Projects;

with Mutools;

package body Stackcheck.Files
is

   use Ada.Strings.Unbounded;

   package SOUS is new Ada.Containers.Indefinite_Hashed_Sets
     (Element_Type        => Unbounded_String,
      Hash                => Ada.Strings.Unbounded.Hash,
      Equivalent_Elements => Ada.Strings.Unbounded.Equal_Case_Insensitive);

   Error_Message : Unbounded_String;

   --  Store given error message for later retrieval.
   procedure Save_Error_Msg (Msg : String);

   --  Load project tree from specified project file.
   procedure Load_Tree
     (Tree    : in out GNATCOLL.Projects.Project_Tree;
      Project :        String);

   --  Append control-flow info files of given project to specified file set.
   --  Each file added to the set is guaranteed to exist on the filesystem.
   procedure Append_CI_Files
     (File_Set : in out SOUS.Set;
      Project  :        GNATCOLL.Projects.Project_Type);

   -------------------------------------------------------------------------

   procedure Append_CI_Files
     (File_Set : in out SOUS.Set;
      Project  :        GNATCOLL.Projects.Project_Type)
   is
      use GNATCOLL.VFS;

      Obj_Dir   : constant Virtual_File := Project.Object_Dir;
      Src_Files : File_Array_Access     := Project.Source_Files;
   begin
      if Src_Files = null then
         return;
      end if;

      for Src of Src_Files.all loop
         declare
            Basename : constant Filesystem_String
              := Base_Name
                (File      => Src,
                 Suffix    => File_Extension (File => Src),
                 Normalize => True);
            CI_File  : constant Unbounded_String
              := To_Unbounded_String
                (Obj_Dir.Display_Full_Name & (+Basename) & ".ci");
         begin
            if not File_Set.Contains (Item => CI_File)
              and then Ada.Directories.Exists (Name => To_String (CI_File))
            then
               File_Set.Insert (New_Item => CI_File);
            end if;
         end;
      end loop;

      Unchecked_Free (Arr => Src_Files);
   end Append_CI_Files;

   -------------------------------------------------------------------------

   procedure For_Each_File
     (Path    : String;
      Pattern : String;
      Process : not null access procedure (File : Ada.Text_IO.File_Type))
   is
      --  Call process for the given directory entry.
      procedure Process_Entry (Dir : Ada.Directories.Directory_Entry_Type);

      ----------------------------------------------------------------------

      procedure Process_Entry (Dir : Ada.Directories.Directory_Entry_Type)
      is
         Fname : constant String := Ada.Directories.Full_Name
           (Directory_Entry => Dir);
         File  : Ada.Text_IO.File_Type;
      begin
         begin
            Ada.Text_IO.Open (File => File,
                              Mode => Ada.Text_IO.In_File,
                              Name => Fname);

         exception
            when E : others =>
               raise IO_Error with "Unable to open file '" & Fname
                 & "' - " & Ada.Exceptions.Exception_Message (X => E);
         end;

         begin
            Process (File => File);

         exception
            when others =>
               Ada.Text_IO.Close (File => File);
               raise;
         end;

         Ada.Text_IO.Close (File => File);
      end Process_Entry;

      Dir_Exists : Boolean;
   begin
      begin
         Dir_Exists := Ada.Directories.Exists (Name => Path);
      exception
         when others =>
            raise IO_Error with "Invalid directory name '" & Path & "'";
      end;

      if not Dir_Exists then
         raise IO_Error with "Directory '" & Path & "' does not exist";
      end if;

      Ada.Directories.Search
        (Directory => Path,
         Pattern   => Pattern,
         Filter    => (Ada.Directories.Ordinary_File => True, others => False),
         Process   => Process_Entry'Access);
   end For_Each_File;

   -------------------------------------------------------------------------

   function Get_Object_Dirs (GPR_File : String) return Path_Names
   is
      use GNATCOLL.VFS;

      Tree : GNATCOLL.Projects.Project_Tree;
   begin
      Load_Tree (Tree    => Tree,
                 Project => GPR_File);

      declare
         VFS_Dirs : constant File_Array
           := Tree.Root_Project.Object_Path (Recursive => True);
      begin
         Tree.Unload;

         return To_Path_Names (Files => VFS_Dirs);
      end;
   end Get_Object_Dirs;

   -------------------------------------------------------------------------

   procedure Load_Tree
     (Tree    : in out GNATCOLL.Projects.Project_Tree;
      Project :        String)
   is
      use GNATCOLL.VFS;
   begin
      Error_Message := Null_Unbounded_String;
      Tree.Load (Root_Project_Path => Create (Full_Filename => +Project),
                 Errors            => Save_Error_Msg'Access);

   exception
      when others =>
         Tree.Unload;
         raise IO_Error with To_String (Error_Message);
   end Load_Tree;

   -------------------------------------------------------------------------

   procedure Save_Error_Msg (Msg : String)
   is
      Msg_End : constant Natural
        := (if Msg (Msg'Last) = ASCII.LF then Msg'Last - 1 else Msg'Last);
   begin
      Error_Message := To_Unbounded_String (Msg (Msg'First .. Msg_End));
   end Save_Error_Msg;

   -------------------------------------------------------------------------

   function To_Path_Names (Files : GNATCOLL.VFS.File_Array) return Path_Names
   is
      Result : Path_Names (Files'Range);
   begin
      for I in Files'Range loop
         Result (I) := To_Unbounded_String (Files (I).Display_Full_Name);
      end loop;

      return Result;
   end To_Path_Names;

end Stackcheck.Files;
