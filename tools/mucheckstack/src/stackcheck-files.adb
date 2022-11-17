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

with GNAT.Strings;

with GNATCOLL.Projects;

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
      use type GNAT.Strings.String_List_Access;
      use GNATCOLL.VFS;

      Bind_File_Prefix : constant String := "b__";

      Obj_Dir   : constant Virtual_File := Project.Object_Dir;
      Src_Files : File_Array_Access     := Project.Source_Files;
      Mains     : GNAT.Strings.String_List_Access
        := Project.Attribute_Value
          (Attribute    => GNATCOLL.Projects.Main_Attribute,
           Use_Extended => True);
   begin
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

      if Project.Has_Attribute (Attribute => GNATCOLL.Projects.Main_Attribute)
        and then Mains /= null
      then

         --  Add CI from bind file.

         for I in Mains'Range loop
            declare
               Main    : constant String
                 := Ada.Directories.Base_Name (Name => Mains (I).all);
               CI_File : constant Unbounded_String
                 := To_Unbounded_String
                   (Obj_Dir.Display_Full_Name & Bind_File_Prefix
                    & Main & ".ci");
            begin
               if not File_Set.Contains (Item => CI_File)
                 and then Ada.Directories.Exists (Name => To_String (CI_File))
               then
                  File_Set.Insert (New_Item => CI_File);
               end if;
            end;
         end loop;

         GNAT.Strings.Free (Mains);
      end if;
   end Append_CI_Files;

   -------------------------------------------------------------------------

   procedure For_Each_File
     (Files   : Path_Names;
      Process : not null access procedure (File : Ada.Text_IO.File_Type))
   is
   begin
      for F of Files loop
         declare
            Fname : constant String := To_String (F);
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
         end;
      end loop;
   end For_Each_File;

   -------------------------------------------------------------------------

   function Get_Control_Flow_Info_Files (GPR_File : String) return Path_Names
   is
      Tree     : GNATCOLL.Projects.Project_Tree;
      File_Set : SOUS.Set;
   begin
      Load_Tree (Tree    => Tree,
                 Project => GPR_File);

      declare
         use GNATCOLL.Projects;

         Iterator    : Project_Iterator
           := Tree.Root_Project.Start (Include_Extended => False);
         Cur_Project : Project_Type;
      begin
         loop
            Cur_Project := Current (Iterator);
            exit when Cur_Project = No_Project;
            Append_CI_Files (File_Set => File_Set,
                             Project  => Cur_Project);
            Next (Iterator);
         end loop;
      end;

      Tree.Unload;

      declare
         Result  : Path_Names (1 .. Natural (File_Set.Length));
         Cur_Idx : SOUS.Cursor := File_Set.First;
      begin
         for Res of Result loop
            Res     := SOUS.Element (Position => Cur_Idx);
            Cur_Idx := SOUS.Next (Position => Cur_Idx);
         end loop;
         return Result;
      end;
   end Get_Control_Flow_Info_Files;

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
