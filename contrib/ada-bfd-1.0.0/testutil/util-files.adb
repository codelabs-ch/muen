-----------------------------------------------------------------------
--  Util.Files -- Various File Utility Packages
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Util.Strings.Tokenizers;
package body Util.Files is

   --  ------------------------------
   --  Read a complete file into a string.
   --  The <b>Max_Size</b> parameter indicates the maximum size that is read.
   --  ------------------------------
   procedure Read_File (Path     : in String;
                        Into     : out Unbounded_String;
                        Max_Size : in Natural := 0) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;

      F      : File_Type;
      Buffer : Stream_Element_Array (1 .. 10_000);
      Pos    : Positive_Count := 1;
      Last   : Stream_Element_Offset;
      Space  : Natural;
   begin
      if Max_Size = 0 then
         Space := Natural'Last;
      else
         Space := Max_Size;
      end if;
      Open (Name => Path, File => F, Mode => In_File);
      loop
         Read (File => F, Item => Buffer, From => Pos, Last => Last);
         if Natural (Last) > Space then
            Last := Stream_Element_Offset (Space);
         end if;
         for I in 1 .. Last loop
            Append (Into, Character'Val (Buffer (I)));
         end loop;
         exit when Last < Buffer'Length;
         Pos := Pos + Positive_Count (Last);
      end loop;
      Close (F);

   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Read_File;

   --  ------------------------------
   --  Read the file with the given path, one line at a time and execute the <b>Process</b>
   --  procedure with each line as argument.
   --  ------------------------------
   procedure Read_File (Path     : in String;
                        Process  : not null access procedure (Line : in String)) is
      File : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (File => File,
                        Mode => Ada.Text_IO.In_File,
                        Name => Path);
      while not Ada.Text_IO.End_Of_File (File) loop
         Process (Ada.Text_IO.Get_Line (File));
      end loop;
      Ada.Text_IO.Close (File);
   end Read_File;

   --  ------------------------------
   --  Save the string into a file creating the file if necessary
   --  ------------------------------
   procedure Write_File (Path    : in String;
                         Content : in String) is
      use Ada.Streams;
      use Ada.Streams.Stream_IO;
      use Ada.Directories;

      F : File_Type;
      Buffer : Stream_Element_Array (Stream_Element_Offset (Content'First)
                                     .. Stream_Element_Offset (Content'Last));

      Dir : constant String := Containing_Directory (Path);
   begin
      if not Exists (Dir) then
         Create_Path (Dir);
      end if;
      Create (File => F, Name => Path);
      for I in Content'Range loop
         Buffer (Stream_Element_Offset (I))
           := Stream_Element (Character'Pos (Content (I)));
      end loop;
      Write (F, Buffer);
      Close (F);

   exception
      when others =>
         if Is_Open (F) then
            Close (F);
         end if;
         raise;
   end Write_File;

   --  ------------------------------
   --  Save the string into a file creating the file if necessary
   --  ------------------------------
   procedure Write_File (Path    : in String;
                         Content : in Unbounded_String) is
   begin
      Write_File (Path, Ada.Strings.Unbounded.To_String (Content));
   end Write_File;

   --  ------------------------------
   --  Iterate over the search directories defined in <b>Paths</b> and execute
   --  <b>Process</b> with each directory until it returns <b>True</b> in <b>Done</b>
   --  or the last search directory is found.  Each search directory
   --  is separated by ';' (yes, even on Unix).  When <b>Going</b> is set to Backward, the
   --  directories are searched in reverse order.
   --  ------------------------------
   procedure Iterate_Path (Path     : in String;
                           Process  : not null access procedure (Dir  : in String;
                                                                 Done : out Boolean);
                           Going    : in Direction := Ada.Strings.Forward) is
   begin
      Util.Strings.Tokenizers.Iterate_Tokens (Content => Path,
                                              Pattern => ";",
                                              Process => Process,
                                              Going   => Going);
   end Iterate_Path;

   --  ------------------------------
   --  Find the file in one of the search directories.  Each search directory
   --  is separated by ';' (yes, even on Unix).
   --  Returns the path to be used for reading the file.
   --  ------------------------------
   function Find_File_Path (Name  : String;
                            Paths : String) return String is
      use Ada.Directories;
      use Ada.Strings.Fixed;

      Sep_Pos : Natural;
      Pos     : Positive := Paths'First;
      Last    : constant Natural := Paths'Last;

   begin
      while Pos <= Last loop
         Sep_Pos := Index (Paths, ";", Pos);
         if Sep_Pos = 0 then
            Sep_Pos := Last;
         else
            Sep_Pos := Sep_Pos - 1;
         end if;
         declare
            Path : constant String := Util.Files.Compose (Paths (Pos .. Sep_Pos), Name);
         begin
            if Exists (Path) and then Kind (Path) = Ordinary_File then
               return Path;
            end if;
         exception
            when Name_Error =>
               null;
         end;
         Pos := Sep_Pos + 2;
      end loop;
      return Name;
   end Find_File_Path;

   --  ------------------------------
   --  Iterate over the search directories defined in <b>Path</b> and search
   --  for files matching the pattern defined by <b>Pattern</b>.  For each file,
   --  execute <b>Process</b> with the file basename and the full file path.
   --  Stop iterating when the <b>Process</b> procedure returns True.
   --  Each search directory is separated by ';'.  When <b>Going</b> is set to Backward, the
   --  directories are searched in reverse order.
   --  ------------------------------
   procedure Iterate_Files_Path (Pattern  : in String;
                                 Path     : in String;
                                 Process  : not null access procedure (Name : in String;
                                                                       File : in String;
                                                                       Done : out Boolean);
                                 Going    : in Direction := Ada.Strings.Forward) is

      procedure Find_Files (Dir  : in String;
                            Done : out Boolean);

      --  ------------------------------
      --  Find the files matching the pattern in <b>Dir</b>.
      --  ------------------------------
      procedure Find_Files (Dir  : in String;
                            Done : out Boolean) is
         use Ada.Directories;

         Filter  : constant Filter_Type := (Ordinary_File => True, others => False);
         Ent     : Directory_Entry_Type;
         Search  : Search_Type;
      begin
         Done := False;
         Start_Search (Search, Directory => Dir,
                       Pattern => Pattern, Filter => Filter);
         while More_Entries (Search) loop
            Get_Next_Entry (Search, Ent);
            declare
               Name      : constant String := Simple_Name (Ent);
               File_Path : constant String := Full_Name (Ent);
            begin
               Process (Name, File_Path, Done);
               exit when Done;
            end;
         end loop;
      end Find_Files;

   begin
      Iterate_Path (Path => Path, Process => Find_Files'Access, Going => Going);
   end Iterate_Files_Path;

   --  ------------------------------
   --  Find the files which match the pattern in the directories specified in the
   --  search path <b>Path</b>.  Each search directory is separated by ';'.
   --  File names are added to the string set in <b>Into</b>.
   --  ------------------------------
   procedure Find_Files_Path (Pattern : in String;
                              Path    : in String;
                              Into    : in out Util.Strings.Maps.Map) is

      procedure Add_File (Name : in String;
                          File_Path : in String;
                          Done : out Boolean);

      --  ------------------------------
      --  Find the files matching the pattern in <b>Dir</b>.
      --  ------------------------------
      procedure Add_File (Name : in String;
                          File_Path : in String;
                          Done      : out Boolean) is
      begin
         if not Into.Contains (Name) then
            Into.Insert (Name, File_Path);
         end if;
         Done := False;
      end Add_File;

   begin
      Iterate_Files_Path (Pattern => Pattern, Path => Path, Process => Add_File'Access);
   end Find_Files_Path;

   --  ------------------------------
   --  Compose an existing path by adding the specified name to each path component
   --  and return a new paths having only existing directories.  Each directory is
   --  separated by ';'.
   --  If the composed path exists, it is added to the result path.
   --  Example:
   --    paths = 'web;regtests'  name = 'info'
   --    result = 'web/info;regtests/info'
   --  Returns the composed path.
   --  ------------------------------
   function Compose_Path (Paths : in String;
                          Name  : in String) return String is

      procedure Compose (Dir : in String;
                         Done : out Boolean);

      Result  : Unbounded_String;

      --  ------------------------------
      --  Build the new path by checking if <b>Name</b> exists in <b>Dir</b>
      --  and appending the new path in the <b>Result</b>.
      --  ------------------------------
      procedure Compose (Dir : in String;
                         Done : out Boolean) is
         use Ada.Directories;
         use Ada.Strings.Fixed;

         Path : constant String := Util.Files.Compose (Dir, Name);
      begin
         Done := False;
         if Exists (Path) and then Kind (Path) = Directory then
            if Length (Result) > 0 then
               Append (Result, ';');
            end if;
            Append (Result, Path);
         end if;
      exception
         when Name_Error =>
            null;
      end Compose;

   begin
      Iterate_Path (Path => Paths, Process => Compose'Access);
      return To_String (Result);
   end Compose_Path;

   --  ------------------------------
   --  Returns the name of the external file with the specified directory
   --  and the name.  Unlike the Ada.Directories.Compose, the name can represent
   --  a relative path and thus include directory separators.
   --  ------------------------------
   function Compose (Directory : in String;
                     Name      : in String) return String is
   begin
      if Name'Length = 0 then
         return Directory;
      elsif Directory'Length = 0 or Directory = "." or Directory = "./" then
         return Name;
      elsif Directory (Directory'Last) = '/' and Name (Name'First) = '/' then
         return Directory & Name (Name'First + 1 .. Name'Last);
      elsif Directory (Directory'Last) = '/' or Name (Name'First) = '/' then
         return Directory & Name;
      else
         return Directory & "/" & Name;
      end if;
   end Compose;

   --  ------------------------------
   --  Returns a relative path whose origin is defined by <b>From</b> and which refers
   --  to the absolute path referenced by <b>To</b>.  Both <b>From</b> and <b>To</b> are
   --  assumed to be absolute pathes.  Returns the absolute path <b>To</b> if the relative
   --  path could not be found.  Both paths must have at least one root component in common.
   --  ------------------------------
   function Get_Relative_Path (From : in String;
                               To   : in String) return String is
      Result : Unbounded_String;
      Last   : Natural := 0;
   begin
      for I in From'Range loop
         if I > To'Last or else From (I) /= To (I) then
            --  Nothing in common, return the absolute path <b>To</b>.
            if Last <= From'First + 1 then
               return To;
            end if;

            for J in Last .. From'Last - 1 loop
               if From (J) = '/' or From (J) = '\' then
                  Append (Result, "../");
               end if;
            end loop;
            if Last <= To'Last and From (I) /= '/' and From (I) /= '\' then
               Append (Result, "../");
               Append (Result, To (Last .. To'Last));
            end if;
            return To_String (Result);

         elsif I < From'Last and then (From (I) = '/' or From (I) = '\') then
            Last := I + 1;

         end if;
      end loop;
      if To'Last = From'Last or (To'Last = From'Last + 1
                                 and (To (To'Last) = '/' or To (To'Last) = '\')) then
         return ".";
      elsif Last = 0 then
         return To;
      elsif To (From'Last + 1) = '/' or To (From'Last + 1) = '\' then
         return To (From'Last + 2 .. To'Last);
      else
         return To (Last .. To'Last);
      end if;
   end Get_Relative_Path;

end Util.Files;
