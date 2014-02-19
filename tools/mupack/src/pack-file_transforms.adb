--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with Ada.IO_Exceptions;
with Ada.Directories;

with Interfaces;

with Mulog;
with Mutools.Files;

with Pack.Image;
with Pack.Command_Line;

package body Pack.File_Transforms
is

   type Transform_Procedure is not null access procedure
     (File : not null access Parser.File_Entry_Type);

   procedure Null_Transform
     (File : not null access Parser.File_Entry_Type) is null;

   --  Default transform: prepend input directory to file path and normalize
   --  names. Also check that the file exists.
   procedure Default_Transform (File : not null access Parser.File_Entry_Type);

   --  Patch Linux bzImage.
   procedure Patch_Bzimage (File : not null access Parser.File_Entry_Type);

   --  Convert given ELF binary to raw object format.
   procedure To_Raw_Binary (File : not null access Parser.File_Entry_Type);

   Transforms : constant array (Parser.File_Format_Type) of Transform_Procedure
     := (Parser.Bzimage => Patch_Bzimage'Access,
         Parser.Elf     => To_Raw_Binary'Access,
         others         => Null_Transform'Access);

   -------------------------------------------------------------------------

   procedure Default_Transform (File : not null access Parser.File_Entry_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Filepath : constant String := S (File.Path) & "/" & S (File.Filename);
   begin
      if not Ada.Directories.Exists (Name => Filepath) then
         raise Pack_Error with "File '" & Filepath & "' does not exist";
      end if;
   end Default_Transform;

   -------------------------------------------------------------------------

   procedure Patch_Bzimage (File : not null access Parser.File_Entry_Type)
   is
      use Ada.Streams.Stream_IO;

      type Byte_Array is array (Natural range <>) of Interfaces.Unsigned_8;
      pragma Pack (Byte_Array);

      subtype Sector_Type is Byte_Array (0 .. 511);

      bzImage32BitEntryPoint : constant Byte_Array (0 .. 25) :=
        (16#fc#, 16#f6#, 16#86#, 16#11#,
         16#02#, 16#00#, 16#00#, 16#40#,
         16#75#, 16#10#, 16#fa#, 16#b8#,
         16#18#, 16#00#, 16#00#, 16#00#,
         16#8e#, 16#d8#, 16#8e#, 16#c0#,
         16#8e#, 16#e0#, 16#8e#, 16#e8#,
         16#8e#, 16#d0#);
      bzImage64BitEntryPoint : constant Byte_Array (0 .. 21) :=
        (16#fc#, 16#f6#, 16#86#, 16#11#,
         16#02#, 16#00#, 16#00#, 16#40#,
         16#75#, 16#0c#, 16#fa#, 16#b8#,
         16#18#, 16#00#, 16#00#, 16#00#,
         16#8e#, 16#d8#, 16#8e#, 16#c0#,
         16#8e#, 16#d0#);

      Sector        : Sector_Type;
      Setup_Sectors : Integer := 4;
      File_In       : File_Type;
      File_Out      : File_Type;
      Filepath_In   : constant String
        := S (File.Path) & "/" & S (File.Filename);
      Filename_Out  : constant String := S (File.Filename) & ".patched";
      Filepath_Out  : constant String
        := Command_Line.Get_Output_Dir & "/" & Filename_Out;
   begin
      Mulog.Log (Msg => "Processing Linux bzImage '" & Filepath_In & "' --> '"
                 & Filepath_Out & "'");

      Open (Name => Filepath_In,
            File => File_In,
            Mode => In_File);

      begin
         Sector_Type'Read (Stream (File => File_In), Sector);
         if Natural (Sector (497)) > 0 then
            Setup_Sectors := Natural (Sector (497)) + 1;
         end if;
         for I in Integer range 2 .. Setup_Sectors loop
            Sector_Type'Read (Stream (File => File_In), Sector);
         end loop;

      exception
         when Ada.IO_Exceptions.End_Error =>
            raise Transform_Error with "Unexpected file layout in bzImage '"
              & Filepath_In & "'";
      end;

      begin
         Sector_Type'Read (Stream (File => File_In), Sector);
      exception
         when End_Error => null;
      end;

      --  This is special handling for the bzImage entry point. It tries to
      --  load segment selectors from the GDT, but fails since Muen doesn't
      --  setup that table at all.
      --  On the upside, it configures the segment selectors properly, so we
      --  don't need this here.
      --  Note: There's a Linux bootparams flag "KEEP_SEGMENTS" that would work
      --        similarily. Unfortunately it also disables later segment
      --        configuration code that we _do_ need.
      --  We assume that there are only two entry points, one for each of 32bit
      --  and 64bit mode images.

      if Sector (bzImage32BitEntryPoint'Range) = bzImage32BitEntryPoint then

         --  Overwrite start of 32-bit bzImage entry point with NOP.

         Mulog.Log (Msg => "Patching 32-bit entry point in '"
                    & Filepath_Out & "'");
         Sector (bzImage32BitEntryPoint'Range) := (others => 16#90#);
      elsif Sector (bzImage64BitEntryPoint'Range) = bzImage64BitEntryPoint then

         --  Overwrite start of 64-bit bzImage entry point with NOP.

         Mulog.Log (Msg => "Patching 64-bit entry point in '"
                    & Filepath_Out & "'");
         Sector (bzImage64BitEntryPoint'Range) := (others => 16#90#);
      else
         raise Transform_Error with "Unable to find entry point in bzImage '"
           & Filepath_In & "'";
      end if;

      Mutools.Files.Open (Filename => Filepath_Out,
                          File     => File_Out);
      Sector_Type'Write (Stream (File => File_Out), Sector);

      while not Ada.Streams.Stream_IO.End_Of_File (File_In) loop
         begin
            Sector_Type'Read (Stream (File => File_In), Sector);
         exception
            when End_Error => null;
         end;
         Sector_Type'Write (Stream (File => File_Out), Sector);
      end loop;
      Close (File => File_In);
      Close (File => File_Out);

      --  Update file reference.

      File.Filename := U (Filename_Out);
      File.Path     := U (Command_Line.Get_Output_Dir);
   end Patch_Bzimage;

   -------------------------------------------------------------------------

   procedure Process (Files : in out Parser.File_Array)
   is
   begin
      for I in Files'Range loop
         Default_Transform (File => Files (I)'Access);
         Transforms (Files (I).Format) (File => Files (I)'Access);
      end loop;
   end Process;

   -------------------------------------------------------------------------

   procedure To_Raw_Binary (File : not null access Parser.File_Entry_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Input_Path   : constant String := S (File.Path) & "/"
        & S (File.Filename);
      Output_Fname : constant String := S (File.Filename) & ".bin";
      Output_Path  : constant String := Command_Line.Get_Output_Dir & "/"
        & Output_Fname;
   begin
      Mulog.Log (Msg => "Converting file '" & Input_Path & "' from ELF to raw "
                 & "binary '" & Output_Path & "'");
      Image.To_Binary (Src_Elf => Input_Path,
                       Dst_Bin => Output_Path);

      --  Update file reference.

      File.Filename := U (Output_Fname);
      File.Path     := U (Command_Line.Get_Output_Dir);
      File.Format   := Parser.Bin_Raw;
   end To_Raw_Binary;

end Pack.File_Transforms;
