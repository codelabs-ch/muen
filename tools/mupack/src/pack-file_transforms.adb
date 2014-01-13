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

with Ada.Strings.Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;

with SK;

with Mulog;

with Pack.Image;
with Pack.Command_Line;

package body Pack.File_Transforms
is

   type Transform_Procedure is not null access procedure
     (File : access Parser.File_Entry_Type);

   --  Normalize name field of file entry.
   function Normalize (Name : String) return String;

   --  Default transform: prepend input directory to file path and normalize
   --  names. This transform must be called from the other transform
   --  implementations first.
   procedure Default_Transform (File : access Parser.File_Entry_Type);

   --  Patch Linux bzImage.
   procedure Patch_Bzimage (File : access Parser.File_Entry_Type);

   --  Convert given ELF binary to raw object format.
   procedure To_Raw_Binary (File : access Parser.File_Entry_Type);

   Transforms : constant array (Parser.File_Format_Type) of Transform_Procedure
     := (Parser.Bzimage => Patch_Bzimage'Access,
         Parser.Elf     => To_Raw_Binary'Access,
         others         => Default_Transform'Access);

   -------------------------------------------------------------------------

   procedure Default_Transform (File : access Parser.File_Entry_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      File.Path := Command_Line.Get_Input_Dir & "/" & File.Path;
      File.Name := U (Normalize (Name => S (File.Name)));
   end Default_Transform;

   -------------------------------------------------------------------------

   function Normalize (Name : String) return String
   is
      Charmap : constant Ada.Strings.Maps.Character_Mapping
        := Ada.Strings.Maps.To_Mapping (From => "|",
                                        To   => "_");
   begin
      return Ada.Strings.Fixed.Translate
        (Source  => Name,
         Mapping => Charmap);
   end Normalize;

   -------------------------------------------------------------------------

   procedure Patch_Bzimage (File : access Parser.File_Entry_Type)
   is
      use Ada.Streams.Stream_IO;
      use type SK.Byte;

      type Byte_Array is array (Natural range <>) of SK.Byte;
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
      Filename_Out  : constant String
        := Command_Line.Get_Output_Dir & "/" & S (File.Path) & ".patched";
   begin
      Default_Transform (File => File);

      Mulog.Log (Msg => "Processing Linux bzImage '" & S (File.Path)
                 & "' --> '" & Filename_Out & "'");

      Open (Name => S (File.Path),
            File => File_In,
            Mode => In_File);
      Create (Name => Filename_Out,
              File => File_Out,
              Mode => Out_File);

      Sector_Type'Read (Stream (File => File_In), Sector);
      if Natural (Sector (497)) > 0 then
         Setup_Sectors := Natural (Sector (497)) + 1;
      end if;
      for I in Integer range 2 .. Setup_Sectors loop
         Sector_Type'Read (Stream (File => File_In), Sector);
      end loop;

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
                    & Filename_Out & "'");
         Sector (bzImage32BitEntryPoint'Range) := (others => 16#90#);
      elsif Sector (bzImage64BitEntryPoint'Range) = bzImage64BitEntryPoint then

         --  Overwrite start of 64-bit bzImage entry point with NOP.

         Mulog.Log (Msg => "Patching 64-bit entry point in '"
                    & Filename_Out & "'");
         Sector (bzImage64BitEntryPoint'Range) := (others => 16#90#);
      end if;

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

      --  Update path.

      File.Path := U (Filename_Out);
   end Patch_Bzimage;

   -------------------------------------------------------------------------

   procedure Process (Files : in out Parser.File_Array)
   is
   begin
      for I in Files'Range loop
         Transforms (Files (I).Format) (File => Files (I)'Access);
      end loop;
   end Process;

   -------------------------------------------------------------------------

   procedure To_Raw_Binary (File : access Parser.File_Entry_Type)
   is
      use type Ada.Strings.Unbounded.Unbounded_String;

      Output : constant String := Command_Line.Get_Output_Dir & "/"
        & S (File.Path) & ".bin";
   begin
      Default_Transform (File => File);

      Mulog.Log (Msg => "Converting file '" & S (File.Path) & "' from ELF to "
                 & "raw binary '" & Output & "'");
      Image.To_Binary (Src_Elf => S (File.Path),
                       Dst_Bin => Output);

      --  Update path.

      File.Path := U (Output);
   end To_Raw_Binary;

end Pack.File_Transforms;
