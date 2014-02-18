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

with Ada.Exceptions;
with Ada.Directories;
with Ada.Strings.Unbounded;

with Pack.Parser;
with Pack.File_Transforms;

with Pack.Command_Line.Test;

with Test_Utils;

package body Transform_Tests
is

   use Ahven;
   use Pack;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   -------------------------------------------------------------------------

   procedure Default_Transform
   is
      use type Parser.File_Array;

      Files : Parser.File_Array
        := (1 => (Path    => U ("obj1.o"),
                  Address => 16#0010_0000#,
                  Size    => 16#0001_3000#,
                  Offset  => 0,
                  Format  => Parser.Elf),
            2 => (Path    => U ("obj2.o"),
                  Address => 16#0011_3000#,
                  Size    => 16#0001_3000#,
                  Offset  => 0,
                  Format  => Parser.Acpi_Rsdp));
      Ref_Files : constant Parser.File_Array
        := (1 => (Path    => U ("obj/obj1.o.bin"),
                  Address => 16#0010_0000#,
                  Size    => 16#0001_3000#,
                  Offset  => 0,
                  Format  => Parser.Elf),
            2 => (Path    => U ("data/obj2.o"),
                  Address => 16#0011_3000#,
                  Size    => 16#0001_3000#,
                  Offset  => 0,
                  Format  => Parser.Acpi_Rsdp));
   begin
      Command_Line.Test.Set_Input_Dir  (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");

      File_Transforms.Process (Files => Files);
      Assert (Condition => Ref_Files = Files,
              Message   => "Transformed files mismatch");

      Ada.Directories.Delete_File (Name => "obj/obj1.o.bin");
   end Default_Transform;

   -------------------------------------------------------------------------

   procedure Default_Transform_Nonexistent
   is
      use type Parser.File_Array;

      Files : Parser.File_Array
        := (1 => (Path    => U ("nonexistent.o"),
                  Address => 16#0010_0000#,
                  Size    => 16#0001_3000#,
                  Offset  => 0,
                  Format  => Parser.Iobm));
   begin
      Command_Line.Test.Set_Input_Dir  (Path => "data");

      File_Transforms.Process (Files => Files);
      Fail (Message => "Exception expected");

   exception
      when Pack.Pack_Error => null;
   end Default_Transform_Nonexistent;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "File transform tests");
      T.Add_Test_Routine
        (Routine => Default_Transform'Access,
         Name    => "Default transform");
      T.Add_Test_Routine
        (Routine => Default_Transform_Nonexistent'Access,
         Name    => "Default transform (nonexistent file)");
      T.Add_Test_Routine
        (Routine => Patch_Bzimage'Access,
         Name    => "Patch Linux bzImage");
      T.Add_Test_Routine
        (Routine => Invalid_Bzimage'Access,
         Name    => "Invalid Linux bzImage");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Invalid_Bzimage
   is
   begin
      Command_Line.Test.Set_Input_Dir  (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");

      declare
         Files : Parser.File_Array
           := (1 => (Path    => U ("invalid_bzimage"),
                     Address => 16#0010_0000#,
                     Size    => 16#0001_3000#,
                     Offset  => 0,
                     Format  => Parser.Bzimage));
      begin
         File_Transforms.Process (Files => Files);

      exception
         when E : File_Transforms.Transform_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unable to find entry point in bzImage "
                    & "'data/invalid_bzimage'",
                    Message   => "Exception mismatch (1)");
      end;

      declare
         Files : Parser.File_Array
           := (1 => (Path    => U ("obj1.o"),
                     Address => 16#0010_0000#,
                     Size    => 16#0001_3000#,
                     Offset  => 0,
                     Format  => Parser.Bzimage));
      begin
         File_Transforms.Process (Files => Files);

      exception
         when E : File_Transforms.Transform_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Unexpected file layout in bzImage 'data/obj1.o'",
                    Message   => "Exception mismatch (2)");
      end;
   end Invalid_Bzimage;

   -------------------------------------------------------------------------

   procedure Patch_Bzimage
   is
   begin
      Command_Line.Test.Set_Input_Dir  (Path => "data");
      Command_Line.Test.Set_Output_Dir (Path => "obj");

      declare
         Files : Parser.File_Array
           := (1 => (Path    => U ("bzimage.32"),
                     Address => 16#0010_0000#,
                     Size    => 16#0001_3000#,
                     Offset  => 0,
                     Format  => Parser.Bzimage));

      begin
         File_Transforms.Process (Files => Files);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/bzimage.32.ref",
                  Filename2 => "obj/bzimage.32.patched"),
                 Message   => "Mismatch in 32-bit image");
         Ada.Directories.Delete_File (Name => "obj/bzimage.32.patched");
      end;

      declare
         Files : Parser.File_Array
           := (1 => (Path    => U ("bzimage.64"),
                     Address => 16#0010_0000#,
                     Size    => 16#0001_3000#,
                     Offset  => 0,
                     Format  => Parser.Bzimage));

      begin
         File_Transforms.Process (Files => Files);
         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/bzimage.64.ref",
                  Filename2 => "obj/bzimage.64.patched"),
                 Message   => "Mismatch in 64-bit image");
         Ada.Directories.Delete_File (Name => "obj/bzimage.64.patched");
      end;
   end Patch_Bzimage;

end Transform_Tests;
