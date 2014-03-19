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

with Bzpatch;

with Test_Utils;

package body Patch_Tests
is

   use Ahven;
   use Bzpatch;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Patch tests");
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
      Patch (Input  => "data/invalid_bzimage",
             Output => "obj/foobar");
      Fail (Message => "Exception expected");

   exception
      when E : Patch_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Unable to find entry point in bzImage "
                 & "'data/invalid_bzimage'",
                 Message   => "Exception mismatch");
   end Invalid_Bzimage;

   -------------------------------------------------------------------------

   procedure Patch_Bzimage
   is
   begin
      Patch (Input  => "data/bzimage.32",
             Output => "obj/bzimage.32.patched");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bzimage.32.ref",
               Filename2 => "obj/bzimage.32.patched"),
              Message   => "Mismatch in 32-bit image");
      Ada.Directories.Delete_File (Name => "obj/bzimage.32.patched");

      Patch (Input  => "data/bzimage.64",
             Output => "obj/bzimage.64.patched");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => "data/bzimage.64.ref",
               Filename2 => "obj/bzimage.64.patched"),
              Message   => "Mismatch in 64-bit image");
      Ada.Directories.Delete_File (Name => "obj/bzimage.64.patched");
   end Patch_Bzimage;

end Patch_Tests;
