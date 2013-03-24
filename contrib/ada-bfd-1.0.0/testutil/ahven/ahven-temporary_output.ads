--
-- Copyright (c) 2007, 2008, 2010 Tero Koskinen <tero.koskinen@iki.fi>
--
-- Permission to use, copy, modify, and distribute this software for any
-- purpose with or without fee is hereby granted, provided that the above
-- copyright notice and this permission notice appear in all copies.
--
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
--
with Ahven.AStrings;
with Ada.Text_IO;

package Ahven.Temporary_Output is
   Temporary_File_Error : exception;

   type Temporary_File is limited private;

   procedure Create_Temp (File : out Temporary_File);
   -- Create a new temporary file. Exception Temporary_File_Error
   -- is raised if the procedure cannot create a new temp file.

   function Get_Name (File : Temporary_File) return String;
   -- Return the name of the file.

   procedure Redirect_Output (To_File : in out Temporary_File);
   -- Redirect the standard output to the file.
   -- To_File must be opened using Create_Temp.

   procedure Restore_Output;
   -- Restore the standard output to its default settings.

   procedure Remove_Temp (File : in out Temporary_File);
   -- Remove the temporary file. File can be either open or closed.

   procedure Close_Temp (File : in out Temporary_File);
   -- Close the temporary file.

private
   type Temporary_File is limited record
      Name   : AStrings.Bounded_String;
      Handle : Ada.Text_IO.File_Type;
   end record;

end Ahven.Temporary_Output;
