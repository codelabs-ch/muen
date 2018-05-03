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

with Ada.Directories;
with Ada.Exceptions;

package body Mutools.Files
is

   -------------------------------------------------------------------------

   procedure Open
     (Filename :     String;
      File     : out Ada.Streams.Stream_IO.File_Type;
      Writable :     Boolean := True)
   is
      use type Ada.Directories.File_Kind;
   begin
      if Writable then
         Ada.Streams.Stream_IO.Create
           (File => File,
            Mode => Ada.Streams.Stream_IO.Out_File,
            Name => Filename);
      else
         if not Ada.Directories.Exists (Name => Filename) then
            raise IO_Error with "File does not exist";
         end if;

         if Ada.Directories.Kind
           (Name => Filename) /= Ada.Directories.Ordinary_File
         then
            raise IO_Error with "File is not a regular file";
         end if;

         Ada.Streams.Stream_IO.Open
           (File => File,
            Mode => Ada.Streams.Stream_IO.In_File,
            Name => Filename);
      end if;

   exception
      when E : others =>
         raise IO_Error with "Unable to open file '" & Filename & "' - "
           & Ada.Exceptions.Exception_Message (X => E);
   end Open;

end Mutools.Files;
