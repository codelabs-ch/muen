--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Test_Utils
is

   package D_IO is new Ada.Direct_IO (Element_Type => Character);

   -------------------------------------------------------------------------

   function Equal_Files
     (Filename1 : String;
      Filename2 : String)
      return Boolean
   is
      use type D_IO.Count;

      procedure Open_File
        (Filename :     String;
         File     : out D_IO.File_Type);
      --  Open file specified by filename.

      procedure Open_File
        (Filename :     String;
         File     : out D_IO.File_Type)
      is
      begin
         D_IO.Open (File => File,
                    Mode => D_IO.In_File,
                    Name => Filename,
                    Form => "shared=no");

      exception
         when others =>
            raise Open_File_Error with
              "Unable to open file '" & Filename & "'";
      end Open_File;

      File1, File2 : D_IO.File_Type;
      Char1, Char2 : Character;
      Result       : Boolean := True;
   begin
      Open_File (Filename => Filename1,
                 File     => File1);
      Open_File (Filename => Filename2,
                 File     => File2);

      if D_IO.Size (File1) /= D_IO.Size (File2) then
         D_IO.Close (File => File1);
         D_IO.Close (File => File2);
         return False;
      end if;

      while not D_IO.End_Of_File (File => File1) loop

         --  Read one byte from both files.

         D_IO.Read (File => File1,
                    Item => Char1);
         D_IO.Read (File => File2,
                    Item => Char2);

         if Char1 /= Char2 then
            Result := False;
         end if;
      end loop;

      D_IO.Close (File => File1);
      D_IO.Close (File => File2);

      return Result;
   end Equal_Files;

   -------------------------------------------------------------------------

   function Read_File (Filename : String) return String
   is
      File : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open
           (File => File,
            Mode => Ada.Text_IO.In_File,
            Name => Filename,
            Form => "shared=no");

      exception
         when others =>
            raise Open_File_Error with
              "Unable to open file '" & Filename & "'";
      end;

      declare
         use Ada.Strings.Unbounded;

         Data : Unbounded_String;
      begin
         Read_Data :
         loop
            Data := Data & Ada.Text_IO.Get_Line (File => File);

            if Ada.Text_IO.End_Of_File (File => File) then
               exit Read_Data;
            end if;

            Data := Data & ASCII.LF;
         end loop Read_Data;

         Ada.Text_IO.Close (File);
         return To_String (Source => Data);

      exception
         when others =>
            Ada.Text_IO.Close (File);
            raise IO_Error with "Error reading data from file '"
              & Filename & "'";
      end;
   end Read_File;

end Test_Utils;
