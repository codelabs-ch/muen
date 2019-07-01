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

with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Test_Utils
is

   -------------------------------------------------------------------------

   function Equal_Files
     (Filename1 : String;
      Filename2 : String)
      return Boolean
   is
      package S_IO renames Ada.Streams.Stream_IO;
      use type S_IO.Count;

      --  Open file specified by filename.
      procedure Open_File
        (Filename :     String;
         File     : out S_IO.File_Type);

      ----------------------------------------------------------------------

      procedure Open_File
        (Filename :     String;
         File     : out S_IO.File_Type)
      is
      begin
         S_IO.Open (File => File,
                    Mode => S_IO.In_File,
                    Name => Filename,
                    Form => "shared=no");

      exception
         when others =>
            raise Open_File_Error with
              "Unable to open file '" & Filename & "'";
      end Open_File;

      File1, File2 : S_IO.File_Type;
      Result       : Boolean := True;
   begin
      Open_File (Filename => Filename1,
                 File     => File1);
      Open_File (Filename => Filename2,
                 File     => File2);

      if S_IO.Size (File1) /= S_IO.Size (File2) then
         S_IO.Close (File => File1);
         S_IO.Close (File => File2);
         return False;
      end if;

      while Result and not S_IO.End_Of_File (File => File1) loop
         declare
            use type Ada.Streams.Stream_Element_Array;

            Buffer_F1, Buffer_F2 : Ada.Streams.Stream_Element_Array (1 .. 8);
            Last_1, Last_2 : Ada.Streams.Stream_Element_Offset;
         begin
            S_IO.Read (File => File1,
                       Item => Buffer_F1,
                       Last => Last_1);
            S_IO.Read (File => File2,
                       Item => Buffer_F2,
                       Last => Last_2);

            if Buffer_F1 (Buffer_F1'First .. Last_1)
              /= Buffer_F2 (Buffer_F2'First .. Last_2)
            then
               Result := False;
            end if;
         end;
      end loop;

      S_IO.Close (File => File1);
      S_IO.Close (File => File2);

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
