--
--  Copyright (C) 2017  secunet Security Networks AG
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

with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;

with Mutools.Files;

with Mulog;

with Bfd.Sections;

package body Bin_Split.Files
is

   --------------------------------------------------------------------------

   procedure Write_Section
     (Info             : Section_Info;
      Output_File_Name : String;
      Descriptor       : Bfd.Files.File_Type)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Out_File : Ada.Streams.Stream_IO.File_Type;
   begin
      Mutools.Files.Open
        (Filename => Output_File_Name,
         File     => Out_File);

      if Info.Write_To_File then
         declare
            Sec : constant Bfd.Sections.Section
              := Bfd.Sections.Find_Section
                (File => Descriptor,
                 Name => Ada.Strings.Unbounded.To_String (Info.Name));
            Buf : Ada.Streams.Stream_Element_Array
              (1 .. Ada.Streams.Stream_Element_Offset (Sec.Size));
            Last : Ada.Streams.Stream_Element_Offset;
         begin
            Bfd.Sections.Get_Section_Contents
              (File => Descriptor,
               S    => Sec,
               Item => Buf,
               Last => Last);

            Mulog.Log (Level => Mulog.Debug,
                       Msg   => "Writing section '"
                         & Ada.Strings.Unbounded.To_String (Info.Name)
                         & "' to file '" & Output_File_Name & "'.");

            Ada.Streams.Stream_IO.Write (File => Out_File,
                                         Item => Buf);
         end;
      end if;

      Ada.Streams.Stream_IO.Close (Out_File);

   exception
      when others =>
         Ada.Streams.Stream_IO.Close (Out_File);
         raise;
   end Write_Section;

end Bin_Split.Files;
