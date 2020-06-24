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
     (Info             :     Section_Info;
      Output_File_Name :     String;
      Descriptor       :     Bfd.Files.File_Type;
      Hash             : out GNAT.SHA256.Message_Digest)
   is
      use type Ada.Streams.Stream_Element_Offset;

      Sec : constant Bfd.Sections.Section
          := Bfd.Sections.Find_Section
            (File => Descriptor,
             Name => Ada.Strings.Unbounded.To_String (Info.Name));

      Buf_Size  : constant Ada.Streams.Stream_Element_Offset := 4096;
      Size      : constant Ada.Streams.Stream_Element_Offset
        := Ada.Streams.Stream_Element_Offset (Sec.Size);
      Rem_Bytes : constant Ada.Streams.Stream_Element_Offset
        := Size mod Buf_Size;

      Out_File  : Ada.Streams.Stream_IO.File_Type;
      Pos, Last : Ada.Streams.Stream_Element_Offset;
      Buf       : Ada.Streams.Stream_Element_Array (0 .. Buf_Size - 1);
      Hash_Ctx  : GNAT.SHA256.Context := GNAT.SHA256.Initial_Context;
   begin
      Hash := (others => ASCII.NUL);
      Last := 1;
      Pos  := 0;
      Mulog.Log (Level => Mulog.Debug,
                 Msg   => "Writing section '"
                 & Ada.Strings.Unbounded.To_String (Info.Name)
                 & "' to file '" & Output_File_Name);

      Mutools.Files.Open
        (Filename => Output_File_Name,
         File     => Out_File);

      loop
         Bfd.Sections.Get_Section_Contents
           (File => Descriptor,
            S    => Sec,
            Pos  => Pos,
            Item => Buf (Buf'First ..
              (if Size - Pos < Buf_Size then Rem_Bytes - 1 else Buf'Last)),
            Last => Last);

         Ada.Streams.Stream_IO.Write
           (File => Out_File,
            Item => Buf (Buf'First .. Ada.Streams.Stream_Element_Offset'Max
              (Last, Rem_Bytes - 1)));
         GNAT.SHA256.Update
           (C     => Hash_Ctx,
            Input => Buf (Buf'First .. Ada.Streams.Stream_Element_Offset'Max
              (Last, Rem_Bytes - 1)));

         Pos := Pos + Last + 1;
         exit when Pos >= Size;
      end loop;

      Ada.Streams.Stream_IO.Close (Out_File);

      --  Update hash to include zero-padding up to 4 KB since the hash is over
      --  the entire memory region and not just the file content.

      Buf (Ada.Streams.Stream_Element_Offset'Max (Last + 1,
           Rem_Bytes) .. Buf'Last) := (others => 0);
      GNAT.SHA256.Update
        (C     => Hash_Ctx,
         Input => Buf (Ada.Streams.Stream_Element_Offset'Max (Last + 1,
           Rem_Bytes) .. Buf'Last));
      Hash := GNAT.SHA256.Digest (C => Hash_Ctx);

   exception
      when others =>
         Ada.Streams.Stream_IO.Close (Out_File);
         raise;
   end Write_Section;

end Bin_Split.Files;
