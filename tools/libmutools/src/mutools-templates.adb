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

with Ada.Exceptions;

with GNAT.Regpat;

with Mutools.Files;

package body Mutools.Templates
is

   package SIO renames Ada.Streams.Stream_IO;

   -------------------------------------------------------------------------

   procedure Close (Template : in out Stream_Template_Type)
   is
   begin
      SIO.Close (Template.Fd);
   end Close;

   -------------------------------------------------------------------------

   function Create (Content : String) return Template_Type
   is
   begin
      return T : Template_Type do
         T.Data := To_Unbounded_String (Content);
      end return;
   end Create;

   -------------------------------------------------------------------------

   function Create
     (Content  : String;
      Filename : String)
      return Stream_Template_Type
   is
   begin
      return S : Stream_Template_Type do
         Tokenize (Data => Content,
                   List => S.Tokens);
         S.Pos := S.Tokens.First;
         Files.Open (Filename => Filename,
                     File     => S.Fd,
                     Writable => True);
      end return;
   end Create;

   -------------------------------------------------------------------------

   procedure Replace
     (Template : in out Template_Type;
      Pattern  :        String;
      Content  :        String)
   is
      First : Boolean := True;
      Idx   : Natural := 1;
   begin
      loop
         Idx := Index (Source  => Template.Data,
                       From    => Idx,
                       Pattern => Pattern);
         if First and then Idx = 0 then
            raise Pattern_Not_Found with "Pattern '" & Pattern
              & "' does not exist";
         end if;

         exit when Idx = 0;

         First := False;
         Replace_Slice (Source => Template.Data,
                        Low    => Idx,
                        High   => Idx + Pattern'Length - 1,
                        By     => Content);
      end loop;
   end Replace;

   -------------------------------------------------------------------------

   procedure Stream (Template : in out Stream_Template_Type)
   is
      use type USLP.Cursor;
   begin
      if Template.Pos /= USLP.No_Element then
         Write
           (Template => Template,
            Item     => To_String (USLP.Element (Position => Template.Pos)));
         Template.Pos := USLP.Next (Position => Template.Pos);
      else
         raise IO_Error with "Unable to stream: no content left";
      end if;
   end Stream;

   -------------------------------------------------------------------------

   procedure Tokenize
     (Data :     String;
      List : out USLP.List)
   is
      use type GNAT.Regpat.Match_Location;

      Re  : constant GNAT.Regpat.Pattern_Matcher
        := GNAT.Regpat.Compile ("(__[a-z0-9_]+__)");
      M   : GNAT.Regpat.Match_Array (0 .. 1);
      Idx : Positive := Data'First;
   begin
      List := USLP.Empty_List;

      GNAT.Regpat.Match
        (Self    => Re,
         Data    => Data,
         Matches => M);

      while M (0) /= GNAT.Regpat.No_Match loop
         List.Append (New_Item => To_Unbounded_String
           (Data (Idx .. M (1).First - 1)));
         Idx := M (1).Last + 1;

         GNAT.Regpat.Match
           (Self       => Re,
            Data       => Data,
            Data_First => Idx,
            Matches    => M);
      end loop;

      if Idx < Data'Length then
         List.Append (New_Item => To_Unbounded_String
           (Data (Idx .. Data'Last)));
      end if;
   end Tokenize;

   -------------------------------------------------------------------------

   procedure Write
     (Template : Template_Type;
      Filename : String)
   is
      Output_File   : SIO.File_Type;
      Output_Stream : SIO.Stream_Access;
   begin
      Files.Open (Filename => Filename,
                  File     => Output_File,
                  Writable => True);

      Output_Stream := SIO.Stream (File => Output_File);
      String'Write (Output_Stream,
                    To_String (Template.Data));
      SIO.Close (File => Output_File);

   exception
      when E : others =>
         if SIO.Is_Open (File => Output_File) then
            SIO.Close (File => Output_File);
         end if;
         raise IO_Error with "Unable to write template to '"
           & Filename & "' - " & Ada.Exceptions.Exception_Message (X => E);
   end Write;

   -------------------------------------------------------------------------

   procedure Write
     (Template : in out Stream_Template_Type;
      Item     :        String)
   is
   begin
      String'Write (SIO.Stream (Template.Fd), Item);
   end Write;

end Mutools.Templates;
