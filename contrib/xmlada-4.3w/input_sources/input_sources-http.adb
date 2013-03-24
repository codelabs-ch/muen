------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Unicode.CES;        use Unicode.CES;
with Unicode.CES.Utf32;  use Unicode.CES.Utf32;
with Unicode.CES.Utf16;  use Unicode.CES.Utf16;
with Unicode.CES.Utf8;   use Unicode.CES.Utf8;

with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Regpat;             use GNAT.Regpat;
with GNAT.Sockets;            use GNAT.Sockets;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Streams; use Ada.Streams;

package body Input_Sources.Http is

   Debug : constant Boolean := False;

   ----------
   -- Open --
   ----------

   procedure Open
     (Hostname : String;
      Port     : Positive := 80;
      Filename : String;
      Input    : out Http_Input)
   is
      Length  : Natural;
      BOM     : Bom_Type;
      Socket  : Socket_Type;
      Addr    : Sock_Addr_Type;
      Channel : Stream_Access;
      Image_Port : constant String := Positive'Image (Port);

      HTTP_Token_OK        : constant String := "HTTP/1\.\d \d\d\d (OK|FOUND)";
      Content_Length_Token : constant String := "CONTENT-LENGTH: ";
      --  These must be upper-cased.

      Buffer  : Stream_Element_Array (1 .. 2048);
      Buffer_Last : Stream_Element_Count := 0;
      Index   : Stream_Element_Count := Buffer'First;

      function Parse_Header return Natural;
      --  Parse the headers of the http message, and return the length of the
      --  message.

      procedure Update_Buffer;
      --  Read the next stream of bytes from the socket

      function Get_Char return Character;
      --  Return the next character from the buffer

      procedure Send (Str : String);
      --  Send a request to the server

      --------------
      -- Get_Char --
      --------------

      function Get_Char return Character is
      begin
         if Index >= Buffer_Last then
            Update_Buffer;
         end if;

         if Index >= Buffer_Last then
            return ASCII.NUL;
         else
            Index := Index + 1;
            return Character'Val (Buffer (Index - 1));
         end if;
      end Get_Char;

      -------------------
      -- Update_Buffer --
      -------------------

      procedure Update_Buffer is
      begin
         GNAT.Sockets.Receive_Socket (Socket, Buffer, Buffer_Last);
         if Debug then
            Put ("< ");
            for B in Buffer'First .. Buffer_Last loop
               Put (Character'Val (Buffer (B)));
            end loop;
            New_Line;
         end if;
         Index := Buffer'First;
      end Update_Buffer;

      ------------------
      -- Parse_Header --
      ------------------

      function Parse_Header return Natural is
         Line        : String (1 .. 2048);
         Line_Index  : Natural;
         Length : Natural := 0;
         C      : Character;
         Ok : Boolean := False;
         Token : constant Pattern_Matcher := Compile (HTTP_Token_OK);

      begin
         loop
            Line_Index := Line'First;
            loop
               C := Get_Char;
               exit when C = ASCII.LF
                 or else C = ASCII.NUL;

               Line (Line_Index) := To_Upper (C);
               Line_Index := Line_Index + 1;
               exit when Line_Index > Line'Last;
            end loop;

            if Line_Index > Line'First
              and then Line (Line_Index - 1) = ASCII.CR
            then
               Line_Index := Line_Index - 1;
            end if;

            exit when Line_Index = Line'First;

            if Match (Token, Line (Line'First .. Line_Index - 1)) then
               Ok := True;

            elsif Line_Index > Content_Length_Token'Length
              and then Line (1 .. Content_Length_Token'Length) =
                Content_Length_Token
            then
               begin
                  Length := Natural'Value
                    (Line (Content_Length_Token'Length + 1 .. Line_Index - 1));
               exception
                  when others =>
                     Length := 0;
               end;
            end if;
         end loop;

         if Ok then
            return Length;
         else
            return 0;
         end if;
      end Parse_Header;

      ----------
      -- Send --
      ----------

      procedure Send (Str : String) is
      begin
         if Debug then
            Put_Line ("> " & Str);
         end if;
         String'Write (Channel, Str);
      end Send;

   begin
      if Debug then
         Put_Line ("Hostname: " & Hostname);
         Put_Line ("Port:    " & Integer'Image (Port));
         Put_Line ("File:     " & Filename);
      end if;

      Addr := (GNAT.Sockets.Family_Inet,
               Addresses (Get_Host_By_Name (Hostname), 1),
               Port_Type (Port));

      Create_Socket (Socket);
      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
      Set_Socket_Option (Socket, Option => (Receive_Buffer, 3000));
      Connect_Socket (Socket, Addr);

      Channel := Stream (Socket);

      Send ("GET http://"
            & Hostname & ":"
            & Image_Port (Image_Port'First + 1 .. Image_Port'Last)
            & "/" & Filename & " HTTP/1.0" & ASCII.LF);
      Send ("" & ASCII.LF);

      Length := Parse_Header;
      if Length = 0 then
         Put_Line ("Nothing to read");
         raise Http_Error;
      end if;

      Input.Buffer := new String (1 .. Length - 1);

      Input.Index := 1;
      for A in 1 .. Length - 1 loop
         Input.Buffer (A) := Get_Char;
      end loop;

      Read_Bom (Input.Buffer.all, Input.Prolog_Size, BOM);
      case BOM is
         when Utf32_LE =>
            Set_Encoding (Input, Utf32_LE_Encoding);
         when Utf32_BE =>
            Set_Encoding (Input, Utf32_BE_Encoding);
         when Utf16_LE =>
            Set_Encoding (Input, Utf16_LE_Encoding);
         when Utf16_BE =>
            Set_Encoding (Input, Utf16_BE_Encoding);
         when others =>
            Set_Encoding (Input, Utf8_Encoding);
      end case;

      Input.Index := Input.Buffer'First + Input.Prolog_Size;

      Close_Socket (Socket);
   end Open;

   ----------
   -- Open --
   ----------

   procedure Open (URL : String; Input : out Http_Input) is
      Host_Start, Host_End : Natural;
      Port : Integer := 80;
      File_Start : Integer;
   begin
      if URL'Length > 6
        and then URL (URL'First .. URL'First + 6) = "http://"
      then
         Host_Start := URL'First + 7;
         Host_End := Host_Start;

         while Host_End <= URL'Last
           and then URL (Host_End) /= '/'
           and then URL (Host_End) /= ':'
         loop
            Host_End := Host_End + 1;
         end loop;

         if Host_End > URL'Last then
            --  Invalid URL
            Put_Line ("Invalid URL");
            raise Http_Error;
         end if;

         if URL (Host_End) = ':' then
            File_Start := Host_End + 1;
            while File_Start <= URL'Last
              and then URL (File_Start) /= '/'
            loop
               File_Start := File_Start + 1;
            end loop;

            if File_Start > URL'Last then
               --  Invalid URL
               Put_Line ("Invalid URL");
               raise Http_Error;
            end if;

            begin
               Port := Integer'Value (URL (Host_End + 1 .. File_Start - 1));
            exception
               when others =>
                  Port := 80;
            end;

            File_Start := File_Start + 1;
         else
            File_Start := Host_End + 1;
         end if;

         Open (Hostname => URL (Host_Start .. Host_End - 1),
               Port     => Port,
               Filename => URL (File_Start .. URL'Last),
               Input    => Input);

      else
         --  Invalid URL
         Put_Line ("Invalid URL");
         raise Http_Error;
      end if;
   end Open;

   -----------
   -- Close --
   -----------

   procedure Close (Input : in out Http_Input) is
   begin
      Input_Sources.Close (Input_Source (Input));
      Free (Input.Buffer);
      Input.Index := Natural'Last;
   end Close;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (From : in out Http_Input;
      C    : out Unicode.Unicode_Char) is
   begin
      From.Es.Read (From.Buffer.all, From.Index, C);
      C := From.Cs.To_Unicode (C);

   exception
      --  The whole page has been fully loaded in the Open step.
      --  Hence if the buffer ends with an Incomplete_Encoding, this
      --  is a fatale error.
      when Incomplete_Encoding =>
         raise Invalid_Encoding;
   end Next_Char;

   ---------
   -- Eof --
   ---------

   function Eof (From : Http_Input) return Boolean is
   begin
      return From.Buffer = null
        or else From.Index > From.Buffer'Length;
   end Eof;

begin
   pragma Warnings (Off);
   GNAT.Sockets.Initialize;
   pragma Warnings (On);
end Input_Sources.Http;
