-----------------------------------------------------------------------
--  Util-texts -- Various Text Transformation Utilities
--  Copyright (C) 2001, 2002, 2003, 2009, 2010, 2011, 2012 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
with Interfaces;
package body Util.Texts.Transforms is

   type Code is mod 2**32;

   Conversion : constant String (1 .. 16) := "0123456789ABCDEF";

   procedure Put_Dec (Into  : in out Stream;
                      Value : in Code);

   procedure To_Hex (Into  : in out Stream;
                     Value : in Code);

   procedure Put (Into  : in out Stream;
                  Value : in String) is
   begin
      for I in Value'Range loop
         Put (Into, Value (I));
      end loop;
   end Put;

   --  ------------------------------
   --  Write in the output stream the value as a \uNNNN encoding form.
   --  ------------------------------
   procedure To_Hex (Into  : in out Stream;
                     Value : in Char) is
   begin
      To_Hex (Into, Code (Char'Pos (Value)));
   end To_Hex;

   --  ------------------------------
   --  Write in the output stream the value as a \uNNNN encoding form.
   --  ------------------------------
   procedure To_Hex (Into  : in out Stream;
                     Value : in Code) is
      S          : String (1 .. 6) := (1 => '\', 2 => 'u', others => '0');
      P          : Code := Value;
      N          : Code;
      I          : Positive := S'Last;
   begin
      while P /= 0 loop
         N := P mod 16;
         P := P / 16;
         S (I) := Conversion (Positive'Val (N + 1));
         exit when I = 1;
         I := I - 1;
      end loop;
      Put (Into, S);
   end To_Hex;

   procedure Put_Dec (Into  : in out Stream;
                      Value : in Code) is
      S : String (1 .. 9) := (others => '0');
      P : Code := Value;
      N : Code;
      I : Positive := S'Last;
   begin
      while P /= 0 loop
         N := P mod 10;
         P := P / 10;
         S (I) := Conversion (Positive'Val (N + 1));
         exit when P = 0;
         I := I - 1;
      end loop;
      Put (Into, S (I .. S'Last));
   end Put_Dec;

   --  ------------------------------
   --  Capitalize the string into the result stream.
   --  ------------------------------
   procedure Capitalize (Content : in Input;
                         Into    : in out Stream) is
      Upper  : Boolean := True;
      C      : Code;
   begin
      for I in Content'Range loop
         if Upper then
            C := Char'Pos (To_Upper (Content (I)));
            Upper := False;
         else
            C := Char'Pos (To_Lower (Content (I)));
            if C = Character'Pos ('_') or C = Character'Pos ('.') or C = Character'Pos (':')
              or C = Character'Pos (';') or C = Character'Pos (',') or C = Character'Pos (' ') then
               Upper := True;
            end if;
         end if;
         Put (Into, Character'Val (C));
      end loop;
   end Capitalize;

   --  ------------------------------
   --  Capitalize the string
   --  ------------------------------
   function Capitalize (Content : Input) return Input is
      Result : Stream;
   begin
      Capitalize (Content, Result);
      return To_Input (Result);
   end Capitalize;

   --  ------------------------------
   --  Translate the input string into an upper case string in the result stream.
   --  ------------------------------
   procedure To_Upper_Case (Content : in Input;
                            Into    : in out Stream) is
      C      : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (To_Upper (Content (I)));
         Put (Into, Character'Val (C));
      end loop;
   end To_Upper_Case;

   --  ------------------------------
   --  Translate the input string into an upper case string.
   --  ------------------------------
   function To_Upper_Case (Content : Input) return Input is
      Result : Input (Content'Range);
   begin
      for I in Content'Range loop
         Result (I) := To_Upper (Content (I));
      end loop;
      return Result;
   end To_Upper_Case;

   --  ------------------------------
   --  Translate the input string into a lower case string in the result stream.
   --  ------------------------------
   procedure To_Lower_Case (Content : in Input;
                            Into    : in out Stream) is
      C      : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (To_Lower (Content (I)));
         Put (Into, Character'Val (C));
      end loop;
   end To_Lower_Case;

   --  ------------------------------
   --  Translate the input string into a lower case string in the result stream.
   --  ------------------------------
   function To_Lower_Case (Content : Input) return Input is
      Result : Input (Content'Range);
   begin
      for I in Content'Range loop
         Result (I) := To_Lower (Content (I));
      end loop;
      return Result;
   end To_Lower_Case;

   procedure Escape_Java_Script (Content : in Input;
                                 Into    : in out Stream) is
   begin
      Escape_Java (Content             => Content, Into => Into,
                   Escape_Single_Quote => True);
   end Escape_Java_Script;

   function Escape_Java_Script (Content : Input) return Input is
      Result : Stream;
   begin
      Escape_Java (Content             => Content,
                   Into                => Result,
                   Escape_Single_Quote => True);
      return To_Input (Result);
   end Escape_Java_Script;

   procedure Escape_Java (Content : in Input;
                          Into    : in out Stream) is
   begin
      Escape_Java (Content             => Content, Into => Into,
                   Escape_Single_Quote => False);
   end Escape_Java;

   function Escape_Java (Content : Input) return Input is
      Result : Stream;
   begin
      Escape_Java (Content             => Content,
                   Into                => Result,
                   Escape_Single_Quote => False);
      return To_Input (Result);
   end Escape_Java;

   procedure Escape_Java (Content             : in Input;
                          Escape_Single_Quote : in Boolean;
                          Into                : in out Stream) is
      C : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (Content (I));
         if C < 16#20# then
            if C = 16#0A# then
               Put (Into, '\');
               Put (Into, 'n');

            elsif C = 16#0D# then
               Put (Into, '\');
               Put (Into, 'r');

            elsif C = 16#08# then
               Put (Into, '\');
               Put (Into, 'b');

            elsif C = 16#09# then
               Put (Into, '\');
               Put (Into, 't');

            elsif C = 16#0C# then
               Put (Into, '\');
               Put (Into, 'f');
            else
               To_Hex (Into, C);
            end if;

         elsif C = 16#27# then
            if Escape_Single_Quote then
               Put (Into, '\');
            end if;
            Put (Into, Character'Val (C));

         elsif C = 16#22# then
            Put (Into, '\');
            Put (Into, Character'Val (C));

         elsif C = 16#5C# then
            Put (Into, '\');
            Put (Into, Character'Val (C));

         elsif C >= 16#100# then
            To_Hex (Into, C);

         else
            Put (Into, Character'Val (C));
         end if;
      end loop;
   end Escape_Java;


   function Escape_Xml (Content : Input) return Input is
      Result : Stream;
   begin
      Escape_Xml (Content => Content,
                  Into    => Result);
      return To_Input (Result);
   end Escape_Xml;

   --  Escape the content into the result stream using the XML
   --  escape rules:
   --   '<' -> '&lt;'
   --   '>' -> '&gt;'
   --   ''' -> '&apos;'
   --   '&' -> '&amp;'
   --       -> '&#nnn;' if Character'Pos >= 128
   procedure Escape_Xml (Content : in Input;
                         Into    : in out Stream) is
      C : Code;
   begin
      for I in Content'Range loop
         C := Char'Pos (Content (I));
         if C = Character'Pos ('<') then
            Put (Into, "&lt;");

         elsif C = Character'Pos ('>') then
            Put (Into, "&gt;");

         elsif C = Character'Pos ('&') then
            Put (Into, "&amp;");

         elsif C = Character'Pos (''') then
            Put (Into, "&apos;");

         elsif C > 16#7F# then
            Put (Into, '&');
            Put (Into, '#');
            Put_Dec (Into, C);
            Put (Into, ';');

         else
            Put (Into, Character'Val (C));
         end if;
      end loop;
   end Escape_Xml;

   --  ------------------------------
   --  Translate the XML entity represented by <tt>Entity</tt> into an UTF-8 sequence
   --  in the output stream.
   --  ------------------------------
   procedure Translate_Xml_Entity (Entity : in Input;
                                   Into   : in out Stream) is
   begin
      if Entity (Entity'First) = Char'Val (Character'Pos ('&'))
        and then Entity (Entity'Last) = Char'Val (Character'Pos (';')) then
         case Char'Pos (Entity (Entity'First + 1)) is
            when Character'Pos ('l') =>
               if Entity'Length = 4
                 and then Entity (Entity'First + 2) = Char'Val (Character'Pos ('t')) then
                  Put (Into, '<');
                  return;
               end if;

            when Character'Pos ('g') =>
               if Entity'Length = 4
                 and then Entity (Entity'First + 2) = Char'Val (Character'Pos ('t')) then
                  Put (Into, '>');
                  return;
               end if;

            when Character'Pos ('a') =>
               if Entity'Length = 5
                 and then Entity (Entity'First + 2) = Char'Val (Character'Pos ('m'))
                 and then Entity (Entity'First + 3) = Char'Val (Character'Pos ('p')) then
                  Put (Into, '&');
                  return;
               end if;
               if Entity'Length = 6
                 and then Entity (Entity'First + 2) = Char'Val (Character'Pos ('p'))
                 and then Entity (Entity'First + 3) = Char'Val (Character'Pos ('o'))
                 and then Entity (Entity'First + 4) = Char'Val (Character'Pos ('s')) then
                  Put (Into, ''');
                  return;
               end if;

            when Character'Pos ('q') =>
               if Entity'Length = 6
                 and then Entity (Entity'First + 2) = Char'Val (Character'Pos ('u'))
                 and then Entity (Entity'First + 3) = Char'Val (Character'Pos ('o'))
                 and then Entity (Entity'First + 4) = Char'Val (Character'Pos ('t')) then
                  Put (Into, '"');
                  return;
               end if;

            when Character'Pos ('#') =>
               declare
                  use Interfaces;

                  V : Unsigned_32 := 0;
                  C : Code;
               begin
                  if Entity (Entity'First + 2) = Char'Val (Character'Pos ('x')) then
                     for I in Entity'First + 3 .. Entity'Last - 1 loop
                        C := Char'Pos (Entity (I));
                        if C >= Character'Pos ('0') and C <= Character'Pos ('9') then
                           V := (V * 16) + Unsigned_32 (C - Character'Pos ('0'));
                        elsif C >= Character'Pos ('a') and C <= Character'Pos ('z') then
                           V := (V * 16) + 10 + Unsigned_32 (C - Character'Pos ('a'));
                        elsif C >= Character'Pos ('A') and C <= Character'Pos ('Z') then
                           V := (V * 16) + 10 + Unsigned_32 (C - Character'Pos ('A'));
                        end if;
                     end loop;
                  else
                     for I in Entity'First + 2 .. Entity'Last - 1 loop
                        C := Char'Pos (Entity (I));
                        if C >= Character'Pos ('0') and C <= Character'Pos ('9') then
                           V := (V * 10) + Unsigned_32 (C - Character'Pos ('0'));
                        end if;
                     end loop;
                  end if;
                  if V <= 16#007F# then
                     Put (Into, Character'Val (V));
                     return;

                  elsif V <= 16#07FF# then
                     Put (Into, Character'Val (16#C0# or Interfaces.Shift_Right (V, 6)));
                     Put (Into, Character'Val (16#80# or (V and 16#03F#)));
                     return;

                  elsif V <= 16#0FFFF# then
                     Put (Into, Character'Val (16#D0# or Shift_Right (V, 12)));
                     Put (Into, Character'Val (16#80# or (Shift_Right (V, 6) and 16#03F#)));
                     Put (Into, Character'Val (16#80# or (V and 16#03F#)));
                     return;

                  elsif V <= 16#10FFFF# then
                     Put (Into, Character'Val (16#E0# or Shift_Right (V, 18)));
                     Put (Into, Character'Val (16#80# or (Shift_Right (V, 12) and 16#03F#)));
                     Put (Into, Character'Val (16#80# or (Shift_Right (V, 6) and 16#03F#)));
                     Put (Into, Character'Val (16#80# or (V and 16#03F#)));
                     return;

                  end if;
               end;

            when others =>
               null;

         end case;
      end if;

      --  Invalid entity.
   end Translate_Xml_Entity;

   --  ------------------------------
   --  Unescape the XML entities from the content into the result stream.
   --  For each XML entity found, call the <tt>Translator</tt> procedure who is responsible
   --  for writing the result in the stream.  The XML entity starts with '&' and ends with ';'.
   --  The '&' and ';' are part of the entity when given to the translator.  If the trailing
   --  ';' is not part of the entity, it means the entity was truncated or the end of input
   --  stream is reached.
   --  ------------------------------
   procedure Unescape_Xml (Content    : in Input;
                           Translator : not null access
                             procedure (Entity : in Input;
                                        Into   : in out Stream) := Translate_Xml_Entity'Access;
                           Into       : in out Stream) is
      MAX_ENTITY_LENGTH : constant Positive := 30;

      Entity : Input (1 .. MAX_ENTITY_LENGTH);
      C      : Code;
      Pos    : Natural := Content'First;
      Last   : Natural;
   begin
      while Pos <= Content'Last loop
         C := Char'Pos (Content (Pos));
         Pos := Pos + 1;
         if C = Character'Pos ('&') then
            Entity (Entity'First) := Char'Val (C);
            Last := Entity'First;
            while Pos <= Content'Last loop
               C := Char'Pos (Content (Pos));
               Pos := Pos + 1;
               if Last < Entity'Last then
                  Last := Last + 1;
                  Entity (Last) := Char'Val (C);
               end if;
               exit when C = Character'Pos (';');
            end loop;
            Translator (Entity (Entity'First .. Last), Into);
         else
            Put (Into, Character'Val (C));
         end if;
      end loop;
   end Unescape_Xml;

end Util.Texts.Transforms;
