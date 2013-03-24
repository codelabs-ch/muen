------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with Unicode;                use Unicode;
with Unicode.CES;            use Unicode.CES;
with Unicode.CES.Utf32;      use Unicode.CES.Utf32;
with Unicode.CES.Utf16;      use Unicode.CES.Utf16;
with Unicode.CES.Utf8;       use Unicode.CES.Utf8;
with Unicode.CES.Basic_8bit; use Unicode.CES.Basic_8bit;
with Unicode.CCS;            use Unicode.CCS;
with Unicode.Encodings;      use Unicode.Encodings;
with Unicode.CCS.Iso_8859_2; use Unicode.CCS.Iso_8859_2;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

procedure Test_Unicode is

   Test_Num : Natural := 1;

   Had_Error : Boolean := False;

   procedure Assert (S1, S2 : String; Msg : String);
   function Image (Str : String) return String;
   function Encode
     (C : Unicode_Char; Scheme : Encoding_Scheme) return Byte_Sequence;

   -----------
   -- Image --
   -----------

   function Image (Str : String) return String is
      S : String (1 .. 6 * Str'Length);
      K : Natural := S'First;
   begin
      for J in Str'Range loop
         declare
            Img : constant String := Natural'Image (Character'Pos (Str (J)))
              & " ";
         begin
            S (K .. K + Img'Length - 1) := Img;
            K := K + Img'Length;
         end;
      end loop;
      return S (1 .. K - 1);
   end Image;

   ------------
   -- Assert --
   ------------

   procedure Assert (S1, S2 : String; Msg : String) is
   begin
      Test_Num := Test_Num + 1;
      if S1 /= S2 then
         Put_Line ("### (" & Natural'Image (Test_Num) & ") " & Msg);
         Put_Line ("  --" & S1 & "--");
         Put_Line ("  --" & S2 & "--");
         Had_Error := True;
      end if;
   end Assert;

   ------------
   -- Encode --
   ------------

   function Encode
     (C : Unicode_Char; Scheme : Encoding_Scheme) return Byte_Sequence
   is
      Buffer : Byte_Sequence (1 .. 20);
      Index  : Natural := Buffer'First - 1;
   begin
      Scheme.Encode (C, Buffer, Index);
      return Buffer (Buffer'First .. Index);
   end Encode;

   Utf16_S : constant Utf16_String := Encode (16#004D#, Utf16_LE_Encoding)
     & Encode (16#0061#, Utf16_LE_Encoding)
     & Encode (16#D800#, Utf16_LE_Encoding)
     & Encode (16#DC00#, Utf16_LE_Encoding);
   Utf16_LE_BOM : constant String :=
     Character'Val (16#FF#) & Character'Val (16#FE#);
   Utf16_BE_BOM : constant String :=
     Character'Val (16#FE#) & Character'Val (16#FF#);

begin
   --  Simple Utf32 encoding of characters
   Assert (Image (Encode (16#004D#, Utf32_LE_Encoding)), " 77  0  0  0 ",
           "Incorrect encoding for 16#004D#");
   Assert (Image (Encode (16#0061#, Utf32_LE_Encoding)),
           " 97  0  0  0 ",
           "Incorrect encoding for 16#0061#");
   Assert (Image (Encode (16#D800#, Utf32_LE_Encoding)),
           " 0  216  0  0 ",
           "Incorrect encoding for 16#D800#");

   --  Conversion from utf16 to utf32
   Assert (Image (Utf16_S), " 77  0  97  0  0  216  0  220 ",
           "Incorrect encoding of Utf16_S");
   Assert (Image (Utf16.To_Utf32 (Utf16_S)),
           " 77  0  0  0  97  0  0  0  0  0  1  0 ",
           "Incorrect conversion to utf32 for Utf16_S");
   Assert (Image (Utf16.To_Unicode_LE (Utf16_S)),
           " 77  0  97  0  0  216  0  220 ",
           "Incorrect conversion of Utf16_S to little-endian");
   Assert (Image (Utf16.To_Unicode_LE (Utf16_S)),
           " 77  0  97  0  0  216  0  220 ",
           "Incorrect conversion of Utf16_S to little-endian");
   Assert (Image (Utf16.To_Unicode_LE (Utf16_LE_BOM & Utf16_S)),
           " 77  0  97  0  0  216  0  220 ",
           "Incorrect conversion of Utf16_S when adding LE_BOM");
   Assert (Image (Utf16.To_Unicode_LE (Utf16_BE_BOM & Utf16_S)),
           " 0  77  0  97  216  0  220  0 ",
           "Incorrect conversion of Utf16_S when adding BE_BOM");

   --  Conversion from utf16 to Utf8
   declare
      Utf8_S : constant String :=
        Utf8.From_Utf32 (Utf16.To_Utf32 (Utf16_S));
   begin
      Assert (Image (Utf8_S), " 77  97  240  144  128  128 ",
              "Incorrect conversion of Utf16_S to Utf8");
      Assert (Image (Utf8.To_Unicode_LE (Utf8_S)),
              " 77  97  240  144  128  128 ",
              "Incorrect conversion of Utf16_S to Utf8, checking BOM");
      Assert (Utf16.From_Utf32 (Utf8.To_Utf32 (Utf8_S)),
              Utf16_S,
              "Incorrect conversion of Utf8_S to Utf16");
   end;

   --  Conversion from other character sets
   declare
      Latin_1_Utf16 : constant String :=
        Encode (16#E9#, Utf16_LE_Encoding)    --  e_Acute
        & Encode (Character'Pos ('t'), Utf16_LE_Encoding)
        & Encode (16#E9#, Utf16_LE_Encoding); --  e_Acute
      Latin_2_Utf16 : constant String :=
        Encode (16#F9#, Utf16_LE_Encoding)    --  u_dot
        & Encode (Character'Pos ('t'), Utf16_LE_Encoding)
        & Encode (16#E8#, Utf16_LE_Encoding); --  c carron
      Latin_1_8bit : constant String :=
        Character'Val (16#E9#)    --  e_Acute
        & 't'
        & Character'Val (16#E9#); --  e_Acute
   begin
      Assert (Image (Utf32.To_Unicode_LE (Utf16.To_Utf32 (Latin_1_Utf16))),
              " 233  0  0  0  116  0  0  0  233  0  0  0 ",
              "Incorrect conversion to utf32 for Latin1_Utf16, with "
              & "Iso_8859_1");
      Assert (Image (Utf32.To_Unicode_LE
                     (Utf16.To_Utf32 (Latin_1_Utf16),
                      Get_By_Name ("Latin1").Character_Set)),
              " 233  0  0  0  116  0  0  0  233  0  0  0 ",
              "Incorrect conversion to utf32 for Latin1_Utf16, with "
              & "Iso_8859_1");
      Assert (Image (Utf32.To_Unicode_LE (Utf16.To_Utf32 (Latin_2_Utf16))),
              " 249  0  0  0  116  0  0  0  232  0  0  0 ",
              "Incorrect conversion to utf32 for Latin2_Utf16, with "
              & "Iso_8859_2");
      Assert (Image (Utf32.To_Unicode_LE
                     (Utf16.To_Utf32 (Latin_2_Utf16),
                      Iso_8859_2_Character_Set)),
              " 111  1  0  0  116  0  0  0  13  1  0  0 ",
              "Incorrect conversion to utf32 for Latin2_Utf16, with "
              & "Iso_8859_2");
      Assert (Image (Basic_8bit.To_Unicode_LE (Latin_1_8bit)),
              " 233  116  233 ",
              "Incorrect conversion to 8bit for Latin1_8bit");
      Assert (Image (Basic_8bit.To_Unicode_LE
                     (Latin_1_8bit, Get_By_Name ("Latin1").Character_Set)),
              " 233  116  233 ",
              "Incorrect conversion to 8bit for Latin1_8bit, with "
              & "Iso_8859_1");

      --  The next two instructions do the same thing, although the first form
      --  is prefered
      Assert (Image (Basic_8bit.To_Utf32 (Basic_8bit.To_Unicode_LE
                     (Latin_1_8bit,
                      Get_By_Name ("Latin1").Character_Set))),
              " 233  0  0  0  116  0  0  0  233  0  0  0 ",
              "Incorrect conversion to 8bit for Latin1_8bit, with "
              & "Iso_8859_1");
      Assert (Image (Utf32.To_Unicode_LE
                     (Basic_8bit.To_Utf32 (Latin_1_8bit),
                      Get_By_Name ("Latin1").Character_Set)),
              " 233  0  0  0  116  0  0  0  233  0  0  0 ",
              "Incorrect conversion to 8bit for Latin1_8bit, with "
              & "Iso_8859_1");
   end;

   declare
      Asc  : constant Unicode_Encoding := Get_By_Name ("ascii");
      Utf8 : constant Unicode_Encoding := Get_By_Name ("utf8");
      Str  : constant String := "Ascii string";
   begin
      Assert (Convert (Str, From => Asc, To => Asc), Str,
              "Incorrect conversion from ascii to ascii");
      Assert (Convert (Str, From => Asc, To => Utf8), Str,
              "Incorrect conversion from ascii to utf8");
      Assert (Convert
                (Convert (Str, From => Asc, To => Utf8),
                From => Utf8, To => Asc),
              Str,
              "Incorrect conversion from ascii to utf8 and back");
   end;

   if not Had_Error then
      Put_Line ("test_unicode : SUCCESS");
   end if;

exception
   when E : others =>
      Raise_Exception
        (Exception_Identity (E),
         Exception_Message (E)
         & " while processing test" & Natural'Image (Test_Num));
end Test_Unicode;
