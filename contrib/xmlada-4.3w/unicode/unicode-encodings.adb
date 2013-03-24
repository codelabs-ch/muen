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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Unicode.CES;               use Unicode.CES;
with Unicode.CES.Basic_8bit;    use Unicode.CES.Basic_8bit;
with Unicode.CES.Utf8;          use Unicode.CES.Utf8;
with Unicode.CES.Utf16;         use Unicode.CES.Utf16;
with Unicode.CES.Utf32;         use Unicode.CES.Utf32;
with Unicode.CCS;               use Unicode.CCS;
with Unicode.CCS.Iso_8859_1;    use Unicode.CCS.Iso_8859_1;
with Unicode.CCS.Iso_8859_2;    use Unicode.CCS.Iso_8859_2;
with Unicode.CCS.Iso_8859_3;    use Unicode.CCS.Iso_8859_3;
with Unicode.CCS.Iso_8859_4;    use Unicode.CCS.Iso_8859_4;
with Unicode.CCS.Iso_8859_15;   use Unicode.CCS.Iso_8859_15;
with Unicode.CCS.Windows_1251;  use Unicode.CCS.Windows_1251;
with Unicode.CCS.Windows_1252;  use Unicode.CCS.Windows_1252;

package body Unicode.Encodings is

   Cst_Utf16    : aliased constant String := "utf-16";
   Cst_Utf16_BE : aliased constant String := "utf-16be";
   Cst_Utf16_LE : aliased constant String := "utf-16le";
   Cst_Utf8     : aliased constant String := "utf-8";
   Cst_Utf32    : aliased constant String := "utf-32";
   Cst_Utf32_BE : aliased constant String := "utf-32be";
   Cst_Utf32_LE : aliased constant String := "utf-32le";

   -----------------
   -- Get_By_Name --
   -----------------

   function Get_By_Name (Name : String) return Unicode_Encoding is
      N : constant String := To_Lower (Name);
   begin
      if N = Cst_Utf16 or else N = Cst_Utf16_LE then
         return (Name            => Cst_Utf16'Access,
                 Character_Set   => Unicode_Character_Set,
                 Encoding_Scheme => Utf16_LE_Encoding);
      elsif N = Cst_Utf16_BE then
         return (Name            => Cst_Utf16_BE'Access,
                 Character_Set   => Unicode_Character_Set,
                 Encoding_Scheme => Utf16_BE_Encoding);
      elsif N = Cst_Utf32 or else N = Cst_Utf32_LE then
         return (Name            => Cst_Utf32'Access,
                 Character_Set   => Unicode_Character_Set,
                 Encoding_Scheme => Utf32_LE_Encoding);
      elsif N = Cst_Utf32_BE then
         return (Name            => Cst_Utf32_BE'Access,
                 Character_Set   => Unicode_Character_Set,
                 Encoding_Scheme => Utf32_BE_Encoding);
      elsif N = Cst_Utf8 or else N = "utf8" then
         return (Name            => Cst_Utf8'Access,
                 Character_Set   => Unicode_Character_Set,
                 Encoding_Scheme => Utf8_Encoding);
      elsif N = To_Lower (Unicode.CCS.Iso_8859_1.Name1)
        or else N = "ascii"
      then
         return (Name            => Unicode.CCS.Iso_8859_1.Name1'Access,
                 Character_Set   => Iso_8859_1_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Iso_8859_1.Name2) then
         return (Name            => Unicode.CCS.Iso_8859_1.Name2'Access,
                 Character_Set   => Iso_8859_1_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Iso_8859_2.Name1) then
         return (Name            => Unicode.CCS.Iso_8859_2.Name1'Access,
                 Character_Set   => Iso_8859_2_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Iso_8859_2.Name2) then
         return (Name            => Unicode.CCS.Iso_8859_2.Name2'Access,
                 Character_Set   => Iso_8859_2_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Iso_8859_3.Name1) then
         return (Name            => Unicode.CCS.Iso_8859_3.Name1'Access,
                 Character_Set   => Iso_8859_3_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Iso_8859_4.Name1) then
         return (Name            => Unicode.CCS.Iso_8859_4.Name1'Access,
                 Character_Set   => Iso_8859_4_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Iso_8859_15.Name1) then
         return (Name            => Unicode.CCS.Iso_8859_15.Name1'Access,
                 Character_Set   => Iso_8859_15_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Windows_1251.Name1) then
         return (Name            => Unicode.CCS.Windows_1251.Name1'Access,
                 Character_Set   => Windows_1251_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      elsif N = To_Lower (Unicode.CCS.Windows_1252.Name1) then
         return (Name            => Unicode.CCS.Windows_1252.Name1'Access,
                 Character_Set   => Windows_1252_Character_Set,
                 Encoding_Scheme => Basic_8bit_Encoding);
      else
         Raise_Exception
           (Invalid_Encoding'Identity,  "Invalid encoding: " & Name);
      end if;
   end Get_By_Name;

   -------------
   -- Convert --
   -------------

   function Convert
     (Str  : Byte_Sequence;
      From : Unicode_Encoding := Get_By_Name ("iso-8859-15");
      To   : Unicode_Encoding := Get_By_Name ("utf-8"))
     return Byte_Sequence
   is
      J      : Natural := Str'First;
      C      : Unicode.Unicode_Char;
      Buffer : Byte_Sequence (1 .. 20);
      Index  : Natural;
      Result : Unbounded_String;
   begin
      if From = To then
         return Str;
      end if;

      while J <= Str'Last loop
         From.Encoding_Scheme.Read (Str, J, C);
         C := From.Character_Set.To_Unicode (C);
         C := To.Character_Set.To_CS (C);
         Index := 0;
         To.Encoding_Scheme.Encode (C, Buffer, Index);
         Append (Result, Buffer (1 .. Index));
      end loop;
      return To_String (Result);
   end Convert;

end Unicode.Encodings;
