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

--  This package implements a basic 8bit encoding.
--  Only code points from 16#00# to 16#FF# can be encoded in such strings.
--
--  However, then can be used to read files that contain accented characters,
--  in combination with Unicode.CCS.Iso_8859_1 for instance

with Unicode.CES.Utf32;    use Unicode.CES.Utf32;
with Unicode.CCS;          use Unicode.CCS;

package body Unicode.CES.Basic_8bit is

   function Convert
     (Str     : Basic_8bit_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Basic_8bit_String;
   --  Internal conversion function

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural) is
   begin
      if Char > 16#FF# then
         raise Invalid_Encoding;
      end if;
      Index := Index + 1;
      Output (Index) := Character'Val (Char);
   end Encode;

   ----------
   -- Read --
   ----------

   procedure Read
     (Str   : Basic_8bit_String;
      Index : in out Positive;
      Char  : out Unicode_Char) is
   begin
      Char := Character'Pos (Str (Index));
      Index := Index + 1;
   end Read;

   -----------
   -- Width --
   -----------

   function Width (Char : Unicode_Char) return Natural is
      pragma Warnings (Off, Char);
   begin
      return 1;
   end Width;

   ------------
   -- Length --
   ------------

   function Length (Str : Basic_8bit_String) return Natural is
   begin
      return Str'Length;
   end Length;

   ----------------
   -- From_Utf32 --
   ----------------

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Basic_8bit_String
   is
      Result : Basic_8bit_String (1 .. Str'Length / Utf32_Char_Width);
      R_Index : Natural := Result'First - 1;
      C : Unicode_Char;
      J : Positive := Str'First;
   begin
      while J <= Str'Last loop
         Unicode.CES.Utf32.Read (Str, J, Char => C);
         Encode (C, Result, R_Index);
      end loop;
      return Result;
   end From_Utf32;

   --------------
   -- To_Utf32 --
   --------------

   function To_Utf32
     (Str : Basic_8bit_String)
      return Unicode.CES.Utf32.Utf32_LE_String
   is
      Result : Utf32_LE_String (1 .. Str'Length * Utf32_Char_Width);
      R_Index : Natural := Result'First - 1;
      J : Positive := Str'First;
      C : Unicode_Char;
   begin
      while J <= Str'Last loop
         Read (Str, J, C);
         Unicode.CES.Utf32.Encode (C, Result, R_Index);
      end loop;
      return Result;
   end To_Utf32;

   -------------
   -- Convert --
   -------------

   function Convert
     (Str     : Basic_8bit_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Basic_8bit_String
   is
      pragma Warnings (Off, Order);
      S : String (Str'Range);
      C : Unicode_Char;
      J : Positive := Str'First;
      J_Out : Natural := S'First - 1;
   begin
      if Convert = Identity'Access then
         return Str;
      else
         while J <= Str'Last loop
            Read (Str, J, C);
            Encode (Convert (C), S, J_Out);
         end loop;
         return S;
      end if;
   end Convert;

   -------------------
   -- To_Unicode_LE --
   -------------------

   function To_Unicode_LE
     (Str   : Basic_8bit_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Basic_8bit_String is
   begin
      return Convert (Str, Cs.To_Unicode, Order);
   end To_Unicode_LE;

   -----------
   -- To_CS --
   -----------

   function To_CS
     (Str   : Basic_8bit_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Basic_8bit_String is
   begin
      return Convert (Str, Cs.To_CS, Order);
   end To_CS;

end Unicode.CES.Basic_8bit;
