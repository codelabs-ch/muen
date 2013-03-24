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

with Unicode.CES.Utf32; use Unicode.CES.Utf32;
with Unicode.CCS;       use Unicode.CCS;

package body Unicode.CES.Utf8 is

   function Internal_Convert
     (Str     : Utf8_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Utf8_String;
   --  Internal function used to convert character sets

   -----------
   -- Width --
   -----------

   function Width (Char : Unicode_Char) return Natural is
   begin
      if Char < 16#80# then
         return 1;
      elsif Char < 16#800# then
         return 2;
      elsif Char < 16#10000# then
         return 3;
      elsif Char < 16#200000# then
         return 4;
      elsif Char < 16#4000000# then
         return 5;
      else
         return 6;
      end if;
   end Width;

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural)
   is
      Len  : Natural;
      Mask : Unicode_Char;
      Val  : Unicode_Char := Char;
      First, Last : Natural;
   begin
      if Char < 16#80# then
         Len := 1;
         Mask := 16#0#;
      elsif Char < 16#800# then
         Len := 2;
         Mask := 16#C0#;
      elsif Char < 16#10000# then
         Len := 3;
         Mask := 16#E0#;
      elsif Char < 16#200000# then
         Len := 4;
         Mask := 16#F0#;
      elsif Char < 16#4000000# then
         Len := 5;
         Mask := 16#F8#;
      else
         Len := 6;
         Mask := 16#FC#;
      end if;

      First := Index + 2;
      Last  := Index + Len;
      for J in reverse First .. Last loop
         Output (J) := Character'Val ((Val and 16#3f#) or 16#80#);
         Val := Val / (2 ** 6);
      end loop;
      Output (Index + 1) := Character'Val (Val or Mask);
      Index := Last;
   end Encode;

   ----------
   -- Read --
   ----------

   procedure Read
     (Str   : Utf8_String;
      Index : in out Positive;
      Char  : out Unicode_Char)
   is
      Len  : Natural;
      Val  : Unicode_Char;
      C    : Unicode_Char := Character'Pos (Str (Index));
   begin

      --  Compute the length of the encoding given what was in the first byte
      if C < 128 then
         Len := Index;
         Val := C and 16#7f#;
      elsif (C and 16#E0#) = 16#C0# then
         Len := Index + 1;
         Val := C and 16#1f#;
      elsif (C and 16#F0#) = 16#E0# then
         Len := Index + 2;
         Val := C and 16#0f#;
      elsif (C and 16#F8#) = 16#F0# then
         Len := Index + 3;
         Val := C and 16#07#;
      elsif (C and 16#FC#) = 16#F8# then
         Len := Index + 4;
         Val := C and 16#03#;
      elsif (C and 16#FE#) = 16#FC# then
         Len := Index + 5;
         Val := C and 16#01#;
      else
         raise Invalid_Encoding;
      end if;

      for Count in Index + 1 .. Natural'Min (Len, Str'Last) loop
         C := Character'Pos (Str (Count));
         if (C and 16#C0#) /= 16#80# then
            raise Invalid_Encoding;
         end if;
         Val := (Val * (2 ** 6)) or (C and 16#3f#);
      end loop;

      if Str'Last < Len then
         raise Incomplete_Encoding;
      end if;

      Index := Len + 1;
      Char  := Val;
   end Read;

   ------------
   -- Length --
   ------------

   function Length (Str : Utf8_String) return Natural is
      Pos : Natural := Str'First;
      Length : Natural := 0;
      C : Unicode_Char;
   begin
      while Pos <= Str'Last loop
         Read (Str, Pos, C);
         Length := Length + 1;
      end loop;
      return Length;
   end Length;

   ----------------
   -- From_Utf32 --
   ----------------

   function From_Utf32 (Str : Unicode.CES.Utf32.Utf32_String)
      return Utf8_String
   is
      --  Allocate worst case
      Result : Utf8_String (1 .. (Str'Length / Utf32_Char_Width) * 6);
      J : Positive := Str'First;
      R_Index : Natural := Result'First - 1;
      C : Unicode_Char;
   begin
      while J <= Str'Last loop
         Unicode.CES.Utf32.Read (Str, J, C);
         Encode (C, Result, R_Index);
      end loop;

      return Result (1 .. R_Index);
   end From_Utf32;

   --------------
   -- To_Utf32 --
   --------------

   function To_Utf32 (Str : Utf8_String)
      return Unicode.CES.Utf32.Utf32_LE_String
   is
      --  Allocate worst case
      Result : Utf32_LE_String (1 .. Str'Length * Utf32_Char_Width);
      J : Positive := Str'First;
      R_Index : Natural := Result'First - 1;
      C : Unicode_Char;
   begin
      while J <= Str'Last loop
         Read (Str, J, C);
         Unicode.CES.Utf32.Encode (C, Result, R_Index);
      end loop;
      return Result (1 .. R_Index);
   end To_Utf32;

   ----------------------
   -- Internal_Convert --
   ----------------------

   function Internal_Convert
     (Str     : Utf8_String;
      Convert : Unicode.CCS.Conversion_Function := Identity'Access;
      Order   : Byte_Order := Default_Byte_Order) return Utf8_String
   is
      pragma Warnings (Off, Order);
      Offset : Natural := 0;
      BOM : Bom_Type;
      C : Unicode_Char;
      J : Natural;
   begin
      Read_Bom (Str, Offset, BOM);

      case BOM is
         when Utf8_All | Unknown => null;
         when others             => raise Invalid_Encoding;
      end case;

      if Convert = Identity'Access then
         return Str (Str'First + Offset .. Str'Last);
      else
         declare
            --  Allocate worst case for the string
            Result : Utf8_String (1 .. Str'Length * 6);
            R_Index : Natural := Result'First - 1;
         begin
            J := Str'First + Offset;
            while J <= Str'Last loop
               Read (Str, J, C);
               Encode (Convert (C), Result, R_Index);
            end loop;
            return Result (1 .. R_Index);
         end;
      end if;
   end Internal_Convert;

   -------------------
   -- To_Unicode_LE --
   -------------------

   function To_Unicode_LE
     (Str   : Utf8_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf8_String is
   begin
      return Internal_Convert (Str, Cs.To_Unicode, Order);
   end To_Unicode_LE;

   -----------
   -- To_CS --
   -----------

   function To_CS
     (Str   : Utf8_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf8_String is
   begin
      return Internal_Convert (Str, Cs.To_CS, Order);
   end To_CS;
end Unicode.CES.Utf8;
