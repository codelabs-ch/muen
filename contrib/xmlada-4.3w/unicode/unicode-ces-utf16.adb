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

with Unicode.CES.Utf32;  use Unicode.CES.Utf32;
with Unicode.CCS;        use Unicode.CCS;

package body Unicode.CES.Utf16 is

   ------------
   -- Encode --
   ------------

   procedure Encode
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural)
   is
      C, D : Unicode_Char;
   begin
      if Char < 16#10000# then
         C := Char and 16#00FF#;
         D := (Char and 16#FF00#) / (2 ** 8);
         Output (Index + 1) := Character'Val (C);
         Output (Index + 2) := Character'Val (D);
         Index := Index + 2;

      else
         C := 16#D800#
           + ((Char - 16#10000#) and 2#11111111110000000000#) / (2 ** 10);
         D := 16#DC00#
           + ((Char - 16#10000#) and 2#1111111111#);
         Output (Index + 1) := Character'Val (C and 16#00FF#);
         Output (Index + 2) := Character'Val ((C and 16#FF00#) / (2 ** 8));
         Output (Index + 3) := Character'Val (D and 16#00FF#);
         Output (Index + 4) := Character'Val ((D and 16#FF00#) / (2 ** 8));
         Index := Index + 4;
      end if;
   end Encode;

   ---------------
   -- Encode_BE --
   ---------------

   procedure Encode_BE
     (Char   : Unicode_Char;
      Output : in out Byte_Sequence;
      Index  : in out Natural)
   is
      C, D : Unicode_Char;
   begin
      if Char < 16#10000# then
         C := Char and 16#00FF#;
         D := (Char and 16#FF00#) / (2 ** 8);
         Output (Index + 1) := Character'Val (D);
         Output (Index + 2) := Character'Val (C);
         Index := Index + 2;

      else
         C := 16#D800#
           + ((Char - 16#10000#) and 2#11111111110000000000#) / (2 ** 10);
         D := 16#DC00#
           + ((Char - 16#10000#) and 2#1111111111#);
         Output (Index + 1) := Character'Val ((C and 16#FF00#) / (2 ** 8));
         Output (Index + 2) := Character'Val (C and 16#00FF#);
         Output (Index + 3) := Character'Val ((D and 16#FF00#) / (2 ** 8));
         Output (Index + 4) := Character'Val (D and 16#00FF#);
         Index := Index + 4;
      end if;
   end Encode_BE;

   ----------
   -- Read --
   ----------

   procedure Read
     (Str   : Utf16_LE_String;
      Index : in out Positive;
      Char  : out Unicode_Char)
   is
      C, D : Unicode_Char;
   begin
      if Index + 1 > Str'Last then
         raise Incomplete_Encoding;
      end if;

      C := Character'Pos (Str (Index + 1)) * 256 + Character'Pos (Str (Index));

      --  High surrogate value
      if C in 16#D800# .. 16#DBFF# then
         if Index + 3 > Str'Last then
            raise Incomplete_Encoding;
         end if;
         D := Character'Pos (Str (Index + 3)) * 256
           + Character'Pos (Str (Index + 2));

         --  Not a low surrogate ?
         if not (D in 16#DC00# .. 16#DFFF#) then
            raise Invalid_Encoding;
         end if;

         C := C and 2#1111111111#;
         D := D and 2#1111111111#;
         Char := C * 2#10000000000# + D + 16#10000#;
         Index := Index + 4;
      else
         Char := C;
         Index := Index + 2;
      end if;
   end Read;

   -------------
   -- Read_BE --
   -------------

   procedure Read_BE
     (Str   : Utf16_BE_String;
      Index : in out Positive;
      Char  : out Unicode_Char)
   is
      C, D : Unicode_Char;
   begin
      if Index + 1 > Str'Last then
         raise Incomplete_Encoding;
      end if;

      C := Character'Pos (Str (Index)) * 256 + Character'Pos (Str (Index + 1));

      --  High surrogate value
      if C in 16#D800# .. 16#DBFF# then
         if Index + 3 > Str'Last then
            raise Incomplete_Encoding;
         end if;
         D := Character'Pos (Str (Index + 2)) * 256
           + Character'Pos (Str (Index + 3));

         --  Not a low surrogate ?
         if not (D in 16#DC00# .. 16#DFFF#) then
            raise Invalid_Encoding;
         end if;

         C := C and 2#1111111111#;
         D := D and 2#1111111111#;
         Char := C * 2#10000000000# + D + 16#10000#;
         Index := Index + 4;
      else
         Char := C;
         Index := Index + 2;
      end if;
   end Read_BE;

   -----------
   -- Width --
   -----------

   function Width (Char : Unicode_Char) return Natural is
   begin
      if Char >= 16#10000# then
         return 4;
      else
         return 2;
      end if;
   end Width;

   ------------
   -- Length --
   ------------

   function Length (Str : Utf16_String) return Natural is
      Pos : Natural := Str'First;
      Len : Natural := 0;
      C   : Unicode_Char;
   begin
      while Pos <= Str'Last loop
         Read (Str, Pos, C);
         Len := Len + 1;
      end loop;
      return Len;
   end Length;

   ----------------
   -- From_Utf32 --
   ----------------

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Utf16_LE_String
   is
      Result : Utf16_LE_String (1 .. (Str'Length / Utf32_Char_Width) * 4);
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

   function To_Utf32
     (Str : Utf16_LE_String)
      return Unicode.CES.Utf32.Utf32_LE_String
   is
      Result : Utf32_LE_String (1 .. (Str'Length / 2) * Utf32_Char_Width);
      J : Natural := Str'First;
      R_Index : Natural := Result'First - 1;
      C : Unicode_Char;
   begin
      while J <= Str'Last loop
         Read (Str, J, C);
         Unicode.CES.Utf32.Encode (C, Result, R_Index);
      end loop;
      return Result (1 .. R_Index);
   end To_Utf32;

   -------------------
   -- To_Unicode_LE --
   -------------------
   --  ??? Note: this assumes that the original character and its
   --  conversion are encoded on the same length, which is always
   --  right so far with Unicode.

   function To_Unicode_LE
     (Str   : Utf16_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf16_LE_String
   is
      BOM    : Bom_Type;
      Offset : Natural := 0;
      O      : Byte_Order := Order;
      J      : Natural := Str'First;
      S      : Utf16_LE_String (1 .. Str'Length);
      C      : Unicode_Char;

   begin
      Read_Bom (Str, Offset, BOM);

      case BOM is
         when Utf16_LE => O := Low_Byte_First;
         when Utf16_BE => O := High_Byte_First;
         when Unknown  => null;
         when others   => raise Invalid_Encoding;
      end case;

      if O = Low_Byte_First then
         if Cs.To_Unicode = Identity'Access then
            return Str (Str'First + Offset .. Str'Last);
         else
            J := J + Offset - 1;
            while J <= Str'Last loop
               Read (Str, J, C);
               Encode (Cs.To_Unicode (C), S, J);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
      else
         J := J + Offset;
         if Cs.To_Unicode = Identity'Access then
            while J <= Str'Last loop
               S (J + 1) := Str (J);
               S (J)     := Str (J + 1);
               J := J + 2;
            end loop;
         else
            J := J - 1;
            while J <= Str'Last loop
               Read_BE (Str, J, C);
               Encode (Cs.To_Unicode (C), S, J);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
         return S (S'First + Offset .. S'Last);
      end if;
   end To_Unicode_LE;

   -----------
   -- To_CS --
   -----------

   function To_CS
     (Str   : Utf16_LE_String;
      Cs    : Unicode.CCS.Character_Set := Unicode.CCS.Unicode_Character_Set;
      Order : Byte_Order := Default_Byte_Order) return Utf16_String
   is
      pragma Warnings (Off, Order);
      Offset : constant Natural := 0;
      J      : Natural := Str'First;
      S      : Utf16_LE_String (1 .. Str'Length);
      C      : Unicode_Char;
   begin
      if Order = Low_Byte_First then
         if Cs.To_CS = Identity'Access then
            return Str (Str'First + Offset .. Str'Last);
         else
            J := J + Offset - 1;
            while J <= Str'Last loop
               Read (Str, J, C);
               Encode (Cs.To_CS (C), S, J);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
      else
         J := J + Offset;
         if Cs.To_CS = Identity'Access then
            while J <= Str'Last loop
               S (J + 1) := Str (J);
               S (J)     := Str (J + 1);
               J := J + 2;
            end loop;
         else
            J := J - 1;
            while J <= Str'Last loop
               Read (Str, J, C);
               Encode_BE (Cs.To_CS (C), S, J);
            end loop;
            return S (S'First + Offset .. S'Last);
         end if;
         return S (S'First + Offset .. S'Last);
      end if;
   end To_CS;

end Unicode.CES.Utf16;
