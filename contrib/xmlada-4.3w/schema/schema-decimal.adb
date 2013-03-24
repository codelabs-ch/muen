------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

pragma Warnings (Off, "*is an internal GNAT unit");
with System.Img_Real;           use System.Img_Real;
pragma Warnings (On, "*is an internal GNAT unit");
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Sax.Encodings;             use Sax.Encodings;
with Sax.Symbols;               use Sax.Symbols;
with Sax.Utils;                 use Sax.Utils;
with Unicode.CES;               use Unicode, Unicode.CES;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;

package body Schema.Decimal is

   type Compare_Result is (Less_Than, Equal, Greater_Than);
   function Compare (Num1, Num2 : String) return Compare_Result;
   --  Compare two numbers

   function Get_Exp (Num : String) return Long_Long_Integer;
   --  Return the exponential part of Num (ie the part after 'E'.

   procedure Get_Fore (Num : String; First, Last : out Integer);
   --  Return the position of the first and last digit in the integer part of
   --  Num

   procedure To_Next_Digit (Num : String; Pos : in out Integer);
   --  Move Pos to the next digit in Num

   procedure Internal_Value
     (Ch             : Unicode.CES.Byte_Sequence;
      Symbols        : Sax.Utils.Symbol_Table;
      Allow_Exponent : Boolean;
      Val            : out Arbitrary_Precision_Number;
      Error          : out Symbol);
   --  Internal implementation of Value

   -----------
   -- Image --
   -----------

   function Image
     (Number : Arbitrary_Precision_Number) return Unicode.CES.Byte_Sequence is
   begin
      if Number.Value /= No_Symbol then
         return Get (Number.Value).all;
      else
         return "0";
      end if;
   end Image;

   -----------
   -- Value --
   -----------

   function Value
     (Val : Sax.Symbols.Symbol) return Arbitrary_Precision_Number
   is
   begin
      return (Value => Val);
   end Value;

   -----------
   -- Value --
   -----------

   procedure Value
     (Symbols  : Sax.Utils.Symbol_Table;
      Ch       : Unicode.CES.Byte_Sequence;
      Val      : out Arbitrary_Precision_Number;
      Error    : out Sax.Symbols.Symbol) is
   begin
      Internal_Value (Ch, Symbols, True, Val, Error);
   end Value;

   --------------------
   -- Internal_Value --
   --------------------

   procedure Internal_Value
     (Ch             : Unicode.CES.Byte_Sequence;
      Symbols        : Sax.Utils.Symbol_Table;
      Allow_Exponent : Boolean;
      Val            : out Arbitrary_Precision_Number;
      Error          : out Symbol)
   is
      Pos          : Integer := Ch'First;
      First, Last  : Integer;
      C            : Unicode_Char;
      Saw_Exponent : Boolean := False;
      Saw_Point    : Boolean := False;
   begin
      if Ch'Length = 0 then
         Error := Find (Symbols, "Invalid: empty string used as a number");
         Val := Undefined_Number;
         return;
      end if;

      --  Skip leading spaces (because the "whitespace" facet is always
      --  "collapse"

      while Pos <= Ch'Last loop
         First := Pos;
         Encoding.Read (Ch, Pos, C);
         exit when not Is_White_Space (C);
      end loop;

      --  Skip sign, if any

      if C = Plus_Sign or C = Hyphen_Minus then
         Encoding.Read (Ch, Pos, C);
      end if;

      Last := Pos - 1;

      --  Check we only have digits from now on

      loop
         if C = Period then
            if Saw_Point then
               Error := Find
                 (Symbols, "Only one decimal separator allowed in " & Ch);
               Val := Undefined_Number;
               return;
            end if;
            Saw_Point := True;

         elsif C = Latin_Capital_Letter_E
           or else C = Latin_Small_Letter_E
         then
            if Saw_Exponent then
               Error := Find (Symbols, "Only one exponent allowed in " & Ch);
               Val := Undefined_Number;
               return;
            end if;

            if not Allow_Exponent then
               Error := Find
                 (Symbols, "Exponent parent not authorized in " & Ch);
               Val := Undefined_Number;
               return;
            end if;

            Saw_Exponent := True;
            Saw_Point := False;

            if Pos > Ch'Last then
               Error  := Find (Symbols, "No exponent specified in " & Ch);
               Val := Undefined_Number;
               return;
            else
               declare
                  Save : constant Integer := Pos;
               begin
                  Encoding.Read (Ch, Pos, C);
                  if C /= Plus_Sign and C /= Hyphen_Minus then
                     Pos := Save;
                  end if;
               end;
            end if;

         elsif not Is_Digit (C) then
            --  Skip trailing spaces
            if Is_White_Space (C) then
               while Pos <= Ch'Last loop
                  Encoding.Read (Ch, Pos, C);
                  if not Is_White_Space (C) then
                     Error :=
                       Find (Symbols, "Invalid integer: """ & Ch & """");
                     Val := Undefined_Number;
                     return;
                  end if;
               end loop;
               exit;
            else
               Error := Find (Symbols, "Invalid integer: """ & Ch & """");
               Val := Undefined_Number;
               return;
            end if;
         end if;

         Last := Pos - 1;
         exit when Pos > Ch'Last;
         Encoding.Read (Ch, Pos, C);
      end loop;

      Error := No_Symbol;

      if Ch (First .. Last) = "-0" then
         Val := (Value => Find (Symbols, "0"));
      else
         Val := (Value => Find (Symbols, Ch (First .. Last)));
      end if;
   end Internal_Value;

   -----------------------
   -- Value_No_Exponent --
   -----------------------

   procedure Value_No_Exponent
     (Symbols  : Sax.Utils.Symbol_Table;
      Ch       : Unicode.CES.Byte_Sequence;
      Val      : out Arbitrary_Precision_Number;
      Error    : out Sax.Symbols.Symbol) is
   begin
      Internal_Value (Ch, Symbols, False, Val, Error);
   end Value_No_Exponent;

   -------------
   -- Get_Exp --
   -------------

   function Get_Exp (Num : String) return Long_Long_Integer is
      Pos : Integer := Num'Last;
   begin
      while Pos >= Num'First
        and then Num (Pos) /= 'E'
        and then Num (Pos) /= 'e'
      loop
         Pos := Pos - 1;
      end loop;

      if Pos >= Num'First then
         return Long_Long_Integer'Value (Num (Pos + 1 .. Num'Last));
      else
         return 0;
      end if;
   end Get_Exp;

   --------------
   -- Get_Fore --
   --------------

   procedure Get_Fore (Num : String; First, Last : out Integer) is
      Pos : Integer;
   begin
      if Num (Num'First) = '-' or else Num (Num'First) = '+' then
         First := Num'First + 1;
      else
         First := Num'First;
      end if;

      Pos := First;
      while Pos <= Num'Last
        and then Num (Pos) /= '.'
        and then Num (Pos) /= 'E'
        and then Num (Pos) /= 'e'
      loop
         Pos := Pos + 1;
      end loop;

      Last := Pos - 1;
   end Get_Fore;

   -------------------
   -- To_Next_Digit --
   -------------------

   procedure To_Next_Digit (Num : String; Pos : in out Integer) is
   begin
      Pos := Pos + 1;
      if Pos <= Num'Last then
         if Num (Pos) = 'E' or Num (Pos) = 'e' then
            Pos := Num'Last + 1;
         elsif Num (Pos) = '.' then
            Pos := Pos + 1;
         end if;
      end if;
   end To_Next_Digit;

   -------------
   -- Compare --
   -------------

   function Compare (Num1, Num2 : String) return Compare_Result is
      Num1_Negative : constant Boolean := Num1 (Num1'First) = '-';
      Num2_Negative : constant Boolean := Num2 (Num2'First) = '-';

      Exp1, Exp2 : Long_Long_Integer;
      Pos1, Pos2 : Integer;
      Fore_First1, Fore_Last1 : Integer;
      Fore_First2, Fore_Last2 : Integer;
   begin
      --  We have to normalize the numbers (take care of exponents
      if Num1_Negative and not Num2_Negative then
         return Less_Than;

      elsif not Num1_Negative and Num2_Negative then
         return Greater_Than;

      else
         --  They have the same sign
         Exp1 := Get_Exp (Num1);
         Exp2 := Get_Exp (Num2);

         Get_Fore (Num1, Fore_First1, Fore_Last1);
         Get_Fore (Num2, Fore_First2, Fore_Last2);

         --  Different lengths ?
         if Long_Long_Integer (Fore_Last1 - Fore_First1) + Exp1 >
           Long_Long_Integer (Fore_Last2 - Fore_First2) + Exp2
         then
            if Num1_Negative then
               return Less_Than;
            else
               return Greater_Than;
            end if;

         elsif Long_Long_Integer (Fore_Last1 - Fore_First1) + Exp1 <
           Long_Long_Integer (Fore_Last2 - Fore_First2) + Exp2
         then
            if Num1_Negative then
               return Greater_Than;
            else
               return Less_Than;
            end if;
         end if;

         --  Same length of fore parts, we need to compare the digits
         Pos1 := Fore_First1;
         Pos2 := Fore_First2;

         loop
            if Num1 (Pos1) > Num2 (Pos2) then
               if Num1_Negative then
                  return Less_Than;
               else
                  return Greater_Than;
               end if;
            elsif Num1 (Pos1) < Num2 (Pos2) then
               if Num1_Negative then
                  return Greater_Than;
               else
                  return Less_Than;
               end if;
            end if;

            To_Next_Digit (Num1, Pos1);
            To_Next_Digit (Num2, Pos2);

            if Pos1 > Num1'Last
              and then Pos2 > Num2'Last
            then
               return Equal;
            elsif Pos1 > Num1'Last then
               --  If only "0" remain (and because we are in the decimal part),
               --  the two numbers are equal.

               while Num2 (Pos2) = '0' loop
                  To_Next_Digit (Num2, Pos2);
                  if Pos2 > Num2'Last then
                     return Equal;
                  end if;
               end loop;

               if Num1_Negative then
                  return Greater_Than;
               else
                  return Less_Than;
               end if;

            elsif Pos2 > Num2'Last then
               --  If only "0" remain (and because we are in the decimal part),
               --  the two numbers are equal.

               while Num1 (Pos1) = '0' loop
                  To_Next_Digit (Num1, Pos1);
                  if Pos1 > Num1'Last then
                     return Equal;
                  end if;
               end loop;

               if Num1_Negative then
                  return Less_Than;
               else
                  return Greater_Than;
               end if;
            end if;
         end loop;
      end if;
   end Compare;

   ---------
   -- "<" --
   ---------

   function "<" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Get (Num1.Value).all, Get (Num2.Value).all) = Less_Than;
   end "<";

   ----------
   -- "<=" --
   ----------

   function "<=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Get (Num1.Value).all, Get (Num2.Value).all) /=
        Greater_Than;
   end "<=";

   ---------
   -- "=" --
   ---------

   function "=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      if Num1.Value = No_Symbol then
         return Num2.Value = No_Symbol;
      elsif Num2.Value = No_Symbol then
         return False;
      else
         return Compare (Get (Num1.Value).all, Get (Num2.Value).all) = Equal;
      end if;
   end "=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Get (Num1.Value).all, Get (Num2.Value).all) /= Less_Than;
   end ">=";

   ---------
   -- ">" --
   ---------

   function ">" (Num1, Num2 : Arbitrary_Precision_Number) return Boolean is
   begin
      return Compare (Get (Num1.Value).all, Get (Num2.Value).all) =
        Greater_Than;
   end ">";

   ------------------
   -- Check_Digits --
   ------------------

   function Check_Digits
     (Symbols                       : Sax.Utils.Symbol_Table;
      Num                           : Arbitrary_Precision_Number;
      Fraction_Digits, Total_Digits : Integer := -1)
      return Sax.Symbols.Symbol
   is
      Value : constant Cst_Byte_Sequence_Access := Get (Num.Value);
      Exp : constant Long_Long_Integer := Get_Exp (Value.all);
      Fore_First, Fore_Last : Integer;
      Pos : Integer;
      Digits_Count : Natural := 0;
      Frac_Digits  : Natural := 0;
   begin
      Get_Fore (Value.all, Fore_First, Fore_Last);

      --  Now count the significant digits (including fractional part)
      Pos := Value'First;
      if Value (Pos) = '-' or Value (Pos) = '+' then
         Pos := Pos + 1;
      end if;
      if Value (Pos) = '.' then
         Pos := Pos + 1;
      end if;

      while Pos <= Value'Last loop
         Digits_Count := Digits_Count + 1;
         if Pos > Fore_Last then
            Frac_Digits := Frac_Digits + 1;
         end if;
         To_Next_Digit (Value.all, Pos);
      end loop;

      if Total_Digits > 0 then
         --  Gross estimation
         if Long_Long_Integer (Fore_Last - Fore_First) + Exp >=
           Long_Long_Integer (Total_Digits)
         then
            return Find
              (Symbols, "Number " & Value.all
               & " has too many digits (totalDigits is"
               & Integer'Image (Total_Digits) & ')');
         end if;

         if Digits_Count > Total_Digits then
            return Find
              (Symbols, "Number " & Value.all
               & " has too many digits (totalDigits is"
               & Integer'Image (Total_Digits) & ")");
         end if;
      end if;

      if Fraction_Digits >= 0 then
         if Long_Long_Integer (Frac_Digits) - Exp >
           Long_Long_Integer (Fraction_Digits)
         then
            return Find
              (Symbols, "Number " & Value.all
               & " has too many fractional digits (fractionDigits is"
               & Integer'Image (Fraction_Digits) & ')');
         end if;
      end if;

      return No_Symbol;
   end Check_Digits;

   ----------
   -- "<=" --
   ----------

   function "<=" (F1, F2 : XML_Float) return Boolean is
   begin
      case F1.Kind is
         when NaN =>
            return False;
         when Plus_Infinity =>
            return False;
         when Minus_Infinity =>
            return True;
         when Standard_Float =>
            case F2.Kind is
               when NaN =>
                  return False;
               when Plus_Infinity =>
                  return True;
               when Minus_Infinity =>
                  return False;
               when Standard_Float =>
                  if F1.Mantiss < 0.0 then
                     if F2.Mantiss >= 0.0 then
                        return True;
                     else
                        --  Same sign
                        return F1.Exp > F2.Exp
                         or else
                            (F1.Exp = F2.Exp and F1.Mantiss <= F2.Mantiss);
                     end if;

                  else
                     if F2.Mantiss < 0.0 then
                        return False;
                     else
                        return F1.Exp < F2.Exp
                         or else
                            (F1.Exp = F2.Exp and F1.Mantiss <= F2.Mantiss);
                     end if;
                  end if;
            end case;
      end case;
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (F1, F2 : XML_Float) return Boolean is
   begin
      return not (F1 < F2);
   end ">=";

   ---------
   -- ">" --
   ---------

   function ">" (F1, F2 : XML_Float) return Boolean is
   begin
      return not (F1 <= F2);
   end ">";

   ---------
   -- "<" --
   ---------

   function "<" (F1, F2 : XML_Float) return Boolean is
   begin
      case F1.Kind is
         when NaN =>
            return False;
         when Plus_Infinity =>
            return False;
         when Minus_Infinity =>
            return True;
         when Standard_Float =>
            case F2.Kind is
               when NaN =>
                  return False;
               when Plus_Infinity =>
                  return True;
               when Minus_Infinity =>
                  return False;
               when Standard_Float =>
                  if F1.Mantiss < 0.0 then
                     if F2.Mantiss >= 0.0 then
                        return True;
                     else
                        --  Same sign
                        return F1.Exp > F2.Exp
                         or else (F1.Exp = F2.Exp and F1.Mantiss < F2.Mantiss);
                     end if;

                  else
                     if F2.Mantiss < 0.0 then
                        return False;
                     else
                        return F1.Exp < F2.Exp
                         or else (F1.Exp = F2.Exp and F1.Mantiss < F2.Mantiss);
                     end if;
                  end if;
            end case;
      end case;
   end "<";

   -----------
   -- Value --
   -----------

   function Value (Str : String) return XML_Float is
      E : Integer;
      Exp : Integer;
      Mantiss : Long_Long_Float;
   begin
      if Str = "NaN" then
         return XML_Float'(Kind => NaN);
      elsif Str = "INF" then
         return XML_Float'(Kind => Plus_Infinity);
      elsif Str = "-INF" then
         return XML_Float'(Kind => Minus_Infinity);
      else
         --  The issue here is that XML can represent float numbers outside
         --  the range of Long_Long_Float. So we try a basic normalization of
         --  floats (mantissa * 10^exp) with 1.0<=mantissa<10.0

         E := Index (Str, "E");
         if E < Str'First then
            Exp := 0;
            Mantiss := Long_Long_Float'Value (Str);
         else
            Exp := Integer'Value (Str (E + 1 .. Str'Last));
            Mantiss := Long_Long_Float'Value (Str (Str'First .. E - 1));
         end if;

         declare
            Str2 : String (1 .. 200);
            P    : Integer := Str2'First - 1;
            Exp_Chars : constant Natural := 5;
         begin
            System.Img_Real.Set_Image_Real
               (Mantiss,
                S => Str2,
                P => P,
                Fore => 1,
                Aft => 30,
                Exp => Exp_Chars);
            Exp := Exp + Integer'Value (Str2 (P - Exp_Chars + 1 .. P));
            Mantiss := Long_Long_Float'Value
               (Str2 (Str2'First .. P - Exp_Chars - 1));
         end;

         return XML_Float'(Kind  => Standard_Float,
                           Mantiss => Mantiss,
                           Exp     => Exp);
      end if;
   end Value;

   -----------
   -- Image --
   -----------

   function Image (Value : XML_Float) return String is
   begin
      case Value.Kind is
         when NaN =>
            return "NaN";
         when Plus_Infinity =>
            return "INF";
         when Minus_Infinity =>
            return "-INF";
         when Standard_Float =>
            declare
               Str : constant String := Long_Long_Float'Image (Value.Mantiss);
               --  Always has a "E+00", by construction

               Exp : constant String := Integer'Image (Value.Exp);
               E   : Integer := Index (Str, "E");
               F   : Integer := Str'First;
            begin
               if E < Str'First then
                  E := Str'Last + 1;
               end if;

               if Str (F) = ' ' then
                  F := F + 1;
               end if;

               for J in reverse F .. E - 1 loop
                  if Str (J) /= '0' then
                     E := J + 1;
                     exit;
                  end if;
               end loop;

               if Value.Exp = 0 then
                  return Str (F .. E - 1);
               elsif Value.Exp > 0 then
                  return Str (F .. E - 1)
                     & "E+" & Exp (Exp'First + 1 .. Exp'Last);
               else
                  return Str (F .. E - 1) & "E" & Exp;
               end if;
            end;
      end case;
   end Image;

end Schema.Decimal;
