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

with Ada.Exceptions;                   use Ada.Exceptions;
with Unicode.Names.Latin_1_Supplement; use Unicode.Names.Latin_1_Supplement;
with Unicode.Names.Currency_Symbols;   use Unicode.Names.Currency_Symbols;
with Unicode.Names.Latin_Extended_A;   use Unicode.Names.Latin_Extended_A;

package body Unicode.CCS.Iso_8859_15 is
   type Conversion_Arr is array (Unicode_Char range 16#00A4# .. 16#00Be#)
     of Unicode_Char;

   To_Unicode_Arr : constant Conversion_Arr :=
     (16#00A4# => Euro_Sign,
      16#00A5# => Yen_Sign,
      16#00A6# => Latin_Capital_Letter_S_With_Caron,
      16#00A7# => Section_Sign,
      16#00A8# => Latin_Small_Letter_S_With_Caron,
      16#00A9# => Copyright_Sign,
      16#00AA# => Feminine_Ordinal_Indicator,
      16#00AB# => Left_Pointing_Double_Angle_Quotation_Mark,
      16#00AC# => Not_Sign,
      16#00AD# => Soft_Hyphen,
      16#00AE# => Registered_Sign,
      16#00AF# => Macron,
      16#00B0# => Degree_Sign,
      16#00B1# => Plus_Minus_Sign,
      16#00B2# => Superscript_Two,
      16#00B3# => Superscript_Three,
      16#00B4# => Latin_Capital_Letter_Z_With_Caron,
      16#00B5# => Micro_Sign,
      16#00B6# => Pilcrow_Sign,
      16#00B7# => Middle_Dot,
      16#00B8# => Latin_Small_Letter_Z_With_Caron,
      16#00B9# => Superscript_One,
      16#00BA# => Masculine_Ordinal_Indicator,
      16#00BB# => Right_Pointing_Double_Angle_Quotation_Mark,
      16#00BC# => Latin_Capital_Ligature_Oe,
      16#00BD# => Latin_Small_Ligature_Oe,
      16#00BE# => Latin_Capital_Letter_Y_With_Diaeresis);

   -------------------
   -- To_Iso_8859_1 --
   -------------------

   function To_Iso_8859_15 (Char : Unicode_Char) return Unicode_Char is
   begin
      case Char is
         when Euro_Sign                             => return 16#00A4#;
         when Latin_Capital_Letter_S_With_Caron     => return 16#00A6#;
         when Latin_Small_Letter_S_With_Caron       => return 16#00A8#;
         when Latin_Capital_Letter_Z_With_Caron     => return 16#00B4#;
         when Latin_Small_Letter_Z_With_Caron       => return 16#00B8#;
         when Latin_Capital_Ligature_Oe             => return 16#00BC#;
         when Latin_Small_Ligature_Oe               => return 16#00BD#;
         when Latin_Capital_Letter_Y_With_Diaeresis => return 16#00BE#;
         when others   =>
            if Char <= 16#00FF# then
               return Char;
            else
               Raise_Exception
                 (Invalid_Code'Identity,
                  "code " & Unicode_Char'Image (Char)
                  & " is not available in Iso/8859-15");
            end if;
      end case;
   end To_Iso_8859_15;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char > 16#00FF# then
         Raise_Exception
           (Invalid_Code'Identity,
            "code " & Unicode_Char'Image (Char)
            & " is not available in Iso/8859-15");

      elsif Char < To_Unicode_Arr'First
        or else Char > To_Unicode_Arr'Last
      then
         return Char;

      else
         return To_Unicode_Arr (Char);
      end if;
   end To_Unicode;

end Unicode.CCS.Iso_8859_15;
