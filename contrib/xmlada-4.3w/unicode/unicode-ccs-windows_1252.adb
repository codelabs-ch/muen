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

with Ada.Exceptions; use Ada.Exceptions;
with Unicode.Names.Latin_Extended_A; use Unicode.Names.Latin_Extended_A;
with Unicode.Names.Latin_Extended_B; use Unicode.Names.Latin_Extended_B;
with Unicode.Names.Latin_1_Supplement; use Unicode.Names.Latin_1_Supplement;
with Unicode.Names.Spacing_Modifier_Letters;
use Unicode.Names.Spacing_Modifier_Letters;
with Unicode.Names.Currency_Symbols; use Unicode.Names.Currency_Symbols;
with Unicode.Names.General_Punctuation;
use Unicode.Names.General_Punctuation;
with Unicode.Names.Letterlike_Symbols;
use Unicode.Names.Letterlike_Symbols;

package body Unicode.CCS.Windows_1252 is

   Undefined : constant Unicode_Char := 16#0000#;

   To_Unicode_Arr : constant array (Unicode_Char range <>) of Unicode_Char :=
     (16#0080# => Euro_Sign,
      16#0081# => Undefined,
      16#0082# => Single_Low_9_Quotation_Mark,
      16#0083# => Latin_Small_Letter_F_With_Hook,
      16#0084# => Double_Low_9_Quotation_Mark,
      16#0085# => Horizontal_Ellipsis,
      16#0086# => Dagger,
      16#0087# => Double_Dagger,
      16#0088# => Modifier_Letter_Circumflex_Accent,
      16#0089# => Per_Mille_Sign,
      16#008A# => Latin_Capital_Letter_S_With_Caron,
      16#008B# => Single_Left_Pointing_Angle_Quotation_Mark,
      16#008C# => Latin_Capital_Ligature_Oe,
      16#008D# => Undefined,
      16#008E# => Latin_Capital_Letter_Z_With_Caron,
      16#008F# => Undefined,
      16#0090# => Undefined,
      16#0091# => Left_Single_Quotation_Mark,
      16#0092# => Right_Single_Quotation_Mark,
      16#0093# => Left_Double_Quotation_Mark,
      16#0094# => Right_Double_Quotation_Mark,
      16#0095# => Bullet,
      16#0096# => En_Dash,
      16#0097# => Em_Dash,
      16#0098# => Small_Tilde,
      16#0099# => Trade_Mark_Sign,
      16#009A# => Latin_Small_Letter_S_With_Caron,
      16#009B# => Single_Right_Pointing_Angle_Quotation_Mark,
      16#009C# => Latin_Small_Ligature_Oe,
      16#009D# => Undefined,
      16#009E# => Latin_Small_Letter_Z_With_Caron,
      16#009F# => Latin_Capital_Letter_Y_With_Diaeresis,
      16#00A0# => No_Break_Space,
      16#00A1# => Inverted_Exclamation_Mark,
      16#00A2# => Cent_Sign,
      16#00A3# => Pound_Sign,
      16#00A4# => Currency_Sign,
      16#00A5# => Yen_Sign,
      16#00A6# => Broken_Bar,
      16#00A7# => Section_Sign,
      16#00A8# => Diaeresis,
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
      16#00B4# => Acute_Accent,
      16#00B5# => Micro_Sign,
      16#00B6# => Pilcrow_Sign,
      16#00B7# => Middle_Dot,
      16#00B8# => Cedilla,
      16#00B9# => Superscript_One,
      16#00BA# => Masculine_Ordinal_Indicator,
      16#00BB# => Right_Pointing_Double_Angle_Quotation_Mark,
      16#00BC# => Vulgar_Fraction_One_Quarter,
      16#00BD# => Vulgar_Fraction_One_Half,
      16#00BE# => Vulgar_Fraction_Three_Quarters,
      16#00BF# => Inverted_Question_Mark,
      16#00C0# => Latin_Capital_Letter_A_With_Grave,
      16#00C1# => Latin_Capital_Letter_A_With_Acute,
      16#00C2# => Latin_Capital_Letter_A_With_Circumflex,
      16#00C3# => Latin_Capital_Letter_A_With_Tilde,
      16#00C4# => Latin_Capital_Letter_A_With_Diaeresis,
      16#00C5# => Latin_Capital_Letter_A_With_Ring_Above,
      16#00C6# => Latin_Capital_Letter_Ae,
      16#00C7# => Latin_Capital_Letter_C_With_Cedilla,
      16#00C8# => Latin_Capital_Letter_E_With_Grave,
      16#00C9# => Latin_Capital_Letter_E_With_Acute,
      16#00CA# => Latin_Capital_Letter_E_With_Circumflex,
      16#00CB# => Latin_Capital_Letter_E_With_Diaeresis,
      16#00CC# => Latin_Capital_Letter_I_With_Grave,
      16#00CD# => Latin_Capital_Letter_I_With_Acute,
      16#00CE# => Latin_Capital_Letter_I_With_Circumflex,
      16#00CF# => Latin_Capital_Letter_I_With_Diaeresis,
      16#00D0# => Latin_Capital_Letter_Eth,
      16#00D1# => Latin_Capital_Letter_N_With_Tilde,
      16#00D2# => Latin_Capital_Letter_O_With_Grave,
      16#00D3# => Latin_Capital_Letter_O_With_Acute,
      16#00D4# => Latin_Capital_Letter_O_With_Circumflex,
      16#00D5# => Latin_Capital_Letter_O_With_Tilde,
      16#00D6# => Latin_Capital_Letter_O_With_Diaeresis,
      16#00D7# => Multiplication_Sign,
      16#00D8# => Latin_Capital_Letter_O_With_Stroke,
      16#00D9# => Latin_Capital_Letter_U_With_Grave,
      16#00DA# => Latin_Capital_Letter_U_With_Acute,
      16#00DB# => Latin_Capital_Letter_U_With_Circumflex,
      16#00DC# => Latin_Capital_Letter_U_With_Diaeresis,
      16#00DD# => Latin_Capital_Letter_Y_With_Acute,
      16#00DE# => Latin_Capital_Letter_Thorn,
      16#00DF# => Latin_Small_Letter_Sharp_S,
      16#00E0# => Latin_Small_Letter_A_With_Grave,
      16#00E1# => Latin_Small_Letter_A_With_Acute,
      16#00E2# => Latin_Small_Letter_A_With_Circumflex,
      16#00E3# => Latin_Small_Letter_A_With_Tilde,
      16#00E4# => Latin_Small_Letter_A_With_Diaeresis,
      16#00E5# => Latin_Small_Letter_A_With_Ring_Above,
      16#00E6# => Latin_Small_Letter_Ae,
      16#00E7# => Latin_Small_Letter_C_With_Cedilla,
      16#00E8# => Latin_Small_Letter_E_With_Grave,
      16#00E9# => Latin_Small_Letter_E_With_Acute,
      16#00EA# => Latin_Small_Letter_E_With_Circumflex,
      16#00EB# => Latin_Small_Letter_E_With_Diaeresis,
      16#00EC# => Latin_Small_Letter_I_With_Grave,
      16#00ED# => Latin_Small_Letter_I_With_Acute,
      16#00EE# => Latin_Small_Letter_I_With_Circumflex,
      16#00EF# => Latin_Small_Letter_I_With_Diaeresis,
      16#00F0# => Latin_Small_Letter_Eth,
      16#00F1# => Latin_Small_Letter_N_With_Tilde,
      16#00F2# => Latin_Small_Letter_O_With_Grave,
      16#00F3# => Latin_Small_Letter_O_With_Acute,
      16#00F4# => Latin_Small_Letter_O_With_Circumflex,
      16#00F5# => Latin_Small_Letter_O_With_Tilde,
      16#00F6# => Latin_Small_Letter_O_With_Diaeresis,
      16#00F7# => Division_Sign,
      16#00F8# => Latin_Small_Letter_O_With_Stroke,
      16#00F9# => Latin_Small_Letter_U_With_Grave,
      16#00FA# => Latin_Small_Letter_U_With_Acute,
      16#00FB# => Latin_Small_Letter_U_With_Circumflex,
      16#00FC# => Latin_Small_Letter_U_With_Diaeresis,
      16#00FD# => Latin_Small_Letter_Y_With_Acute,
      16#00FE# => Latin_Small_Letter_Thorn,
      16#00FF# => Latin_Small_Letter_Y_With_Diaeresis);

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char > To_Unicode_Arr'Last then
         Raise_Exception
           (Invalid_Code'Identity,
            "code " & Unicode_Char'Image (Char)
            & " is not available in Windows-1252");
      elsif Char < To_Unicode_Arr'First then
         return Char;

      else
         return To_Unicode_Arr (Char);
      end if;
   end To_Unicode;

   ---------------------
   -- To_Windows_1252 --
   ---------------------

   function To_Windows_1252 (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char < To_Unicode_Arr'First then
         return Char;
      else
         case Char is
            when Euro_Sign                                  => return 16#0080#;
            when Single_Low_9_Quotation_Mark                => return 16#0082#;
            when Latin_Small_Letter_F_With_Hook             => return 16#0083#;
            when Double_Low_9_Quotation_Mark                => return 16#0084#;
            when Horizontal_Ellipsis                        => return 16#0085#;
            when Dagger                                     => return 16#0086#;
            when Double_Dagger                              => return 16#0087#;
            when Modifier_Letter_Circumflex_Accent          => return 16#0088#;
            when Per_Mille_Sign                             => return 16#0089#;
            when Latin_Capital_Letter_S_With_Caron          => return 16#008A#;
            when Single_Left_Pointing_Angle_Quotation_Mark  => return 16#008B#;
            when Latin_Capital_Ligature_Oe                  => return 16#008C#;
            when Latin_Capital_Letter_Z_With_Caron          => return 16#008E#;
            when Left_Single_Quotation_Mark                 => return 16#0091#;
            when Right_Single_Quotation_Mark                => return 16#0092#;
            when Left_Double_Quotation_Mark                 => return 16#0093#;
            when Right_Double_Quotation_Mark                => return 16#0094#;
            when Bullet                                     => return 16#0095#;
            when En_Dash                                    => return 16#0096#;
            when Em_Dash                                    => return 16#0097#;
            when Small_Tilde                                => return 16#0098#;
            when Trade_Mark_Sign                            => return 16#0099#;
            when Latin_Small_Letter_S_With_Caron            => return 16#009A#;
            when Single_Right_Pointing_Angle_Quotation_Mark => return 16#009B#;
            when Latin_Small_Ligature_Oe                    => return 16#009C#;
            when Latin_Small_Letter_Z_With_Caron            => return 16#009E#;
            when Latin_Capital_Letter_Y_With_Diaeresis      => return 16#009F#;
            when No_Break_Space                             => return 16#00A0#;
            when Inverted_Exclamation_Mark                  => return 16#00A1#;
            when Cent_Sign                                  => return 16#00A2#;
            when Pound_Sign                                 => return 16#00A3#;
            when Currency_Sign                              => return 16#00A4#;
            when Yen_Sign                                   => return 16#00A5#;
            when Broken_Bar                                 => return 16#00A6#;
            when Section_Sign                               => return 16#00A7#;
            when Diaeresis                                  => return 16#00A8#;
            when Copyright_Sign                             => return 16#00A9#;
            when Feminine_Ordinal_Indicator                 => return 16#00AA#;
            when Left_Pointing_Double_Angle_Quotation_Mark  => return 16#00AB#;
            when Not_Sign                                   => return 16#00AC#;
            when Soft_Hyphen                                => return 16#00AD#;
            when Registered_Sign                            => return 16#00AE#;
            when Macron                                     => return 16#00AF#;
            when Degree_Sign                                => return 16#00B0#;
            when Plus_Minus_Sign                            => return 16#00B1#;
            when Superscript_Two                            => return 16#00B2#;
            when Superscript_Three                          => return 16#00B3#;
            when Acute_Accent                               => return 16#00B4#;
            when Micro_Sign                                 => return 16#00B5#;
            when Pilcrow_Sign                               => return 16#00B6#;
            when Middle_Dot                                 => return 16#00B7#;
            when Cedilla                                    => return 16#00B8#;
            when Superscript_One                            => return 16#00B9#;
            when Masculine_Ordinal_Indicator                => return 16#00BA#;
            when Right_Pointing_Double_Angle_Quotation_Mark => return 16#00BB#;
            when Vulgar_Fraction_One_Quarter                => return 16#00BC#;
            when Vulgar_Fraction_One_Half                   => return 16#00BD#;
            when Vulgar_Fraction_Three_Quarters             => return 16#00BE#;
            when Inverted_Question_Mark                     => return 16#00BF#;
            when Latin_Capital_Letter_A_With_Grave          => return 16#00C0#;
            when Latin_Capital_Letter_A_With_Acute          => return 16#00C1#;
            when Latin_Capital_Letter_A_With_Circumflex     => return 16#00C2#;
            when Latin_Capital_Letter_A_With_Tilde          => return 16#00C3#;
            when Latin_Capital_Letter_A_With_Diaeresis      => return 16#00C4#;
            when Latin_Capital_Letter_A_With_Ring_Above     => return 16#00C5#;
            when Latin_Capital_Letter_Ae                    => return 16#00C6#;
            when Latin_Capital_Letter_C_With_Cedilla        => return 16#00C7#;
            when Latin_Capital_Letter_E_With_Grave          => return 16#00C8#;
            when Latin_Capital_Letter_E_With_Acute          => return 16#00C9#;
            when Latin_Capital_Letter_E_With_Circumflex     => return 16#00CA#;
            when Latin_Capital_Letter_E_With_Diaeresis      => return 16#00CB#;
            when Latin_Capital_Letter_I_With_Grave          => return 16#00CC#;
            when Latin_Capital_Letter_I_With_Acute          => return 16#00CD#;
            when Latin_Capital_Letter_I_With_Circumflex     => return 16#00CE#;
            when Latin_Capital_Letter_I_With_Diaeresis      => return 16#00CF#;
            when Latin_Capital_Letter_Eth                   => return 16#00D0#;
            when Latin_Capital_Letter_N_With_Tilde          => return 16#00D1#;
            when Latin_Capital_Letter_O_With_Grave          => return 16#00D2#;
            when Latin_Capital_Letter_O_With_Acute          => return 16#00D3#;
            when Latin_Capital_Letter_O_With_Circumflex     => return 16#00D4#;
            when Latin_Capital_Letter_O_With_Tilde          => return 16#00D5#;
            when Latin_Capital_Letter_O_With_Diaeresis      => return 16#00D6#;
            when Multiplication_Sign                        => return 16#00D7#;
            when Latin_Capital_Letter_O_With_Stroke         => return 16#00D8#;
            when Latin_Capital_Letter_U_With_Grave          => return 16#00D9#;
            when Latin_Capital_Letter_U_With_Acute          => return 16#00DA#;
            when Latin_Capital_Letter_U_With_Circumflex     => return 16#00DB#;
            when Latin_Capital_Letter_U_With_Diaeresis      => return 16#00DC#;
            when Latin_Capital_Letter_Y_With_Acute          => return 16#00DD#;
            when Latin_Capital_Letter_Thorn                 => return 16#00DE#;
            when Latin_Small_Letter_Sharp_S                 => return 16#00DF#;
            when Latin_Small_Letter_A_With_Grave            => return 16#00E0#;
            when Latin_Small_Letter_A_With_Acute            => return 16#00E1#;
            when Latin_Small_Letter_A_With_Circumflex       => return 16#00E2#;
            when Latin_Small_Letter_A_With_Tilde            => return 16#00E3#;
            when Latin_Small_Letter_A_With_Diaeresis        => return 16#00E4#;
            when Latin_Small_Letter_A_With_Ring_Above       => return 16#00E5#;
            when Latin_Small_Letter_Ae                      => return 16#00E6#;
            when Latin_Small_Letter_C_With_Cedilla          => return 16#00E7#;
            when Latin_Small_Letter_E_With_Grave            => return 16#00E8#;
            when Latin_Small_Letter_E_With_Acute            => return 16#00E9#;
            when Latin_Small_Letter_E_With_Circumflex       => return 16#00EA#;
            when Latin_Small_Letter_E_With_Diaeresis        => return 16#00EB#;
            when Latin_Small_Letter_I_With_Grave            => return 16#00EC#;
            when Latin_Small_Letter_I_With_Acute            => return 16#00ED#;
            when Latin_Small_Letter_I_With_Circumflex       => return 16#00EE#;
            when Latin_Small_Letter_I_With_Diaeresis        => return 16#00EF#;
            when Latin_Small_Letter_Eth                     => return 16#00F0#;
            when Latin_Small_Letter_N_With_Tilde            => return 16#00F1#;
            when Latin_Small_Letter_O_With_Grave            => return 16#00F2#;
            when Latin_Small_Letter_O_With_Acute            => return 16#00F3#;
            when Latin_Small_Letter_O_With_Circumflex       => return 16#00F4#;
            when Latin_Small_Letter_O_With_Tilde            => return 16#00F5#;
            when Latin_Small_Letter_O_With_Diaeresis        => return 16#00F6#;
            when Division_Sign                              => return 16#00F7#;
            when Latin_Small_Letter_O_With_Stroke           => return 16#00F8#;
            when Latin_Small_Letter_U_With_Grave            => return 16#00F9#;
            when Latin_Small_Letter_U_With_Acute            => return 16#00FA#;
            when Latin_Small_Letter_U_With_Circumflex       => return 16#00FB#;
            when Latin_Small_Letter_U_With_Diaeresis        => return 16#00FC#;
            when Latin_Small_Letter_Y_With_Acute            => return 16#00FD#;
            when Latin_Small_Letter_Thorn                   => return 16#00FE#;
            when Latin_Small_Letter_Y_With_Diaeresis        => return 16#00FF#;
            when others =>
               Raise_Exception
                 (Invalid_Code'Identity,
                  "code " & Unicode_Char'Image (Char)
                  & " is not available in Windows-1252");
         end case;
      end if;
   end To_Windows_1252;

end Unicode.CCS.Windows_1252;
