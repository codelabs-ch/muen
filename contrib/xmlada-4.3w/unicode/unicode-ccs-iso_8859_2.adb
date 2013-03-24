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

with Unicode.Names.Latin_Extended_A; use Unicode.Names.Latin_Extended_A;
with Unicode.Names.Latin_1_Supplement; use Unicode.Names.Latin_1_Supplement;
with Unicode.Names.Spacing_Modifier_Letters;
use Unicode.Names.Spacing_Modifier_Letters;
with Ada.Exceptions;                 use Ada.Exceptions;

package body Unicode.CCS.Iso_8859_2 is

   type Conversion_Arr is array (Unicode_Char range 16#00A1# .. 16#00FF#)
     of Unicode_Char;

   To_Unicode_Arr : constant Conversion_Arr :=
     (16#00A1# => Latin_Capital_Letter_A_With_Ogonek,
      16#00A2# => Breve,
      16#00A3# => Latin_Capital_Letter_L_With_Stroke,
      16#00A4# => Currency_Sign,
      16#00A5# => Latin_Capital_Letter_L_With_Caron,
      16#00A6# => Latin_Capital_Letter_S_With_Acute,
      16#00A7# => Section_Sign,
      16#00A8# => Diaeresis,
      16#00A9# => Latin_Capital_Letter_S_With_Caron,
      16#00AA# => Latin_Capital_Letter_S_With_Cedilla,
      16#00AB# => Latin_Capital_Letter_T_With_Caron,
      16#00AC# => Latin_Capital_Letter_Z_With_Acute,
      16#00AD# => Soft_Hyphen,
      16#00AE# => Latin_Capital_Letter_Z_With_Caron,
      16#00AF# => Latin_Capital_Letter_Z_With_Dot_Above,
      16#00B0# => Degree_Sign,
      16#00B1# => Latin_Small_Letter_A_With_Ogonek,
      16#00B2# => Ogonek,
      16#00B3# => Latin_Small_Letter_L_With_Stroke,
      16#00B4# => Acute_Accent,
      16#00B5# => Latin_Small_Letter_L_With_Caron,
      16#00B6# => Latin_Small_Letter_S_With_Acute,
      16#00B7# => Caron,
      16#00B8# => Cedilla,
      16#00B9# => Latin_Small_Letter_S_With_Caron,
      16#00BA# => Latin_Small_Letter_S_With_Cedilla,
      16#00BB# => Latin_Small_Letter_T_With_Caron,
      16#00BC# => Latin_Small_Letter_Z_With_Acute,
      16#00BD# => Double_Acute_Accent,
      16#00BE# => Latin_Small_Letter_Z_With_Caron,
      16#00BF# => Latin_Small_Letter_Z_With_Dot_Above,
      16#00C0# => Latin_Capital_Letter_R_With_Acute,
      16#00C1# => Latin_Capital_Letter_A_With_Acute,
      16#00C2# => Latin_Capital_Letter_A_With_Circumflex,
      16#00C3# => Latin_Capital_Letter_A_With_Breve,
      16#00C4# => Latin_Capital_Letter_A_With_Diaeresis,
      16#00C5# => Latin_Capital_Letter_L_With_Acute,
      16#00C6# => Latin_Capital_Letter_C_With_Acute,
      16#00C7# => Latin_Capital_Letter_C_With_Cedilla,
      16#00C8# => Latin_Capital_Letter_C_With_Caron,
      16#00C9# => Latin_Capital_Letter_E_With_Acute,
      16#00CA# => Latin_Capital_Letter_E_With_Ogonek,
      16#00CB# => Latin_Capital_Letter_E_With_Diaeresis,
      16#00CC# => Latin_Capital_Letter_E_With_Caron,
      16#00CD# => Latin_Capital_Letter_I_With_Acute,
      16#00CE# => Latin_Capital_Letter_I_With_Circumflex,
      16#00CF# => Latin_Capital_Letter_D_With_Caron,
      16#00D0# => Latin_Capital_Letter_D_With_Stroke,
      16#00D1# => Latin_Capital_Letter_N_With_Acute,
      16#00D2# => Latin_Capital_Letter_N_With_Caron,
      16#00D3# => Latin_Capital_Letter_O_With_Acute,
      16#00D4# => Latin_Capital_Letter_O_With_Circumflex,
      16#00D5# => Latin_Capital_Letter_O_With_Double_Acute,
      16#00D6# => Latin_Capital_Letter_O_With_Diaeresis,
      16#00D7# => Multiplication_Sign,
      16#00D8# => Latin_Capital_Letter_R_With_Caron,
      16#00D9# => Latin_Capital_Letter_U_With_Ring_Above,
      16#00DA# => Latin_Capital_Letter_U_With_Acute,
      16#00DB# => Latin_Capital_Letter_U_With_Double_Acute,
      16#00DC# => Latin_Capital_Letter_U_With_Diaeresis,
      16#00DD# => Latin_Capital_Letter_Y_With_Acute,
      16#00DE# => Latin_Capital_Letter_T_With_Cedilla,
      16#00DF# => Latin_Small_Letter_Sharp_S,
      16#00E0# => Latin_Small_Letter_R_With_Acute,
      16#00E1# => Latin_Small_Letter_A_With_Acute,
      16#00E2# => Latin_Small_Letter_A_With_Circumflex,
      16#00E3# => Latin_Small_Letter_A_With_Breve,
      16#00E4# => Latin_Small_Letter_A_With_Diaeresis,
      16#00E5# => Latin_Small_Letter_L_With_Acute,
      16#00E6# => Latin_Small_Letter_C_With_Acute,
      16#00E7# => Latin_Small_Letter_C_With_Cedilla,
      16#00E8# => Latin_Small_Letter_C_With_Caron,
      16#00E9# => Latin_Small_Letter_E_With_Acute,
      16#00EA# => Latin_Small_Letter_E_With_Ogonek,
      16#00EB# => Latin_Small_Letter_E_With_Diaeresis,
      16#00EC# => Latin_Small_Letter_E_With_Caron,
      16#00ED# => Latin_Small_Letter_I_With_Acute,
      16#00EE# => Latin_Small_Letter_I_With_Circumflex,
      16#00EF# => Latin_Small_Letter_D_With_Caron,
      16#00F0# => Latin_Small_Letter_D_With_Stroke,
      16#00F1# => Latin_Small_Letter_N_With_Acute,
      16#00F2# => Latin_Small_Letter_N_With_Caron,
      16#00F3# => Latin_Small_Letter_O_With_Acute,
      16#00F4# => Latin_Small_Letter_O_With_Circumflex,
      16#00F5# => Latin_Small_Letter_O_With_Double_Acute,
      16#00F6# => Latin_Small_Letter_O_With_Diaeresis,
      16#00F7# => Division_Sign,
      16#00F8# => Latin_Small_Letter_R_With_Caron,
      16#00F9# => Latin_Small_Letter_U_With_Ring_Above,
      16#00FA# => Latin_Small_Letter_U_With_Acute,
      16#00FB# => Latin_Small_Letter_U_With_Double_Acute,
      16#00FC# => Latin_Small_Letter_U_With_Diaeresis,
      16#00FD# => Latin_Small_Letter_Y_With_Acute,
      16#00FE# => Latin_Small_Letter_T_With_Cedilla,
      16#00FF# => Dot_Above);

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char > To_Unicode_Arr'Last then
         Raise_Exception
           (Invalid_Code'Identity,
            "code " & Unicode_Char'Image (Char)
            & " is not available in Iso/8859-4");

      elsif Char < To_Unicode_Arr'First then
         return Char;

      else
         return To_Unicode_Arr (Char);
      end if;
   end To_Unicode;

   -------------------
   -- To_Iso_8859_2 --
   -------------------

   function To_Iso_8859_2 (Char : Unicode_Char) return Unicode_Char is
   begin
      case Char is
         when Latin_Capital_Letter_A_With_Ogonek       => return 16#00A1#;
         when Breve                                    => return 16#00A2#;
         when Latin_Capital_Letter_L_With_Stroke       => return 16#00A3#;
         when Latin_Capital_Letter_L_With_Caron        => return 16#00A5#;
         when Latin_Capital_Letter_S_With_Acute        => return 16#00A6#;
         when Latin_Capital_Letter_S_With_Caron        => return 16#00A9#;
         when Latin_Capital_Letter_S_With_Cedilla      => return 16#00AA#;
         when Latin_Capital_Letter_T_With_Caron        => return 16#00AB#;
         when Latin_Capital_Letter_Z_With_Acute        => return 16#00AC#;
         when Latin_Capital_Letter_Z_With_Caron        => return 16#00AE#;
         when Latin_Capital_Letter_Z_With_Dot_Above    => return 16#00AF#;
         when Latin_Small_Letter_A_With_Ogonek         => return 16#00B1#;
         when Ogonek                                   => return 16#00B2#;
         when Latin_Small_Letter_L_With_Stroke         => return 16#00B3#;
         when Latin_Small_Letter_L_With_Caron          => return 16#00B5#;
         when Latin_Small_Letter_S_With_Acute          => return 16#00B6#;
         when Caron                                    => return 16#00B7#;
         when Latin_Small_Letter_S_With_Caron          => return 16#00B9#;
         when Latin_Small_Letter_S_With_Cedilla        => return 16#00BA#;
         when Latin_Small_Letter_T_With_Caron          => return 16#00BB#;
         when Latin_Small_Letter_Z_With_Acute          => return 16#00BC#;
         when Double_Acute_Accent                      => return 16#00BD#;
         when Latin_Small_Letter_Z_With_Caron          => return 16#00BE#;
         when Latin_Small_Letter_Z_With_Dot_Above      => return 16#00BF#;
         when Latin_Capital_Letter_R_With_Acute        => return 16#00C0#;
         when Latin_Capital_Letter_A_With_Breve        => return 16#00C3#;
         when Latin_Capital_Letter_L_With_Acute        => return 16#00C5#;
         when Latin_Capital_Letter_C_With_Acute        => return 16#00C6#;
         when Latin_Capital_Letter_C_With_Caron        => return 16#00C8#;
         when Latin_Capital_Letter_E_With_Ogonek       => return 16#00CA#;
         when Latin_Capital_Letter_E_With_Caron        => return 16#00CC#;
         when Latin_Capital_Letter_D_With_Caron        => return 16#00CF#;
         when Latin_Capital_Letter_D_With_Stroke       => return 16#00D0#;
         when Latin_Capital_Letter_N_With_Acute        => return 16#00D1#;
         when Latin_Capital_Letter_N_With_Caron        => return 16#00D2#;
         when Latin_Capital_Letter_O_With_Double_Acute => return 16#00D5#;
         when Latin_Capital_Letter_R_With_Caron        => return 16#00D8#;
         when Latin_Capital_Letter_U_With_Ring_Above   => return 16#00D9#;
         when Latin_Capital_Letter_U_With_Double_Acute => return 16#00DB#;
         when Latin_Capital_Letter_T_With_Cedilla      => return 16#00DE#;
         when Latin_Small_Letter_R_With_Acute          => return 16#00E0#;
         when Latin_Small_Letter_A_With_Breve          => return 16#00E3#;
         when Latin_Small_Letter_L_With_Acute          => return 16#00E5#;
         when Latin_Small_Letter_C_With_Acute          => return 16#00E6#;
         when Latin_Small_Letter_C_With_Caron          => return 16#00E8#;
         when Latin_Small_Letter_E_With_Ogonek         => return 16#00EA#;
         when Latin_Small_Letter_E_With_Caron          => return 16#00EC#;
         when Latin_Small_Letter_D_With_Caron          => return 16#00EF#;
         when Latin_Small_Letter_D_With_Stroke         => return 16#00F0#;
         when Latin_Small_Letter_N_With_Acute          => return 16#00F1#;
         when Latin_Small_Letter_N_With_Caron          => return 16#00F2#;
         when Latin_Small_Letter_O_With_Double_Acute   => return 16#00F5#;
         when Latin_Small_Letter_R_With_Caron          => return 16#00F8#;
         when Latin_Small_Letter_U_With_Ring_Above     => return 16#00F9#;
         when Latin_Small_Letter_U_With_Double_Acute   => return 16#00FB#;
         when Latin_Small_Letter_T_With_Cedilla        => return 16#00FE#;
         when Dot_Above                                => return 16#00FF#;
         when others   =>
            if Char <= 16#00FF# then
               return Char;
            else
               Raise_Exception
                 (Invalid_Code'Identity,
                  "code " & Unicode_Char'Image (Char)
                  & " is not available in Iso/8859-2");
            end if;
      end case;
   end To_Iso_8859_2;
end Unicode.CCS.Iso_8859_2;
