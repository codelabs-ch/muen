------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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
with Unicode.Names.Cyrillic;           use Unicode.Names.Cyrillic;
with Unicode.Names.Basic_Latin;        use Unicode.Names.Basic_Latin;
with Unicode.Names.Latin_1_Supplement; use Unicode.Names.Latin_1_Supplement;
with Unicode.Names.Currency_Symbols;   use Unicode.Names.Currency_Symbols;
with Unicode.Names.General_Punctuation;
use Unicode.Names.General_Punctuation;
with Unicode.Names.Letterlike_Symbols;
use Unicode.Names.Letterlike_Symbols;

package body Unicode.CCS.Windows_1251 is

   Undefined : constant Unicode_Char := 16#0000#;

   To_Unicode_Arr : constant array (Unicode_Char range <>) of Unicode_Char :=
      (16#0001# => Start_Of_Heading,
       16#0002# => Start_Of_Text,
       16#0003# => End_Of_Text,
       16#0004# => End_Of_Transmission,
       16#0005# => Enquiry,
       16#0006# => Acknowledge,
       16#0007# => Bell,
       16#0008# => Backspace,
       16#0009# => Horizontal_Tabulation,
       16#000A# => Line_Feed,
       16#000B# => Vertical_Tabulation,
       16#000C# => Form_Feed,
       16#000D# => Carriage_Return,
       16#000E# => Shift_Out,
       16#000F# => Shift_In,
       16#0010# => Data_Link_Escape,
       16#0011# => Device_Control_One,
       16#0012# => Device_Control_Two,
       16#0013# => Device_Control_Three,
       16#0014# => Device_Control_Four,
       16#0015# => Negative_Acknowledge,
       16#0016# => Synchronous_Idle,
       16#0017# => End_Of_Transmission_Block,
       16#0018# => Cancel,
       16#0019# => End_Of_Medium,
       16#001A# => Substitute,
       16#001B# => Escape,
       16#001C# => File_Separator,
       16#001D# => Group_Separator,
       16#001E# => Record_Separator,
       16#001F# => Unit_Separator,
       16#0020# => Space,
       16#0021# => Exclamation_Mark,
       16#0022# => Quotation_Mark,
       16#0023# => Number_Sign,
       16#0024# => Dollar_Sign,
       16#0025# => Percent_Sign,
       16#0026# => Ampersand,
       16#0027# => Apostrophe,
       16#0028# => Left_Parenthesis,
       16#0029# => Right_Parenthesis,
       16#002A# => Asterisk,
       16#002B# => Plus_Sign,
       16#002C# => Comma,
       16#002D# => Hyphen_Minus,
       16#002E# => Full_Stop,
       16#002F# => Unicode.Names.Basic_Latin.Solidus,
       16#0030# => Digit_Zero,
       16#0031# => Digit_One,
       16#0032# => Digit_Two,
       16#0033# => Digit_Three,
       16#0034# => Digit_Four,
       16#0035# => Digit_Five,
       16#0036# => Digit_Six,
       16#0037# => Digit_Seven,
       16#0038# => Digit_Eight,
       16#0039# => Digit_Nine,
       16#003A# => Colon,
       16#003B# => Semicolon,
       16#003C# => Less_Than_Sign,
       16#003D# => Equals_Sign,
       16#003E# => Greater_Than_Sign,
       16#003F# => Question_Mark,
       16#0040# => Commercial_At,
       16#0041# => Latin_Capital_Letter_A,
       16#0042# => Latin_Capital_Letter_B,
       16#0043# => Latin_Capital_Letter_C,
       16#0044# => Latin_Capital_Letter_D,
       16#0045# => Latin_Capital_Letter_E,
       16#0046# => Latin_Capital_Letter_F,
       16#0047# => Latin_Capital_Letter_G,
       16#0048# => Latin_Capital_Letter_H,
       16#0049# => Latin_Capital_Letter_I,
       16#004A# => Latin_Capital_Letter_J,
       16#004B# => Latin_Capital_Letter_K,
       16#004C# => Latin_Capital_Letter_L,
       16#004D# => Latin_Capital_Letter_M,
       16#004E# => Latin_Capital_Letter_N,
       16#004F# => Latin_Capital_Letter_O,
       16#0050# => Latin_Capital_Letter_P,
       16#0051# => Latin_Capital_Letter_Q,
       16#0052# => Latin_Capital_Letter_R,
       16#0053# => Latin_Capital_Letter_S,
       16#0054# => Latin_Capital_Letter_T,
       16#0055# => Latin_Capital_Letter_U,
       16#0056# => Latin_Capital_Letter_V,
       16#0057# => Latin_Capital_Letter_W,
       16#0058# => Latin_Capital_Letter_X,
       16#0059# => Latin_Capital_Letter_Y,
       16#005A# => Latin_Capital_Letter_Z,
       16#005B# => Left_Square_Bracket,
       16#005C# => Reverse_Solidus,
       16#005D# => Right_Square_Bracket,
       16#005E# => Circumflex_Accent,
       16#005F# => Low_Line,
       16#0060# => Grave_Accent,
       16#0061# => Latin_Small_Letter_A,
       16#0062# => Latin_Small_Letter_B,
       16#0063# => Latin_Small_Letter_C,
       16#0064# => Latin_Small_Letter_D,
       16#0065# => Latin_Small_Letter_E,
       16#0066# => Latin_Small_Letter_F,
       16#0067# => Latin_Small_Letter_G,
       16#0068# => Latin_Small_Letter_H,
       16#0069# => Latin_Small_Letter_I,
       16#006A# => Latin_Small_Letter_J,
       16#006B# => Latin_Small_Letter_K,
       16#006C# => Latin_Small_Letter_L,
       16#006D# => Latin_Small_Letter_M,
       16#006E# => Latin_Small_Letter_N,
       16#006F# => Latin_Small_Letter_O,
       16#0070# => Latin_Small_Letter_P,
       16#0071# => Latin_Small_Letter_Q,
       16#0072# => Latin_Small_Letter_R,
       16#0073# => Latin_Small_Letter_S,
       16#0074# => Latin_Small_Letter_T,
       16#0075# => Latin_Small_Letter_U,
       16#0076# => Latin_Small_Letter_V,
       16#0077# => Latin_Small_Letter_W,
       16#0078# => Latin_Small_Letter_X,
       16#0079# => Latin_Small_Letter_Y,
       16#007A# => Latin_Small_Letter_Z,
       16#007B# => Left_Curly_Bracket,
       16#007C# => Vertical_Line,
       16#007D# => Right_Curly_Bracket,
       16#007E# => Tilde,
       16#007F# => Delete,
       16#0080# => Cyrillic_Capital_Letter_Dje,
       16#0081# => Cyrillic_Capital_Letter_Gje,
       16#0082# => Single_Low_9_Quotation_Mark,
       16#0083# => Cyrillic_Small_Letter_Gje,
       16#0084# => Double_Low_9_Quotation_Mark,
       16#0085# => Horizontal_Ellipsis,
       16#0086# => Dagger,
       16#0087# => Double_Dagger,
       16#0088# => Euro_Sign,
       16#0089# => Per_Mille_Sign,
       16#008A# => Cyrillic_Capital_Letter_Lje,
       16#008B# => Single_Left_Pointing_Angle_Quotation_Mark,
       16#008C# => Cyrillic_Capital_Letter_Nje,
       16#008D# => Cyrillic_Capital_Letter_Kje,
       16#008E# => Cyrillic_Capital_Letter_Tshe,
       16#008F# => Cyrillic_Capital_Letter_Dzhe,
       16#0090# => Cyrillic_Small_Letter_Dje,
       16#0091# => Left_Single_Quotation_Mark,
       16#0092# => Right_Single_Quotation_Mark,
       16#0093# => Left_Double_Quotation_Mark,
       16#0094# => Right_Double_Quotation_Mark,
       16#0095# => Bullet,
       16#0096# => En_Dash,
       16#0097# => Em_Dash,
       16#0098# => Undefined,
       16#0099# => Trade_Mark_Sign,
       16#009A# => Cyrillic_Small_Letter_Lje,
       16#009B# => Single_Right_Pointing_Angle_Quotation_Mark,
       16#009C# => Cyrillic_Small_Letter_Nje,
       16#009D# => Cyrillic_Small_Letter_Kje,
       16#009E# => Cyrillic_Small_Letter_Tshe,
       16#009F# => Cyrillic_Small_Letter_Dzhe,
       16#00A0# => No_Break_Space,
       16#00A1# => Cyrillic_Capital_Letter_Short_U,
       16#00A2# => Cyrillic_Small_Letter_Short_U,
       16#00A3# => Cyrillic_Capital_Letter_Je,
       16#00A4# => Currency_Sign,
       16#00A5# => Cyrillic_Capital_Letter_Ghe_With_Upturn,
       16#00A6# => Broken_Bar,
       16#00A7# => Section_Sign,
       16#00A8# => Cyrillic_Capital_Letter_Io,
       16#00A9# => Copyright_Sign,
       16#00AA# => Cyrillic_Capital_Letter_Ukrainian_Ie,
       16#00AB# => Left_Pointing_Double_Angle_Quotation_Mark,
       16#00AC# => Not_Sign,
       16#00AD# => Soft_Hyphen,
       16#00AE# => Registered_Sign,
       16#00AF# => Cyrillic_Capital_Letter_Yi,
       16#00B0# => Degree_Sign,
       16#00B1# => Plus_Minus_Sign,
       16#00B2# => Cyrillic_Capital_Letter_Byelorussian_Ukrainian_I,
       16#00B3# => Cyrillic_Small_Letter_Byelorussian_Ukrainian_I,
       16#00B4# => Cyrillic_Small_Letter_Ghe_With_Upturn,
       16#00B5# => Micro_Sign,
       16#00B6# => Pilcrow_Sign,
       16#00B7# => Middle_Dot,
       16#00B8# => Cyrillic_Small_Letter_Io,
       16#00B9# => Numero_Sign,
       16#00BA# => Cyrillic_Small_Letter_Ukrainian_Ie,
       16#00BB# => Right_Pointing_Double_Angle_Quotation_Mark,
       16#00BC# => Cyrillic_Small_Letter_Je,
       16#00BD# => Cyrillic_Capital_Letter_Dze,
       16#00BE# => Cyrillic_Small_Letter_Dze,
       16#00BF# => Cyrillic_Small_Letter_Yi,
       16#00C0# => Cyrillic_Capital_Letter_A,
       16#00C1# => Cyrillic_Capital_Letter_Be,
       16#00C2# => Cyrillic_Capital_Letter_Ve,
       16#00C3# => Cyrillic_Capital_Letter_Ghe,
       16#00C4# => Cyrillic_Capital_Letter_De,
       16#00C5# => Cyrillic_Capital_Letter_Ie,
       16#00C6# => Cyrillic_Capital_Letter_Zhe,
       16#00C7# => Cyrillic_Capital_Letter_Ze,
       16#00C8# => Cyrillic_Capital_Letter_I,
       16#00C9# => Cyrillic_Capital_Letter_Short_I,
       16#00CA# => Cyrillic_Capital_Letter_Ka,
       16#00CB# => Cyrillic_Capital_Letter_El,
       16#00CC# => Cyrillic_Capital_Letter_Em,
       16#00CD# => Cyrillic_Capital_Letter_En,
       16#00CE# => Cyrillic_Capital_Letter_O,
       16#00CF# => Cyrillic_Capital_Letter_Pe,
       16#00D0# => Cyrillic_Capital_Letter_Er,
       16#00D1# => Cyrillic_Capital_Letter_Es,
       16#00D2# => Cyrillic_Capital_Letter_Te,
       16#00D3# => Cyrillic_Capital_Letter_U,
       16#00D4# => Cyrillic_Capital_Letter_Ef,
       16#00D5# => Cyrillic_Capital_Letter_Ha,
       16#00D6# => Cyrillic_Capital_Letter_Tse,
       16#00D7# => Cyrillic_Capital_Letter_Che,
       16#00D8# => Cyrillic_Capital_Letter_Sha,
       16#00D9# => Cyrillic_Capital_Letter_Shcha,
       16#00DA# => Cyrillic_Capital_Letter_Hard_Sign,
       16#00DB# => Cyrillic_Capital_Letter_Yeru,
       16#00DC# => Cyrillic_Capital_Letter_Soft_Sign,
       16#00DD# => Cyrillic_Capital_Letter_E,
       16#00DE# => Cyrillic_Capital_Letter_Yu,
       16#00DF# => Cyrillic_Capital_Letter_Ya,
       16#00E0# => Cyrillic_Small_Letter_A,
       16#00E1# => Cyrillic_Small_Letter_Be,
       16#00E2# => Cyrillic_Small_Letter_Ve,
       16#00E3# => Cyrillic_Small_Letter_Ghe,
       16#00E4# => Cyrillic_Small_Letter_De,
       16#00E5# => Cyrillic_Small_Letter_Ie,
       16#00E6# => Cyrillic_Small_Letter_Zhe,
       16#00E7# => Cyrillic_Small_Letter_Ze,
       16#00E8# => Cyrillic_Small_Letter_I,
       16#00E9# => Cyrillic_Small_Letter_Short_I,
       16#00EA# => Cyrillic_Small_Letter_Ka,
       16#00EB# => Cyrillic_Small_Letter_El,
       16#00EC# => Cyrillic_Small_Letter_Em,
       16#00ED# => Cyrillic_Small_Letter_En,
       16#00EE# => Cyrillic_Small_Letter_O,
       16#00EF# => Cyrillic_Small_Letter_Pe,
       16#00F0# => Cyrillic_Small_Letter_Er,
       16#00F1# => Cyrillic_Small_Letter_Es,
       16#00F2# => Cyrillic_Small_Letter_Te,
       16#00F3# => Cyrillic_Small_Letter_U,
       16#00F4# => Cyrillic_Small_Letter_Ef,
       16#00F5# => Cyrillic_Small_Letter_Ha,
       16#00F6# => Cyrillic_Small_Letter_Tse,
       16#00F7# => Cyrillic_Small_Letter_Che,
       16#00F8# => Cyrillic_Small_Letter_Sha,
       16#00F9# => Cyrillic_Small_Letter_Shcha,
       16#00FA# => Cyrillic_Small_Letter_Hard_Sign,
       16#00FB# => Cyrillic_Small_Letter_Yeru,
       16#00FC# => Cyrillic_Small_Letter_Soft_Sign,
       16#00FD# => Cyrillic_Small_Letter_E,
       16#00FE# => Cyrillic_Small_Letter_Yu,
       16#00FF# => Cyrillic_Small_Letter_Ya);

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char > To_Unicode_Arr'Last then
         Raise_Exception
           (Invalid_Code'Identity,
            "code " & Unicode_Char'Image (Char)
            & " is not available in Windows-1251");
      elsif Char < To_Unicode_Arr'First then
         return Char;

      else
         return To_Unicode_Arr (Char);
      end if;
   end To_Unicode;

   ---------------------
   -- To_Windows_1251 --
   ---------------------

   function To_Windows_1251 (Char : Unicode_Char) return Unicode_Char is
   begin
      if Char < To_Unicode_Arr'First then
         return Char;
      else
         case Char is
            when Start_Of_Heading                           => return 16#0001#;
            when Start_Of_Text                              => return 16#0002#;
            when End_Of_Text                                => return 16#0003#;
            when End_Of_Transmission                        => return 16#0004#;
            when Enquiry                                    => return 16#0005#;
            when Acknowledge                                => return 16#0006#;
            when Bell                                       => return 16#0007#;
            when Backspace                                  => return 16#0008#;
            when Horizontal_Tabulation                      => return 16#0009#;
            when Line_Feed                                  => return 16#000A#;
            when Vertical_Tabulation                        => return 16#000B#;
            when Form_Feed                                  => return 16#000C#;
            when Carriage_Return                            => return 16#000D#;
            when Shift_Out                                  => return 16#000E#;
            when Shift_In                                   => return 16#000F#;
            when Data_Link_Escape                           => return 16#0010#;
            when Device_Control_One                         => return 16#0011#;
            when Device_Control_Two                         => return 16#0012#;
            when Device_Control_Three                       => return 16#0013#;
            when Device_Control_Four                        => return 16#0014#;
            when Negative_Acknowledge                       => return 16#0015#;
            when Synchronous_Idle                           => return 16#0016#;
            when End_Of_Transmission_Block                  => return 16#0017#;
            when Cancel                                     => return 16#0018#;
            when End_Of_Medium                              => return 16#0019#;
            when Substitute                                 => return 16#001A#;
            when Escape                                     => return 16#001B#;
            when File_Separator                             => return 16#001C#;
            when Group_Separator                            => return 16#001D#;
            when Record_Separator                           => return 16#001E#;
            when Unit_Separator                             => return 16#001F#;
            when Space                                      => return 16#0020#;
            when Exclamation_Mark                           => return 16#0021#;
            when Quotation_Mark                             => return 16#0022#;
            when Number_Sign                                => return 16#0023#;
            when Dollar_Sign                                => return 16#0024#;
            when Percent_Sign                               => return 16#0025#;
            when Ampersand                                  => return 16#0026#;
            when Apostrophe                                 => return 16#0027#;
            when Left_Parenthesis                           => return 16#0028#;
            when Right_Parenthesis                          => return 16#0029#;
            when Asterisk                                   => return 16#002A#;
            when Plus_Sign                                  => return 16#002B#;
            when Comma                                      => return 16#002C#;
            when Hyphen_Minus                               => return 16#002D#;
            when Full_Stop                                  => return 16#002E#;
            when Unicode.Names.Basic_Latin.Solidus          => return 16#002F#;
            when Digit_Zero                                 => return 16#0030#;
            when Digit_One                                  => return 16#0031#;
            when Digit_Two                                  => return 16#0032#;
            when Digit_Three                                => return 16#0033#;
            when Digit_Four                                 => return 16#0034#;
            when Digit_Five                                 => return 16#0035#;
            when Digit_Six                                  => return 16#0036#;
            when Digit_Seven                                => return 16#0037#;
            when Digit_Eight                                => return 16#0038#;
            when Digit_Nine                                 => return 16#0039#;
            when Colon                                      => return 16#003A#;
            when Semicolon                                  => return 16#003B#;
            when Less_Than_Sign                             => return 16#003C#;
            when Equals_Sign                                => return 16#003D#;
            when Greater_Than_Sign                          => return 16#003E#;
            when Question_Mark                              => return 16#003F#;
            when Commercial_At                              => return 16#0040#;
            when Latin_Capital_Letter_A                     => return 16#0041#;
            when Latin_Capital_Letter_B                     => return 16#0042#;
            when Latin_Capital_Letter_C                     => return 16#0043#;
            when Latin_Capital_Letter_D                     => return 16#0044#;
            when Latin_Capital_Letter_E                     => return 16#0045#;
            when Latin_Capital_Letter_F                     => return 16#0046#;
            when Latin_Capital_Letter_G                     => return 16#0047#;
            when Latin_Capital_Letter_H                     => return 16#0048#;
            when Latin_Capital_Letter_I                     => return 16#0049#;
            when Latin_Capital_Letter_J                     => return 16#004A#;
            when Latin_Capital_Letter_K                     => return 16#004B#;
            when Latin_Capital_Letter_L                     => return 16#004C#;
            when Latin_Capital_Letter_M                     => return 16#004D#;
            when Latin_Capital_Letter_N                     => return 16#004E#;
            when Latin_Capital_Letter_O                     => return 16#004F#;
            when Latin_Capital_Letter_P                     => return 16#0050#;
            when Latin_Capital_Letter_Q                     => return 16#0051#;
            when Latin_Capital_Letter_R                     => return 16#0052#;
            when Latin_Capital_Letter_S                     => return 16#0053#;
            when Latin_Capital_Letter_T                     => return 16#0054#;
            when Latin_Capital_Letter_U                     => return 16#0055#;
            when Latin_Capital_Letter_V                     => return 16#0056#;
            when Latin_Capital_Letter_W                     => return 16#0057#;
            when Latin_Capital_Letter_X                     => return 16#0058#;
            when Latin_Capital_Letter_Y                     => return 16#0059#;
            when Latin_Capital_Letter_Z                     => return 16#005A#;
            when Left_Square_Bracket                        => return 16#005B#;
            when Reverse_Solidus                            => return 16#005C#;
            when Right_Square_Bracket                       => return 16#005D#;
            when Circumflex_Accent                          => return 16#005E#;
            when Low_Line                                   => return 16#005F#;
            when Grave_Accent                               => return 16#0060#;
            when Latin_Small_Letter_A                       => return 16#0061#;
            when Latin_Small_Letter_B                       => return 16#0062#;
            when Latin_Small_Letter_C                       => return 16#0063#;
            when Latin_Small_Letter_D                       => return 16#0064#;
            when Latin_Small_Letter_E                       => return 16#0065#;
            when Latin_Small_Letter_F                       => return 16#0066#;
            when Latin_Small_Letter_G                       => return 16#0067#;
            when Latin_Small_Letter_H                       => return 16#0068#;
            when Latin_Small_Letter_I                       => return 16#0069#;
            when Latin_Small_Letter_J                       => return 16#006A#;
            when Latin_Small_Letter_K                       => return 16#006B#;
            when Latin_Small_Letter_L                       => return 16#006C#;
            when Latin_Small_Letter_M                       => return 16#006D#;
            when Latin_Small_Letter_N                       => return 16#006E#;
            when Latin_Small_Letter_O                       => return 16#006F#;
            when Latin_Small_Letter_P                       => return 16#0070#;
            when Latin_Small_Letter_Q                       => return 16#0071#;
            when Latin_Small_Letter_R                       => return 16#0072#;
            when Latin_Small_Letter_S                       => return 16#0073#;
            when Latin_Small_Letter_T                       => return 16#0074#;
            when Latin_Small_Letter_U                       => return 16#0075#;
            when Latin_Small_Letter_V                       => return 16#0076#;
            when Latin_Small_Letter_W                       => return 16#0077#;
            when Latin_Small_Letter_X                       => return 16#0078#;
            when Latin_Small_Letter_Y                       => return 16#0079#;
            when Latin_Small_Letter_Z                       => return 16#007A#;
            when Left_Curly_Bracket                         => return 16#007B#;
            when Vertical_Line                              => return 16#007C#;
            when Right_Curly_Bracket                        => return 16#007D#;
            when Tilde                                      => return 16#007E#;
            when Delete                                     => return 16#007F#;
            when Cyrillic_Capital_Letter_Dje                => return 16#0080#;
            when Cyrillic_Capital_Letter_Gje                => return 16#0081#;
            when Single_Low_9_Quotation_Mark                => return 16#0082#;
            when Cyrillic_Small_Letter_Gje                  => return 16#0083#;
            when Double_Low_9_Quotation_Mark                => return 16#0084#;
            when Horizontal_Ellipsis                        => return 16#0085#;
            when Dagger                                     => return 16#0086#;
            when Double_Dagger                              => return 16#0087#;
            when Euro_Sign                                  => return 16#0088#;
            when Per_Mille_Sign                             => return 16#0089#;
            when Cyrillic_Capital_Letter_Lje                => return 16#008A#;
            when Single_Left_Pointing_Angle_Quotation_Mark  => return 16#008B#;
            when Cyrillic_Capital_Letter_Nje                => return 16#008C#;
            when Cyrillic_Capital_Letter_Kje                => return 16#008D#;
            when Cyrillic_Capital_Letter_Tshe               => return 16#008E#;
            when Cyrillic_Capital_Letter_Dzhe               => return 16#008F#;
            when Cyrillic_Small_Letter_Dje                  => return 16#0090#;
            when Left_Single_Quotation_Mark                 => return 16#0091#;
            when Right_Single_Quotation_Mark                => return 16#0092#;
            when Left_Double_Quotation_Mark                 => return 16#0093#;
            when Right_Double_Quotation_Mark                => return 16#0094#;
            when Bullet                                     => return 16#0095#;
            when En_Dash                                    => return 16#0096#;
            when Em_Dash                                    => return 16#0097#;
            when Undefined                                  => return 16#0098#;
            when Trade_Mark_Sign                            => return 16#0099#;
            when Cyrillic_Small_Letter_Lje                  => return 16#009A#;
            when Single_Right_Pointing_Angle_Quotation_Mark => return 16#009B#;
            when Cyrillic_Small_Letter_Nje                  => return 16#009C#;
            when Cyrillic_Small_Letter_Kje                  => return 16#009D#;
            when Cyrillic_Small_Letter_Tshe                 => return 16#009E#;
            when Cyrillic_Small_Letter_Dzhe                 => return 16#009F#;
            when No_Break_Space                             => return 16#00A0#;
            when Cyrillic_Capital_Letter_Short_U            => return 16#00A1#;
            when Cyrillic_Small_Letter_Short_U              => return 16#00A2#;
            when Cyrillic_Capital_Letter_Je                 => return 16#00A3#;
            when Currency_Sign                              => return 16#00A4#;
            when Cyrillic_Capital_Letter_Ghe_With_Upturn    => return 16#00A5#;
            when Broken_Bar                                 => return 16#00A6#;
            when Section_Sign                               => return 16#00A7#;
            when Cyrillic_Capital_Letter_Io                 => return 16#00A8#;
            when Copyright_Sign                             => return 16#00A9#;
            when Cyrillic_Capital_Letter_Ukrainian_Ie       => return 16#00AA#;
            when Left_Pointing_Double_Angle_Quotation_Mark  => return 16#00AB#;
            when Not_Sign                                   => return 16#00AC#;
            when Soft_Hyphen                                => return 16#00AD#;
            when Registered_Sign                            => return 16#00AE#;
            when Cyrillic_Capital_Letter_Yi                 => return 16#00AF#;
            when Degree_Sign                                => return 16#00B0#;
            when Plus_Minus_Sign                            => return 16#00B1#;
            when Cyrillic_Capital_Letter_Byelorussian_Ukrainian_I =>
               return 16#00B2#;
            when Cyrillic_Small_Letter_Byelorussian_Ukrainian_I =>
               return 16#00B3#;
            when Cyrillic_Small_Letter_Ghe_With_Upturn      => return 16#00B4#;
            when Micro_Sign                                 => return 16#00B5#;
            when Pilcrow_Sign                               => return 16#00B6#;
            when Middle_Dot                                 => return 16#00B7#;
            when Cyrillic_Small_Letter_Io                   => return 16#00B8#;
            when Numero_Sign                                => return 16#00B9#;
            when Cyrillic_Small_Letter_Ukrainian_Ie         => return 16#00BA#;
            when Right_Pointing_Double_Angle_Quotation_Mark => return 16#00BB#;
            when Cyrillic_Small_Letter_Je                   => return 16#00BC#;
            when Cyrillic_Capital_Letter_Dze                => return 16#00BD#;
            when Cyrillic_Small_Letter_Dze                  => return 16#00BE#;
            when Cyrillic_Small_Letter_Yi                   => return 16#00BF#;
            when Cyrillic_Capital_Letter_A                  => return 16#00C0#;
            when Cyrillic_Capital_Letter_Be                 => return 16#00C1#;
            when Cyrillic_Capital_Letter_Ve                 => return 16#00C2#;
            when Cyrillic_Capital_Letter_Ghe                => return 16#00C3#;
            when Cyrillic_Capital_Letter_De                 => return 16#00C4#;
            when Cyrillic_Capital_Letter_Ie                 => return 16#00C5#;
            when Cyrillic_Capital_Letter_Zhe                => return 16#00C6#;
            when Cyrillic_Capital_Letter_Ze                 => return 16#00C7#;
            when Cyrillic_Capital_Letter_I                  => return 16#00C8#;
            when Cyrillic_Capital_Letter_Short_I            => return 16#00C9#;
            when Cyrillic_Capital_Letter_Ka                 => return 16#00CA#;
            when Cyrillic_Capital_Letter_El                 => return 16#00CB#;
            when Cyrillic_Capital_Letter_Em                 => return 16#00CC#;
            when Cyrillic_Capital_Letter_En                 => return 16#00CD#;
            when Cyrillic_Capital_Letter_O                  => return 16#00CE#;
            when Cyrillic_Capital_Letter_Pe                 => return 16#00CF#;
            when Cyrillic_Capital_Letter_Er                 => return 16#00D0#;
            when Cyrillic_Capital_Letter_Es                 => return 16#00D1#;
            when Cyrillic_Capital_Letter_Te                 => return 16#00D2#;
            when Cyrillic_Capital_Letter_U                  => return 16#00D3#;
            when Cyrillic_Capital_Letter_Ef                 => return 16#00D4#;
            when Cyrillic_Capital_Letter_Ha                 => return 16#00D5#;
            when Cyrillic_Capital_Letter_Tse                => return 16#00D6#;
            when Cyrillic_Capital_Letter_Che                => return 16#00D7#;
            when Cyrillic_Capital_Letter_Sha                => return 16#00D8#;
            when Cyrillic_Capital_Letter_Shcha              => return 16#00D9#;
            when Cyrillic_Capital_Letter_Hard_Sign          => return 16#00DA#;
            when Cyrillic_Capital_Letter_Yeru               => return 16#00DB#;
            when Cyrillic_Capital_Letter_Soft_Sign          => return 16#00DC#;
            when Cyrillic_Capital_Letter_E                  => return 16#00DD#;
            when Cyrillic_Capital_Letter_Yu                 => return 16#00DE#;
            when Cyrillic_Capital_Letter_Ya                 => return 16#00DF#;
            when Cyrillic_Small_Letter_A                    => return 16#00E0#;
            when Cyrillic_Small_Letter_Be                   => return 16#00E1#;
            when Cyrillic_Small_Letter_Ve                   => return 16#00E2#;
            when Cyrillic_Small_Letter_Ghe                  => return 16#00E3#;
            when Cyrillic_Small_Letter_De                   => return 16#00E4#;
            when Cyrillic_Small_Letter_Ie                   => return 16#00E5#;
            when Cyrillic_Small_Letter_Zhe                  => return 16#00E6#;
            when Cyrillic_Small_Letter_Ze                   => return 16#00E7#;
            when Cyrillic_Small_Letter_I                    => return 16#00E8#;
            when Cyrillic_Small_Letter_Short_I              => return 16#00E9#;
            when Cyrillic_Small_Letter_Ka                   => return 16#00EA#;
            when Cyrillic_Small_Letter_El                   => return 16#00EB#;
            when Cyrillic_Small_Letter_Em                   => return 16#00EC#;
            when Cyrillic_Small_Letter_En                   => return 16#00ED#;
            when Cyrillic_Small_Letter_O                    => return 16#00EE#;
            when Cyrillic_Small_Letter_Pe                   => return 16#00EF#;
            when Cyrillic_Small_Letter_Er                   => return 16#00F0#;
            when Cyrillic_Small_Letter_Es                   => return 16#00F1#;
            when Cyrillic_Small_Letter_Te                   => return 16#00F2#;
            when Cyrillic_Small_Letter_U                    => return 16#00F3#;
            when Cyrillic_Small_Letter_Ef                   => return 16#00F4#;
            when Cyrillic_Small_Letter_Ha                   => return 16#00F5#;
            when Cyrillic_Small_Letter_Tse                  => return 16#00F6#;
            when Cyrillic_Small_Letter_Che                  => return 16#00F7#;
            when Cyrillic_Small_Letter_Sha                  => return 16#00F8#;
            when Cyrillic_Small_Letter_Shcha                => return 16#00F9#;
            when Cyrillic_Small_Letter_Hard_Sign            => return 16#00FA#;
            when Cyrillic_Small_Letter_Yeru                 => return 16#00FB#;
            when Cyrillic_Small_Letter_Soft_Sign            => return 16#00FC#;
            when Cyrillic_Small_Letter_E                    => return 16#00FD#;
            when Cyrillic_Small_Letter_Yu                   => return 16#00FE#;
            when Cyrillic_Small_Letter_Ya                   => return 16#00FF#;
            when others =>
               Raise_Exception
                 (Invalid_Code'Identity,
                  "code " & Unicode_Char'Image (Char)
                  & " is not available in Windows-1251");
         end case;
      end if;
   end To_Windows_1251;

end Unicode.CCS.Windows_1251;
