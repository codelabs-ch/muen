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

with Unicode.Names.Basic_Latin;      use Unicode.Names.Basic_Latin;
--  with Unicode.Names.Latin_Extended_A; use Unicode.Names.Latin_Extended_A;

package body Unicode is

   type Unichar_Boolean_Array is
     array (Unicode_Char range 0 .. 2 ** 16 - 1) of Boolean;
   pragma Pack (Unichar_Boolean_Array);

   Valid_BaseChar : constant Unichar_Boolean_Array :=
     (16#0041# .. 16#005A# => True,
      16#0061# .. 16#007A# => True,
      16#00C0# .. 16#00D6# => True,
      16#00D8# .. 16#00F6# => True,
      16#00F8# .. 16#00FF# => True,
      16#0100# .. 16#0131# => True,
      16#0134# .. 16#013E# => True,
      16#0141# .. 16#0148# => True,
      16#014A# .. 16#017E# => True,
      16#0180# .. 16#01C3# => True,
      16#01CD# .. 16#01F0# => True,
      16#01F4# .. 16#01F5# => True,
      16#01FA# .. 16#0217# => True,
      16#0250# .. 16#02A8# => True,
      16#02BB# .. 16#02C1# => True,
      16#0386#             => True,
      16#0388# .. 16#038A# => True,
      16#038C#             => True,
      16#038E# .. 16#03A1# => True,
      16#03A3# .. 16#03CE# => True,
      16#03D0# .. 16#03D6# => True,
      16#03DA#             => True,
      16#03DC#             => True,
      16#03DE#             => True,
      16#03E0#             => True,
      16#03E2# .. 16#03F3# => True,
      16#0401# .. 16#040C# => True,
      16#040E# .. 16#044F# => True,
      16#0451# .. 16#045C# => True,
      16#045E# .. 16#0481# => True,
      16#0490# .. 16#04C4# => True,
      16#04C7# .. 16#04C8# => True,
      16#04CB# .. 16#04CC# => True,
      16#04D0# .. 16#04EB# => True,
      16#04EE# .. 16#04F5# => True,
      16#04F8# .. 16#04F9# => True,
      16#0531# .. 16#0556# => True,
      16#0559#             => True,
      16#0561# .. 16#0586# => True,
      16#05D0# .. 16#05EA# => True,
      16#05F0# .. 16#05F2# => True,
      16#0621# .. 16#063A# => True,
      16#0641# .. 16#064A# => True,
      16#0671# .. 16#06B7# => True,
      16#06BA# .. 16#06BE# => True,
      16#06C0# .. 16#06CE# => True,
      16#06D0# .. 16#06D3# => True,
      16#06D5#             => True,
      16#06E5# .. 16#06E6# => True,
      16#0905# .. 16#0939# => True,
      16#093D#             => True,
      16#0958# .. 16#0961# => True,
      16#0985# .. 16#098C# => True,
      16#098F# .. 16#0990# => True,
      16#0993# .. 16#09A8# => True,
      16#09AA# .. 16#09B0# => True,
      16#09B2#             => True,
      16#09B6# .. 16#09B9# => True,
      16#09DC# .. 16#09DD# => True,
      16#09DF# .. 16#09E1# => True,
      16#09F0# .. 16#09F1# => True,
      16#0A05# .. 16#0A0A# => True,
      16#0A0F# .. 16#0A10# => True,
      16#0A13# .. 16#0A28# => True,
      16#0A2A# .. 16#0A30# => True,
      16#0A32# .. 16#0A33# => True,
      16#0A35# .. 16#0A36# => True,
      16#0A38# .. 16#0A39# => True,
      16#0A59# .. 16#0A5C# => True,
      16#0A5E#             => True,
      16#0A72# .. 16#0A74# => True,
      16#0A85# .. 16#0A8B# => True,
      16#0A8D#             => True,
      16#0A8F# .. 16#0A91# => True,
      16#0A93# .. 16#0AA8# => True,
      16#0AAA# .. 16#0AB0# => True,
      16#0AB2# .. 16#0AB3# => True,
      16#0AB5# .. 16#0AB9# => True,
      16#0ABD#             => True,
      16#0AE0#             => True,
      16#0B05# .. 16#0B0C# => True,
      16#0B0F# .. 16#0B10# => True,
      16#0B13# .. 16#0B28# => True,
      16#0B2A# .. 16#0B30# => True,
      16#0B32# .. 16#0B33# => True,
      16#0B36# .. 16#0B39# => True,
      16#0B3D#             => True,
      16#0B5C# .. 16#0B5D# => True,
      16#0B5F# .. 16#0B61# => True,
      16#0B85# .. 16#0B8A# => True,
      16#0B8E# .. 16#0B90# => True,
      16#0B92# .. 16#0B95# => True,
      16#0B99# .. 16#0B9A# => True,
      16#0B9C#             => True,
      16#0B9E# .. 16#0B9F# => True,
      16#0BA3# .. 16#0BA4# => True,
      16#0BA8# .. 16#0BAA# => True,
      16#0BAE# .. 16#0BB5# => True,
      16#0BB7# .. 16#0BB9# => True,
      16#0C05# .. 16#0C0C# => True,
      16#0C0E# .. 16#0C10# => True,
      16#0C12# .. 16#0C28# => True,
      16#0C2A# .. 16#0C33# => True,
      16#0C35# .. 16#0C39# => True,
      16#0C60# .. 16#0C61# => True,
      16#0C85# .. 16#0C8C# => True,
      16#0C8E# .. 16#0C90# => True,
      16#0C92# .. 16#0CA8# => True,
      16#0CAA# .. 16#0CB3# => True,
      16#0CB5# .. 16#0CB9# => True,
      16#0CDE#             => True,
      16#0CE0# .. 16#0CE1# => True,
      16#0D05# .. 16#0D0C# => True,
      16#0D0E# .. 16#0D10# => True,
      16#0D12# .. 16#0D28# => True,
      16#0D2A# .. 16#0D39# => True,
      16#0D60# .. 16#0D61# => True,
      16#0E01# .. 16#0E2E# => True,
      16#0E30#             => True,
      16#0E32# .. 16#0E33# => True,
      16#0E40# .. 16#0E45# => True,
      16#0E81# .. 16#0E82# => True,
      16#0E84#             => True,
      16#0E87# .. 16#0E88# => True,
      16#0E8A#             => True,
      16#0E8D#             => True,
      16#0E94# .. 16#0E97# => True,
      16#0E99# .. 16#0E9F# => True,
      16#0EA1# .. 16#0EA3# => True,
      16#0EA5#             => True,
      16#0EA7#             => True,
      16#0EAA# .. 16#0EAB# => True,
      16#0EAD# .. 16#0EAE# => True,
      16#0EB0#             => True,
      16#0EB2# .. 16#0EB3# => True,
      16#0EBD#             => True,
      16#0EC0# .. 16#0EC4# => True,
      16#0F40# .. 16#0F47# => True,
      16#0F49# .. 16#0F69# => True,
      16#10A0# .. 16#10C5# => True,
      16#10D0# .. 16#10F6# => True,
      16#1100#             => True,
      16#1102# .. 16#1103# => True,
      16#1105# .. 16#1107# => True,
      16#1109#             => True,
      16#110B# .. 16#110C# => True,
      16#110E# .. 16#1112# => True,
      16#113C#             => True,
      16#113E#             => True,
      16#1140#             => True,
      16#114C#             => True,
      16#114E#             => True,
      16#1150#             => True,
      16#1154# .. 16#1155# => True,
      16#1159#             => True,
      16#115F# .. 16#1161# => True,
      16#1163#             => True,
      16#1165#             => True,
      16#1167#             => True,
      16#1169#             => True,
      16#116D# .. 16#116E# => True,
      16#1172# .. 16#1173# => True,
      16#1175#             => True,
      16#119E#             => True,
      16#11A8#             => True,
      16#11AB#             => True,
      16#11AE# .. 16#11AF# => True,
      16#11B7# .. 16#11B8# => True,
      16#11BA#             => True,
      16#11BC# .. 16#11C2# => True,
      16#11EB#             => True,
      16#11F0#             => True,
      16#11F9#             => True,
      16#1E00# .. 16#1E9B# => True,
      16#1EA0# .. 16#1EF9# => True,
      16#1F00# .. 16#1F15# => True,
      16#1F18# .. 16#1F1D# => True,
      16#1F20# .. 16#1F45# => True,
      16#1F48# .. 16#1F4D# => True,
      16#1F50# .. 16#1F57# => True,
      16#1F59#             => True,
      16#1F5B#             => True,
      16#1F5D#             => True,
      16#1F5F# .. 16#1F7D# => True,
      16#1F80# .. 16#1FB4# => True,
      16#1FB6# .. 16#1FBC# => True,
      16#1FBE#             => True,
      16#1FC2# .. 16#1FC4# => True,
      16#1FC6# .. 16#1FCC# => True,
      16#1FD0# .. 16#1FD3# => True,
      16#1FD6# .. 16#1FDB# => True,
      16#1FE0# .. 16#1FEC# => True,
      16#1FF2# .. 16#1FF4# => True,
      16#1FF6# .. 16#1FFC# => True,
      16#2126#             => True,
      16#212A# .. 16#212B# => True,
      16#212E#             => True,
      16#2180# .. 16#2182# => True,
      16#3041# .. 16#3094# => True,
      16#30A1# .. 16#30FA# => True,
      16#3105# .. 16#312C# => True,
      16#AC00# .. 16#D7A3# => True,
      others => False);
   --  The definition of BaseChar, in annex B of XML specifications

   Valid_DigitChar : constant Unichar_Boolean_Array :=
     (16#0030# .. 16#0039# => True,
      16#0660# .. 16#0669# => True,
      16#06F0# .. 16#06F9# => True,
      16#0966# .. 16#096F# => True,
      16#09E6# .. 16#09EF# => True,
      16#0A66# .. 16#0A6F# => True,
      16#0AE6# .. 16#0AEF# => True,
      16#0B66# .. 16#0B6F# => True,
      16#0BE7# .. 16#0BEF# => True,
      16#0C66# .. 16#0C6F# => True,
      16#0CE6# .. 16#0CEF# => True,
      16#0D66# .. 16#0D6F# => True,
      16#0E50# .. 16#0E59# => True,
      16#0ED0# .. 16#0ED9# => True,
      16#0F20# .. 16#0F29# => True,
      others => False);
   --  The definition of Digit, in annex B of XML specifications

   Valid_CombiningChar : constant Unichar_Boolean_Array :=
     (16#0300# .. 16#0345# => True,
      16#0360# .. 16#0361# => True,
      16#0483# .. 16#0486# => True,
      16#0591# .. 16#05A1# => True,
      16#05A3# .. 16#05B9# => True,
      16#05BB# .. 16#05BD# => True,
      16#05BF#             => True,
      16#05C1# .. 16#05C2# => True,
      16#05C4#             => True,
      16#064B# .. 16#0652# => True,
      16#0670#             => True,
      16#06D6# .. 16#06DC# => True,
      16#06DD# .. 16#06DF# => True,
      16#06E0# .. 16#06E4# => True,
      16#06E7# .. 16#06E8# => True,
      16#06EA# .. 16#06ED# => True,
      16#0901# .. 16#0903# => True,
      16#093C#             => True,
      16#093E# .. 16#094C# => True,
      16#094D#             => True,
      16#0951# .. 16#0954# => True,
      16#0962# .. 16#0963# => True,
      16#0981# .. 16#0983# => True,
      16#09BC#             => True,
      16#09BE#             => True,
      16#09BF#             => True,
      16#09C0# .. 16#09C4# => True,
      16#09C7# .. 16#09C8# => True,
      16#09CB# .. 16#09CD# => True,
      16#09D7#             => True,
      16#09E2# .. 16#09E3# => True,
      16#0A02#             => True,
      16#0A3C#             => True,
      16#0A3E#             => True,
      16#0A3F#             => True,
      16#0A40# .. 16#0A42# => True,
      16#0A47# .. 16#0A48# => True,
      16#0A4B# .. 16#0A4D# => True,
      16#0A70# .. 16#0A71# => True,
      16#0A81# .. 16#0A83# => True,
      16#0ABC#             => True,
      16#0ABE# .. 16#0AC5# => True,
      16#0AC7# .. 16#0AC9# => True,
      16#0ACB# .. 16#0ACD# => True,
      16#0B01# .. 16#0B03# => True,
      16#0B3C#             => True,
      16#0B3E# .. 16#0B43# => True,
      16#0B47# .. 16#0B48# => True,
      16#0B4B# .. 16#0B4D# => True,
      16#0B56# .. 16#0B57# => True,
      16#0B82# .. 16#0B83# => True,
      16#0BBE# .. 16#0BC2# => True,
      16#0BC6# .. 16#0BC8# => True,
      16#0BCA# .. 16#0BCD# => True,
      16#0BD7#             => True,
      16#0C01# .. 16#0C03# => True,
      16#0C3E# .. 16#0C44# => True,
      16#0C46# .. 16#0C48# => True,
      16#0C4A# .. 16#0C4D# => True,
      16#0C55# .. 16#0C56# => True,
      16#0C82# .. 16#0C83# => True,
      16#0CBE# .. 16#0CC4# => True,
      16#0CC6# .. 16#0CC8# => True,
      16#0CCA# .. 16#0CCD# => True,
      16#0CD5# .. 16#0CD6# => True,
      16#0D02# .. 16#0D03# => True,
      16#0D3E# .. 16#0D43# => True,
      16#0D46# .. 16#0D48# => True,
      16#0D4A# .. 16#0D4D# => True,
      16#0D57#             => True,
      16#0E31#             => True,
      16#0E34# .. 16#0E3A# => True,
      16#0E47# .. 16#0E4E# => True,
      16#0EB1#             => True,
      16#0EB4# .. 16#0EB9# => True,
      16#0EBB# .. 16#0EBC# => True,
      16#0EC8# .. 16#0ECD# => True,
      16#0F18# .. 16#0F19# => True,
      16#0F35#             => True,
      16#0F37#             => True,
      16#0F39#             => True,
      16#0F3E#             => True,
      16#0F3F#             => True,
      16#0F71# .. 16#0F84# => True,
      16#0F86# .. 16#0F8B# => True,
      16#0F90# .. 16#0F95# => True,
      16#0F97#             => True,
      16#0F99# .. 16#0FAD# => True,
      16#0FB1# .. 16#0FB7# => True,
      16#0FB9#             => True,
      16#20D0# .. 16#20DC# => True,
      16#20E1#             => True,
      16#302A# .. 16#302F# => True,
      16#3099#             => True,
      16#309A#             => True,
      others               => False);
   --  The definition of CombiningChar in the annex B of the XML specifications

   Valid_ExtenderChar : constant Unichar_Boolean_Array :=
     (16#00B7#             => True,
      16#02D0#             => True,
      16#02D1#             => True,
      16#0387#             => True,
      16#0640#             => True,
      16#0E46#             => True,
      16#0EC6#             => True,
      16#3005#             => True,
      16#3031# .. 16#3035# => True,
      16#309D# .. 16#309E# => True,
      16#30FC# .. 16#30FE# => True,
      others               => False);
   --  The definition of ExtenderChar in the annex B of the XML specifications

   --------------------
   -- Is_White_Space --
   --------------------

   function Is_White_Space (Char : Unicode_Char) return Boolean is
   begin
      return    Char = Space                    --  16#20#
        or else Char = Horizontal_Tabulation    --  16#09#
        or else Char = Line_Feed                --  16#0A#
        or else Char = Carriage_Return;         --  16#0D#
   end Is_White_Space;

   --------------------
   -- Is_Ideographic --
   --------------------

   function Is_Ideographic (Char : Unicode_Char) return Boolean is
   begin
      return Char in 16#4E00# .. 16#9FA5#
        or else Char = 16#3007#
        or else Char in 16#3021# .. 16#3029#;
   end Is_Ideographic;

   ---------------
   -- Is_Letter --
   ---------------

   function Is_Letter (Char : Unicode_Char) return Boolean is
   begin
      return Is_Base_Char (Char) or else Is_Ideographic (Char);
   end Is_Letter;

   ------------------
   -- Is_Base_Char --
   ------------------

   function Is_Base_Char (Char : Unicode_Char) return Boolean is
      pragma Suppress (All_Checks);
   begin
      return Char < 2 ** 16 and then Valid_BaseChar (Char);
   end Is_Base_Char;

   --------------
   -- Is_Digit --
   --------------

   function Is_Digit (Char : Unicode_Char) return Boolean is
      pragma Suppress (All_Checks);
   begin
      return Char < 2 ** 16 and then Valid_DigitChar (Char);
   end Is_Digit;

   -----------------------
   -- Is_Combining_Char --
   -----------------------

   function Is_Combining_Char (Char : Unicode_Char) return Boolean is
      pragma Suppress (All_Checks);
   begin
      return Char < 2 ** 16 and then Valid_CombiningChar (Char);
   end Is_Combining_Char;

   -----------------
   -- Is_Extender --
   -----------------

   function Is_Extender (Char : Unicode_Char) return Boolean is
   begin
      return Char < 2 ** 16 and then Valid_ExtenderChar (Char);
   end Is_Extender;

   ----------------
   -- To_Unicode --
   ----------------

   function To_Unicode (C : Character) return Unicode_Char is
   begin
      return Character'Pos (C);
   end To_Unicode;
end Unicode;
