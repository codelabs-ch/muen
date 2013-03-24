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

with Schema.Date_Time;         use Schema.Date_Time;
with Schema.Validators;        use Schema.Validators;
with Schema.Simple_Types;      use Schema.Simple_Types;
with GNAT.IO;                  use GNAT.IO;
with Sax.Readers;              use Sax.Readers;
with Sax.Utils;                use Sax.Utils;
with Sax.Symbols;              use Sax.Symbols;
with Unicode.CES;              use Unicode.CES;

procedure Test_Date_Time is

   type Local_Reader is new Abstract_Validation_Reader with null record;

   Reader : aliased Local_Reader;
   R      : constant Abstract_Validating_Reader_Access :=
     Reader'Unchecked_Access;

   generic
      type T is private;
      with procedure Value (Symbols : Symbol_Table;
                            Ch      : Byte_Sequence;
                            Val     : out T;
                            Error   : out Symbol) is <>;
   function Generic_Value (Ch : Byte_Sequence) return T;

   generic
      type T is private;
      with function Image (T1 : T) return String is <>;
      with function "<" (T1, T2 : T) return Boolean is <>;
      with function ">" (T1, T2 : T) return Boolean is <>;
   procedure Assert_NC_Generic (T1, T2 : T);

   generic
      type T is private;
      with function Image (T1 : T) return String is <>;
      with function "<" (T1, T2 : T) return Boolean is <>;
      with function ">" (T1, T2 : T) return Boolean is <>;
   procedure Assert_Generic (T1, T2 : T; Expected : Character);

   generic
      type T is private;
      with function Image (T1 : T) return String is <>;
   procedure Assert_Equal (T1, T2 : T);

   procedure Assert_Re (Re1, Re2 : String);
   --  Compare the conversion of Re1 to Re2

   -------------------
   -- Generic_Value --
   -------------------

   function Generic_Value (Ch : Byte_Sequence) return T is
      Val   : T;
      Error : Symbol;
   begin
      Value (Sax.Readers.Get_Symbol_Table (R.all), Ch, Val, Error);
      return Val;
   end Generic_Value;

   function Value is new Generic_Value (Time_T);
   function Value is new Generic_Value (Date_Time_T);
   function Value is new Generic_Value (Duration_T);
   function Value is new Generic_Value (GMonth_T);

   ------------------
   -- Assert_Equal --
   ------------------

   procedure Assert_Equal (T1, T2 : T) is
   begin
      if T1 /= T2 then
         Put_Line (Image (T1) & " /= " & Image (T2));
      end if;
   end Assert_Equal;

   ---------------
   -- Assert_Re --
   ---------------

   procedure Assert_Re (Re1, Re2 : String) is
   begin
      if Convert_Regexp (Re1) /= Re2 then
         Put_Line ("Converting " & Re1 & " gives "
                   & Convert_Regexp (Re1) & " and not " & Re2);
      end if;
   end Assert_Re;

   --------------------
   -- Assert_Generic --
   --------------------

   procedure Assert_Generic (T1, T2 : T; Expected : Character) is
   begin
      case Expected is
         when '<' =>
            if not (T1 < T2) then
               Put_Line (Image (T1) & " < " & Image (T2));
            end if;

            if not (T2 > T1) then
               Put_Line (Image (T2) & " > " & Image (T1));
            end if;

         when '>' =>
            if not (T1 > T2) then
               Put_Line (Image (T1) & " > " & Image (T2));
            end if;

            if not (T2 < T1) then
               Put_Line (Image (T2) & " < " & Image (T1));
            end if;

         when '=' =>
            if not (T1 = T2) then
               Put_Line (Image (T1) & " = " & Image (T2));
            end if;

         when others =>
            Put_Line ("Invalid expectation: " & Expected);
      end case;
   end Assert_Generic;

   -----------------------
   -- Assert_NC_Generic --
   -----------------------

   procedure Assert_NC_Generic (T1, T2 : T) is
      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      begin
         Tmp := T1 < T2;
         Put_Line (Image (T1) & " should not compare with " & Image (T2));
      exception
         when Not_Comparable => null;
      end;

      begin
         Tmp := T2 > T1;
         Put_Line (Image (T1) & " should not compare with " & Image (T2));
      exception
         when Not_Comparable => null;
      end;
   end Assert_NC_Generic;

   procedure Assert (Str1, Str2 : String);
   procedure Assert    is new Assert_Generic (Date_Time_T);
   procedure Assert    is new Assert_Generic (Duration_T);
   procedure Assert_NC is new Assert_NC_Generic (Date_Time_T);
   procedure Assert_NC is new Assert_NC_Generic (Duration_T);
   --  Test whether Str1 = Str2

   procedure Assert    is new Assert_Equal (Integer, Integer'Image);
   procedure Assert    is new Assert_Equal (Date_Time_T);
   procedure Assert    is new Assert_Equal (GMonth_T);

   ------------
   -- Assert --
   ------------

   procedure Assert (Str1, Str2 : String) is
   begin
      if Str1 /= Str2 then
         Put_Line (Str1 & " /= " & Str2);
      end if;
   end Assert;

   Time1_Image : constant String := "2000-01-12T12:13:14Z";
   Time2_Image : constant String := "12345-01-12T12:13:14";
   Time3_Image : constant String := "2002-01-12T12:13:14.123+05:02";
   Time4_Image : constant String := "2002-01-12T12:13:14.1234-05:02";

   Time5_Image : constant String := "2000-01-15T00:00:00";
   T5          : constant Date_Time_T := Value (Time5_Image);
   Time6_Image : constant String := "2000-02-15T00:00:00";
   T6          : constant Date_Time_T := Value (Time6_Image);

   Time7_Image : constant String := "2000-01-15T12:00:00";
   T7          : constant Date_Time_T := Value (Time7_Image);
   Time8_Image : constant String := "2000-01-16T12:00:00Z";
   T8          : constant Date_Time_T := Value (Time8_Image);

   Time9_Image  : constant String := "2000-01-01T12:00:00";
   T9           : constant Date_Time_T := Value (Time9_Image);
   Time10_Image : constant String := "1999-12-31T23:00:00Z";
   T10          : constant Date_Time_T := Value (Time10_Image);

   Time11_Image : constant String := "2000-01-16T12:00:00";
   T11          : constant Date_Time_T := Value (Time11_Image);
   Time12_Image : constant String := "2000-01-16T12:00:00Z";
   T12          : constant Date_Time_T := Value (Time12_Image);

   Time13_Image : constant String := "2000-01-16T00:00:00";
   T13          : constant Date_Time_T := Value (Time13_Image);
   Time14_Image : constant String := "2000-01-16T12:00:00Z";
   T14          : constant Date_Time_T := Value (Time14_Image);

   --  From E802-003
   T15_Image    : constant String := "2001-12-17T09:30:47-05:00";
   T15          : constant Date_Time_T := Value (T15_Image);

   T16_Image    : constant String := "03:58:02.5";
   T16          : constant Time_T := Value (T16_Image);

   Duration1_Image : constant String := "P1Y3M5DT7H10M3.3S";
   Duration2_Image : constant String := "-P1Y";
   Duration3_Image : constant String := "-PT1H";
   Duration4_Image : constant String := "-P1M";
   Duration5_Image : constant String := "-P1M1D";

   Dur1Y    : constant Duration_T := Value ("P1Y");
   Dur364D  : constant Duration_T := Value ("P364D");
   Dur365D  : constant Duration_T := Value ("P365D");
   Dur366D  : constant Duration_T := Value ("P366D");
   Dur367D  : constant Duration_T := Value ("P367D");

   Dur1M  : constant Duration_T := Value ("P1M");
   Dur27D : constant Duration_T := Value ("P27D");
   Dur28D : constant Duration_T := Value ("P28D");
   Dur29D : constant Duration_T := Value ("P29D");
   Dur30D : constant Duration_T := Value ("P30D");
   Dur31D : constant Duration_T := Value ("P31D");
   Dur32D : constant Duration_T := Value ("P32D");

   Dur5M   : constant Duration_T := Value ("P5M");
   Dur149D : constant Duration_T := Value ("P149D");
   Dur150D : constant Duration_T := Value ("P150D");
   Dur151D : constant Duration_T := Value ("P151D");
   Dur152D : constant Duration_T := Value ("P152D");
   Dur153D : constant Duration_T := Value ("P153D");
   Dur154D : constant Duration_T := Value ("P154D");

   Time1       : constant Date_Time_T := Value (Time1_Image);
   Time2       : constant Date_Time_T := Value (Time2_Image);
   Dur1        : constant Duration_T  := Value (Duration1_Image);

begin
   Assert (Time1_Image, Image (Time1));
   Assert (Time2_Image, Image (Time2));
   Assert (Time3_Image, Image (Date_Time_T'(Value (Time3_Image))));
   Assert (T15_Image, Image (T15));
   Assert (T16_Image, Image (T16));
   Assert ("2002-01-12T12:13:14.1234-05:02",
           Image (Date_Time_T'(Value (Time4_Image))));

   Assert ("P1Y3M5DT7H10M3.3S", Image (Dur1));
   Assert ("-P1Y", Image (Duration_T'(Value (Duration2_Image))));
   Assert ("-PT1H", Image (Duration_T'(Value (Duration3_Image))));
   Assert ("-P1M", Image (Duration_T'(Value (Duration4_Image))));
   Assert ("-P1M1D", Image (Duration_T'(Value (Duration5_Image))));

   Assert ("2001-04-17T19:23:17.3Z", Image (Time1 + Dur1));
   Assert ("12346-04-17T19:23:17.3", Image (Time2 + Dur1));

   --  Basic comparison tests
   Assert (Time1, Time2, '<');

   Assert (Time1 + Dur1Y, Date_Time_T'(Value ("2001-01-12T12:13:14Z")));
   Assert (Year (Time1), 2000);
   Assert (Month (Time1), 1);
   Assert (Day (Time1), 12);

   --  Examples from the XML Schema standard
   Assert    (T5, T6, '<');
   Assert    (T7, T8, '<');
   Assert_NC (T9, T10);
   Assert_NC (T11, T12);
   Assert_NC (T13, T14);

   --  Comparing durations
   Assert    (Dur1Y, Dur364D, '>');
   Assert_NC (Dur1Y, Dur365D);
   Assert_NC (Dur1Y, Dur366D);
   Assert    (Dur1Y, Dur367D, '<');

   Assert    (Dur1M, Dur27D, '>');
   Assert_NC (Dur1M, Dur28D);
   Assert_NC (Dur1M, Dur29D);
   Assert_NC (Dur1M, Dur30D);
   Assert_NC (Dur1M, Dur31D);
   Assert    (Dur1M, Dur32D, '<');

   Assert    (Dur5M, Dur149D, '>');
   Assert_NC (Dur5M, Dur150D);
   Assert_NC (Dur5M, Dur151D);
   Assert_NC (Dur5M, Dur152D);
   Assert_NC (Dur5M, Dur153D);
   Assert    (Dur5M, Dur154D, '<');

   --  Parsing GMonth
   --  The format that ends with '--' is actually incorrect, but was mentioned
   --  in earlier versions of the w3c schema specification, so should be
   --  accepted. See http://books.xmlschemata.org/relaxng/ch19-77111.html

   Assert (GMonth_T'(Value ("--01")), GMonth_T'(Value ("--01--")));
   Assert (GMonth_T'(Value ("--01Z")), GMonth_T'(Value ("--01")));
   Assert (GMonth_T'(Value ("--01+02:00")), GMonth_T'(Value ("--01+02:00")));
   Assert (GMonth_T'(Value ("--01-04:00")), GMonth_T'(Value ("--01-04:00")));

   --  Regexps

   Assert_Re ("[a-z]", "^([a-z])$");
   Assert_Re ("a[]b", "^(a[]b)$");
   Assert_Re ("[\c]$", "^[a-z:A-Z0-9._-]$");
   Assert_Re ("^[\i]", "^[A-Za-z:_]$");
   Assert_Re ("\c", "^([a-z:A-Z0-9._-])$");  --  ??? Not sure that's correct
   --  Assert_Re ("[abc-[b]]", "[ac]");  --  ??? Not supported for now
   Assert_Re ("^\d{5}$", "^\d{5}$");

end Test_Date_Time;
