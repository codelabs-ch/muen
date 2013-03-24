------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

pragma Ada_05;

with Ada.Unchecked_Deallocation;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;
with Sax.Encodings;             use Sax.Encodings;
with Unicode;                   use Unicode;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;

package body Schema.Simple_Types is

   use Simple_Type_Tables, Enumeration_Tables;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher_Array, Pattern_Matcher_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Pattern_Matcher, Pattern_Matcher_Access);

   generic
      with function Get_Length (Ch : Byte_Sequence) return Natural;
   function Validate_Length_Facets
     (Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      Mask          : Facets_Mask;
      Length, Min_Length, Max_Length : Integer) return Symbol;
   --  Validate length facets

   generic
      type T is private;
      with procedure Value (Symbols : Symbol_Table;
                            Ch      : Byte_Sequence;
                            Val     : out T;
                            Error   : out Symbol) is <>;
      with function Image (Val : T) return Byte_Sequence is <>;
      with function "<" (T1, T2 : T) return Boolean is <>;
      with function "<=" (T1, T2 : T) return Boolean is <>;
   procedure Validate_Range
     (Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      Mask          : Facets_Mask;
      Min_Inclusive : T;
      Min_Exclusive : T;
      Max_Inclusive : T;
      Max_Exclusive : T;
      Error         : out Symbol;
      Val           : out T);

   generic
      type T is private;
      with procedure Value (Symbols : Symbol_Table;
                            Ch      : Byte_Sequence;
                            Val     : out T;
                            Error   : out Symbol) is <>;
   procedure Override_Single_Range_Facet
     (Symbols       : Sax.Utils.Symbol_Table;
      Facets        : All_Facets;
      Facet         : Facet_Enum;
      Mask          : in out Facets_Mask;
      Val           : in out T;
      Error         : in out Symbol;
      Error_Loc     : in out Location);

   generic
      type T is private;
      with procedure Value (Symbols : Symbol_Table;
                            Ch      : Byte_Sequence;
                            Val     : out T;
                            Error   : out Symbol) is <>;
   procedure Override_Range_Facets
     (Symbols       : Sax.Utils.Symbol_Table;
      Facets        : All_Facets;
      Mask          : in out Facets_Mask;
      Min_Inclusive : in out T;
      Min_Exclusive : in out T;
      Max_Inclusive : in out T;
      Max_Exclusive : in out T;
      Error         : out Symbol;
      Error_Loc     : out Location);
   --  Override some range facets

   procedure Override_Length_Facets
     (Symbols       : Sax.Utils.Symbol_Table;
      Facets        : All_Facets;
      Mask          : in out Facets_Mask;
      Length        : in out Integer;
      Min_Length    : in out Integer;
      Max_Length    : in out Integer;
      Error         : out Symbol;
      Error_Loc     : out Location);
   --  Override some length facets

   generic
      type T is private;
      with procedure Value (Symbols : Symbol_Table;
                            Ch      : Byte_Sequence;
                            Val     : out T;
                            Error   : out Symbol) is <>;
      with function "=" (T1, T2 : T) return Boolean is <>;
      with function Image (T1 : T) return String is <>;
   function Generic_Equal
     (Symbols : Symbol_Table;
      Val1    : Symbol;
      Val2    : Byte_Sequence) return Boolean;
   --  Compare two values, after possibly normalizing them given the type
   --  definition (whitespaces, remove left-most 0 when appropriate,...).
   --  This assumes [Val1] and [Val2] are valid values for the type (ie they
   --  have already been validated).

   function Validate_List_Facets
     (Descr   : Simple_Type_Descr;
      Symbols : Sax.Utils.Symbol_Table;
      Ch      : Unicode.CES.Byte_Sequence;
      Length, Min_Length, Max_Length : Integer) return Symbol;
   --  Validate the facets for a list

   procedure Check_Id
     (Symbols   : Symbol_Table;
      Id_Table  : in out Symbol_Htable_Access;
      Value     : Unicode.CES.Byte_Sequence;
      Error     : in out Symbol);
   --  Check whether Value is a unique ID in the document.
   --  If yes, store it in Id_Table to ensure its future uniqueness.
   --  Return the error message or [No_Symbol]

   -------------------
   -- Generic_Equal --
   -------------------

   function Generic_Equal
     (Symbols : Symbol_Table;
      Val1    : Symbol;
      Val2    : Byte_Sequence) return Boolean
   is
      V1, V2 : T;
      Error  : Symbol;
   begin
      Value (Symbols, Get (Val1).all, V1, Error);
      if Error /= No_Symbol then
         if Debug then
            Debug_Output ("Generic_Equal, could not convert Val1 "
                          & Get (Val1).all & " => " & Get (Error).all);
         end if;
         return False;
      end if;

      Value (Symbols, Val2, V2, Error);
      if Error /= No_Symbol then
         if Debug then
            Debug_Output ("Generic_Equal, could not convert Val2 "
                          & Val2 & " => " & Get (Error).all);
         end if;
         return False;
      end if;

      if Debug then
         Debug_Output ("Comparing " & Image (V1) & " != " & Image (V2));
      end if;

      return V1 = V2;
   end Generic_Equal;

   ----------------------------
   -- Validate_Length_Facets --
   ----------------------------

   function Validate_Length_Facets
     (Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      Mask          : Facets_Mask;
      Length, Min_Length, Max_Length : Integer) return Symbol
   is
      L : Integer := -1;
   begin
      --  Characters are always a string, nothing to check here but the facets

      if Mask (Facet_Length)
        or else Mask (Facet_Min_Length)
        or else Mask (Facet_Max_Length)
      then
         L := Get_Length (Ch);
      else
         return No_Symbol;
      end if;

      if Mask (Facet_Length) then
         if L /= Length then
            return Find
              (Symbols,
               "Invalid length, must be"
               & Integer'Image (Length) & " characters");
         end if;
      end if;

      if Mask (Facet_Min_Length) then
         if L < Min_Length then
            return Find
              (Symbols,
               "String is too short, minimum length is"
               & Integer'Image (Min_Length)
               & " characters");
         end if;
      end if;

      if Mask (Facet_Max_Length) then
         if L > Max_Length then
            return Find
              (Symbols,
               "String is too long, maximum length is"
               & Integer'Image (Max_Length)
               & " characters");
         end if;
      end if;

      return No_Symbol;
   end Validate_Length_Facets;

   ---------------------
   --  Instantiations --
   ---------------------

   function HexBinary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural;
   function Base64Binary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural;
   --  Return the length of a string

   procedure Value (Symbols : Symbol_Table;
                    Ch      : Byte_Sequence;
                    Val     : out XML_Float;
                    Error   : out Symbol);
   --  Converts [Ch] into [Val]

   function Validate_String
     (Descr   : Simple_Type_Descr;
      Symbols : Sax.Utils.Symbol_Table;
      Ch      : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_HexBinary_Facets is new
     Validate_Length_Facets (HexBinary_Get_Length);
   function Validate_Base64Binary_Facets is new
     Validate_Length_Facets (Base64Binary_Get_Length);

   function Validate_NMTOKEN
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol;
   function Validate_NMTOKENS
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol;
   function Validate_Name
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol;
   function Validate_NCName
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol;
   function Validate_NCNames
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol;
   function Validate_Language
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_URI
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_HexBinary
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Notation
     (Notations     : Symbol_Htable.HTable;
      Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Base64Binary
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_QName
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol;
   function Validate_Boolean
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Double
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Decimal
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Duration
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Date_Time
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Date
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_Time
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_GDay
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_GMonth_Day
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_GMonth
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_GYear
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   function Validate_GYear_Month
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol;
   --  Check [Ch] for one of the primitive types, including facets

   function Anchor (Str : String) return String;
   --  Return an anchored version of Str ("^...$").
   --  In XML, regexps are always anchored, as per the beginning of [G]

   function Missing_End_Anchor (Str : String) return Boolean;
   function Missing_Start_Anchor (Str : String) return Boolean;
   --  Whether the regexp is missing the "^" or "$" anchors

   procedure Boolean_Value
     (Symbols : Symbol_Table;
      Ch      : Byte_Sequence;
      Val     : out Boolean;
      Error   : out Symbol);
   --  Converts [Ch] to a boolean, and returns an error message if needed

   function Equal_Boolean     is new Generic_Equal (Boolean, Boolean_Value,
                                                    Image => Boolean'Image);
   function Equal_Float       is new Generic_Equal (XML_Float, Value);
   function Equal_Decimal    is new Generic_Equal (Arbitrary_Precision_Number);
   function Equal_Duration    is new Generic_Equal (Duration_T);
   function Equal_Date_Time   is new Generic_Equal (Date_Time_T);
   function Equal_Date        is new Generic_Equal (Date_T);
   function Equal_Time        is new Generic_Equal (Time_T);
   function Equal_GDay        is new Generic_Equal (GDay_T);
   function Equal_GMonth_Day  is new Generic_Equal (GMonth_Day_T);
   function Equal_GMonth      is new Generic_Equal (GMonth_T);
   function Equal_GYear       is new Generic_Equal (GYear_T);
   function Equal_GYear_Month is new Generic_Equal (GYear_Month_T);

   -------------------------------
   -- Register_Predefined_Types --
   -------------------------------

   procedure Register_Predefined_Types (Symbols : Sax.Utils.Symbol_Table) is
      Zero : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "0"));
      One  : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "1"));
      Minus_One  : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "-1"));
      Max_Unsigned_Long : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+18446744073709551615"));
      Max_Long : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+9223372036854775807"));
      Min_Long : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "-9223372036854775808"));
      Max_Int : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+2147483647"));
      Min_Int : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "-2147483648"));
      Max_Short : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+32767"));
      Min_Short : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "-32768"));
      Max_Byte : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+127"));
      Min_Byte : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "-128"));
      Max_Unsigned_Int : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+4294967295"));
      Max_Unsigned_Short : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+65535"));
      Max_Unsigned_Byte : constant Arbitrary_Precision_Number :=
        Value (Find (Symbols, "+255"));

      Whitespace_Mask : constant Facets_Mask := (Facet_Whitespace => True,
                                                 others           => False);
      Fraction_Min_Mask : constant Facets_Mask :=
        (Facet_Fraction_Digits => True,
         Facet_Min_Inclusive   => True,
         others                => False);
      Fraction_Max_Mask : constant Facets_Mask :=
        (Facet_Fraction_Digits => True,
         Facet_Max_Inclusive   => True,
         others                => False);
      Fraction_Min_Max_Mask : constant Facets_Mask :=
        (Facet_Fraction_Digits => True,
         Facet_Min_Inclusive   => True,
         Facet_Max_Inclusive   => True,
         others                => False);

      Any_Simple_Type, Decimal, Integer, Long, Int, Short : Type_Index;
      Non_Positive_Int, Non_Negative_Int : Type_Index;
      Unsigned_Long, Unsigned_Int, Unsigned_Short : Type_Index;
      Str, Normalized_Str, Token : Type_Index;
      Name, NCName : Type_Index;
      T : Type_Index;
      pragma Unreferenced (T);
   begin
      Any_Simple_Type := Register
        ("anySimpleType",
         (Kind       => Primitive_String,
          Mask       => (Facet_Whitespace => True,
                         others           => False),
          Whitespace => Preserve,
          others     => <>), No_Type_Index);

      Str := Register ("string", (Kind       => Primitive_String,
                                  Mask       => Whitespace_Mask,
                                  Whitespace => Preserve,
                                  others     => <>), Any_Simple_Type);
      Normalized_Str :=
        Register ("normalizedString", (Kind       => Primitive_String,
                                       Mask       => Whitespace_Mask,
                                       Whitespace => Replace,
                                       others => <>), Str);
      Token := Register ("token", (Kind       => Primitive_String,
                                   Mask       => Whitespace_Mask,
                                   Whitespace => Collapse,
                                   others => <>), Normalized_Str);
      T := Register ("language", (Kind       => Primitive_Language,
                             Mask       => Whitespace_Mask,
                             Whitespace => Collapse,
                             others => <>), Token);
      T := Register ("boolean",  (Kind => Primitive_Boolean, others => <>),
                     Any_Simple_Type);
      T := Register ("QName", (Kind => Primitive_QName,
                               Mask => Whitespace_Mask,
                               others => <>),
                     Any_Simple_Type);
      T := Register ("NOTATION", (Kind => Primitive_Notation, others => <>),
                     Any_Simple_Type);
      T := Register ("float",    (Kind => Primitive_Float, others => <>),
                     Any_Simple_Type);
      T := Register ("NMTOKEN",  (Kind => Primitive_NMTOKEN, others => <>),
                     Token);
      T := Register ("NMTOKENS", (Kind => Primitive_NMTOKENS,
                                  Mask => Whitespace_Mask,
                                  others => <>),
        Any_Simple_Type);
      Name := Register ("Name",     (Kind       => Primitive_Name,
                                     Mask       => Whitespace_Mask,
                                     Whitespace => Collapse,
                                     others => <>), Token);
      NCName := Register ("NCName",   (Kind       => Primitive_NCName,
                                       Mask       => Whitespace_Mask,
                                       Whitespace => Collapse,
                                       others => <>), Name);
      T := Register ("ID",       (Kind       => Primitive_ID,
                                  Mask       => Whitespace_Mask,
                                  Whitespace => Preserve,
                                  others => <>), NCName);
      T := Register ("IDREF",    (Kind       => Primitive_NCName,
                                  Mask       => Whitespace_Mask,
                                  Whitespace => Preserve,
                                  others => <>), NCName);
      T := Register ("IDREFS",   (Kind       => Primitive_NCNames,
                                  Mask       => Whitespace_Mask,
                                  Whitespace => Preserve,
                                  others => <>), Any_Simple_Type);
      T := Register ("ENTITY",   (Kind       => Primitive_NCName,
                                  Mask       => Whitespace_Mask,
                                  Whitespace => Preserve,
                                  others => <>), NCName);
      T := Register ("ENTITIES", (Kind       => Primitive_NCNames,
                                  Mask       => Whitespace_Mask,
                                  Whitespace => Preserve,
                                  others => <>), Any_Simple_Type);
      T := Register ("anyURI",   (Kind => Primitive_Any_URI,
                                  Mask => Whitespace_Mask,
                                  others => <>), Any_Simple_Type);
      T := Register ("hexBinary", (Kind => Primitive_HexBinary,
                                   others => <>), Any_Simple_Type);
      T := Register ("base64Binary", (Kind => Primitive_Base64Binary,
                                      others => <>), Any_Simple_Type);
      Decimal := Register ("decimal",
                           (Kind => Primitive_Decimal, others => <>),
                           Any_Simple_Type);
      Integer := Register ("integer",
                           (Kind                  => Primitive_Decimal,
                            Fraction_Digits       => 0,
                            Mask => (Facet_Fraction_Digits => True,
                                     others                => False),
                            others => <>), Decimal);
      Non_Negative_Int :=
        Register ("nonNegativeInteger",
                  (Kind                  => Primitive_Decimal,
                   Fraction_Digits       => 0,
                   Decimal_Min_Inclusive => Zero,
                   Mask                  => Fraction_Min_Mask,
                   others                => <>), Integer);
      Unsigned_Long :=
        Register ("unsignedLong",
                  (Kind                 => Primitive_Decimal,
                   Mask                 => Fraction_Min_Max_Mask,
                   Fraction_Digits       => 0,
                   Decimal_Min_Inclusive => Zero,
                   Decimal_Max_Inclusive => Max_Unsigned_Long,
                   others                => <>), Non_Negative_Int);
      T := Register ("positiveInteger",
                     (Kind                  => Primitive_Decimal,
                      Fraction_Digits       => 0,
                      Decimal_Min_Inclusive => One,
                      Mask                  => Fraction_Min_Mask,
                      others                => <>), Non_Negative_Int);
      Non_Positive_Int :=
        Register ("nonPositiveInteger",
                  (Kind                  => Primitive_Decimal,
                   Fraction_Digits       => 0,
                   Decimal_Max_Inclusive => Zero,
                   Mask                  => Fraction_Max_Mask,
                   others                => <>), Integer);
      T := Register ("negativeInteger",
                     (Kind                  => Primitive_Decimal,
                      Fraction_Digits       => 0,
                      Decimal_Max_Inclusive => Minus_One,
                      Mask                  => Fraction_Max_Mask,
                      others                => <>), Non_Positive_Int);
      Long := Register ("long",
                        (Kind               => Primitive_Decimal,
                         Mask               => Fraction_Min_Max_Mask,
                         Fraction_Digits       => 0,
                         Decimal_Max_Inclusive => Max_Long,
                         Decimal_Min_Inclusive => Min_Long,
                         others                => <>), Integer);
      Int := Register ("int",
                       (Kind                  => Primitive_Decimal,
                        Mask                  => Fraction_Min_Max_Mask,
                        Fraction_Digits       => 0,
                        Decimal_Max_Inclusive => Max_Int,
                        Decimal_Min_Inclusive => Min_Int,
                        others                => <>), Long);
      Short := Register ("short",
                         (Kind                  => Primitive_Decimal,
                          Mask                  => Fraction_Min_Max_Mask,
                          Fraction_Digits       => 0,
                          Decimal_Max_Inclusive => Max_Short,
                          Decimal_Min_Inclusive => Min_Short,
                          others                => <>), Int);
      T := Register ("byte",
                     (Kind                  => Primitive_Decimal,
                      Mask                  => Fraction_Min_Max_Mask,
                      Fraction_Digits       => 0,
                      Decimal_Max_Inclusive => Max_Byte,
                      Decimal_Min_Inclusive => Min_Byte,
                      others                => <>), Short);
      Unsigned_Int :=
        Register ("unsignedInt",
                  (Kind                  => Primitive_Decimal,
                   Mask                  => Fraction_Min_Max_Mask,
                   Fraction_Digits       => 0,
                   Decimal_Max_Inclusive => Max_Unsigned_Int,
                   Decimal_Min_Inclusive => Zero,
                   others                => <>), Unsigned_Long);
      Unsigned_Short :=
        Register ("unsignedShort",
                  (Kind                  => Primitive_Decimal,
                   Mask                  => Fraction_Min_Max_Mask,
                   Fraction_Digits       => 0,
                   Decimal_Max_Inclusive => Max_Unsigned_Short,
                   Decimal_Min_Inclusive => Zero,
                   others                => <>), Unsigned_Int);
      T :=
        Register ("unsignedByte",
                  (Kind                  => Primitive_Decimal,
                   Mask                  => Fraction_Min_Max_Mask,
                   Fraction_Digits       => 0,
                   Decimal_Max_Inclusive => Max_Unsigned_Byte,
                   Decimal_Min_Inclusive => Zero,
                   others                => <>), Unsigned_Short);
      T := Register ("float", (Kind => Primitive_Float, others => <>),
                     Any_Simple_Type);
      T := Register ("double", (Kind => Primitive_Double, others => <>),
                     Any_Simple_Type);
      T := Register ("time", (Kind => Primitive_Time, others => <>),
                     Any_Simple_Type);
      T := Register ("dateTime", (Kind => Primitive_DateTime, others => <>),
                     Any_Simple_Type);
      T := Register ("gDay",       (Kind => Primitive_GDay, others => <>),
                     Any_Simple_Type);
      T := Register ("gMonthDay",  (Kind => Primitive_GMonthDay, others => <>),
                     Any_Simple_Type);
      T := Register ("gMonth",     (Kind => Primitive_GMonth, others => <>),
                     Any_Simple_Type);
      T := Register ("gYearMonth",
                     (Kind => Primitive_GYearMonth, others => <>),
                     Any_Simple_Type);
      T := Register ("gYear", (Kind => Primitive_GYear, others => <>),
                     Any_Simple_Type);
      T := Register ("date",       (Kind => Primitive_Date, others => <>),
                     Any_Simple_Type);
      T := Register ("duration",   (Kind => Primitive_Duration, others => <>),
                     Any_Simple_Type);

      --  Missing attribute "xml:lang" of type "language"
   end Register_Predefined_Types;

   -----------
   -- Equal --
   -----------

   procedure Equal
     (Simple_Types  : Simple_Type_Table;
      Enumerations  : Enumeration_Tables.Instance;
      Notations     : Symbol_Htable.HTable;
      Symbols       : Symbol_Table;
      Id_Table      : in out Symbol_Htable_Access;
      Simple_Type   : Simple_Type_Index;
      Ch1           : Sax.Symbols.Symbol;
      Ch2           : Unicode.CES.Byte_Sequence;
      Is_Equal      : out Boolean;
      XML_Version   : XML_Versions)
   is
      Descr : Simple_Type_Descr renames Simple_Types.Table (Simple_Type);
      Error : Symbol;
   begin
      case Descr.Kind is
         when Primitive_String .. Primitive_HexBinary =>
            Is_Equal := Get (Ch1).all = Ch2;
         when Primitive_Boolean   =>
            Is_Equal := Equal_Boolean (Symbols, Ch1, Ch2);
         when Primitive_Float | Primitive_Double  =>
            Is_Equal := Equal_Float (Symbols, Ch1, Ch2);
         when Primitive_Decimal   =>
            Is_Equal := Equal_Decimal (Symbols, Ch1, Ch2);
         when Primitive_Time      =>
            Is_Equal := Equal_Time (Symbols, Ch1, Ch2);
         when Primitive_DateTime =>
            Is_Equal := Equal_Date_Time (Symbols, Ch1, Ch2);
         when Primitive_GDay      =>
            Is_Equal := Equal_GDay (Symbols, Ch1, Ch2);
         when Primitive_GMonth    =>
            Is_Equal := Equal_GMonth (Symbols, Ch1, Ch2);
         when Primitive_GYear     =>
            Is_Equal := Equal_GYear (Symbols, Ch1, Ch2);
         when Primitive_Date      =>
            Is_Equal := Equal_Date (Symbols, Ch1, Ch2);
         when Primitive_Duration  =>
            Is_Equal := Equal_Duration (Symbols, Ch1, Ch2);
         when Primitive_GMonthDay =>
            Is_Equal := Equal_GMonth_Day (Symbols, Ch1, Ch2);
         when Primitive_GYearMonth =>
            Is_Equal := Equal_GYear_Month (Symbols, Ch1, Ch2);

         when Primitive_Union =>
            for S in Descr.Union'Range loop
               if Descr.Union (S) /= No_Simple_Type_Index then
                  --  We need to do space normalization here: when there is
                  --  a single type (ie not a union), the normalization has
                  --  already been done for the "fixed" value, but this isn't
                  --  doable in the case of union where the normalization
                  --  depends on which member is selected.
                  --  We actually also need to validate the value since we
                  --  don't know precisely for which members it is valid.

                  declare
                     Norm : Byte_Sequence := Get (Ch1).all;
                     Last : Integer := Norm'Last;
                  begin
                     Normalize_Whitespace
                       (Whitespace =>
                          Simple_Types.Table (Descr.Union (S)).Whitespace,
                        Val        => Norm,
                        Last       => Last);

                     if Debug then
                        Debug_Output
                          ("Equal for union, checking simpleType:"
                           & Descr.Union (S)'Img & " "
                           & Simple_Types.Table
                             (Descr.Union (S)).Whitespace'Img
                           & " Ch1=["
                           & Norm (Norm'First .. Last) & "]");
                     end if;

                     Validate_Simple_Type
                       (Simple_Types  => Simple_Types,
                        Enumerations  => Enumerations,
                        Notations     => Notations,
                        Symbols       => Symbols,
                        Id_Table      => Id_Table,
                        Insert_Id     => False,
                        Simple_Type   => Descr.Union (S),
                        Ch            => Norm (Norm'First .. Last),
                        Error         => Error,
                        XML_Version   => XML_Version);

                     if Debug and then Error /= No_Symbol then
                        Debug_Output
                          ("Equal: member doesn't apply: "
                           & Get (Error).all);
                     end if;

                     if Error = No_Symbol then
                        Equal
                          (Simple_Types => Simple_Types,
                           Enumerations => Enumerations,
                           Notations    => Notations,
                           Id_Table     => Id_Table,
                           Symbols      => Symbols,
                           Simple_Type  => Descr.Union (S),
                           Ch1  => Find (Symbols, Norm (Norm'First .. Last)),
                           Ch2  => Ch2,
                           Is_Equal     => Is_Equal,
                           XML_Version  => XML_Version);
                        if Is_Equal then
                           return;
                        end if;
                     end if;
                  end;
               end if;
            end loop;

            Is_Equal := False;

         when Primitive_List =>
            Is_Equal := Get (Ch1).all = Ch2;
      end case;
   end Equal;

   -------------------------------------
   -- Validate_Simple_Type_Characters --
   -------------------------------------

   procedure Validate_Simple_Type
     (Simple_Types  : Simple_Type_Table;
      Enumerations  : Enumeration_Tables.Instance;
      Notations     : Symbol_Htable.HTable;
      Symbols       : Symbol_Table;
      Id_Table      : in out Symbol_Htable_Access;
      Insert_Id     : Boolean := True;
      Simple_Type   : Simple_Type_Index;
      Ch            : Unicode.CES.Byte_Sequence;
      Error         : in out Symbol;
      XML_Version   : XML_Versions)
   is
      Descr : Simple_Type_Descr renames Simple_Types.Table (Simple_Type);
      Index : Integer;
      Char  : Unicode_Char;
      Matched : Match_Array (0 .. 0);

      procedure Validate_List_Item (Str : Byte_Sequence);
      procedure Validate_List_Item (Str : Byte_Sequence) is
      begin
         if Error = No_Symbol then
            Validate_Simple_Type
              (Simple_Types, Enumerations, Notations, Symbols, Id_Table,
               Simple_Type   => Descr.List_Item,
               Ch            => Str,
               Error         => Error,
               XML_Version   => XML_Version);
         end if;
      end Validate_List_Item;

      procedure Validate_List_Items is new For_Each_Item (Validate_List_Item);

   begin
      Error := No_Symbol;

      --  Check common facets

      if Descr.Mask (Facet_Enumeration) then
         declare
            Enum  : Enumeration_Index := Descr.Enumeration;
            Found : Boolean := False;
         begin
            while Enum /= No_Enumeration_Index loop
               Equal
                 (Simple_Types, Enumerations, Notations, Symbols, Id_Table,
                  Simple_Type,
                  Ch1 => Enumerations.Table (Enum).Value,
                  Ch2 => Ch,
                  Is_Equal => Found,
                  XML_Version => XML_Version);
               exit when Found;

               Enum := Enumerations.Table (Enum).Next;
            end loop;

            if not Found then
               Error := Find (Symbols, "Value not in the enumeration set");
               return;
            end if;
         end;
      end if;

      if Descr.Mask (Facet_Pattern) and then Descr.Pattern /= null then

         --  Check whether we have unicode char outside of ASCII

         Index := Ch'First;
         while Index <= Ch'Last loop
            Encoding.Read (Ch, Index, Char);
            if Char > 127 then
               --  Start with '#' because this is a non-implemented feature
               Error := Find
                 (Symbols, "#Regexp matching with unicode not supported");
               return;
            end if;
         end loop;

         for P in Descr.Pattern'Range loop
            Match (Descr.Pattern (P).Pattern.all, String (Ch), Matched);
            if Matched (0).First /= Ch'First
              or else Matched (0).Last /= Ch'Last
            then
               Error := Find
                 (Symbols,
                  "string pattern not matched: "
                  & Get (Descr.Pattern (P).Str).all);
               return;
            end if;
         end loop;
      end if;

      if Descr.Mask (Facet_Whitespace) then
         case Descr.Whitespace is
         when Preserve =>
            null; --  Always valid

         when Replace =>
            for C in Ch'Range loop
               if Ch (C) = ASCII.HT
                 or else Ch (C) = ASCII.LF
                 or else Ch (C) = ASCII.CR
               then
                  Error :=
                    Find (Symbols, "HT, LF and CR characters not allowed");
                  return;
               end if;
            end loop;

         when Collapse =>
            for C in Ch'Range loop
               if Ch (C) = ASCII.HT
                 or else Ch (C) = ASCII.LF
                 or else Ch (C) = ASCII.CR
               then
                  Error :=
                    Find (Symbols, "HT, LF and CR characters not allowed");
                  return;

               elsif Ch (C) = ' '
                 and then C < Ch'Last
                 and then Ch (C + 1) = ' '
               then
                  Error := Find
                    (Symbols, "Duplicate space characters not allowed");
                  return;
               end if;
            end loop;

            --  Leading or trailing white spaces are also forbidden
            if Ch'Length /= 0 then
               if Ch (Ch'First) = ' ' then
                  Error := Find
                    (Symbols, "Leading whitespaces not allowed");
                  return;
               elsif Ch (Ch'Last) = ' ' then
                  Error := Find
                    (Symbols, "Trailing whitespaces not allowed");
                  return;
               end if;
            end if;
         end case;
      end if;

      --  Type-specific facets

      case Descr.Kind is
         when Primitive_String =>
            Error :=  Validate_String (Descr, Symbols, Ch);
         when Primitive_HexBinary =>
            Error := Validate_HexBinary (Descr, Symbols, Ch);
         when Primitive_Notation =>
            Error := Validate_Notation (Notations, Descr, Symbols, Ch);
         when Primitive_Base64Binary =>
            Error := Validate_Base64Binary (Descr, Symbols, Ch);
         when Primitive_Language =>
            Error := Validate_Language (Descr, Symbols, Ch);
         when Primitive_QName    =>
            Error := Validate_QName (Descr, Symbols, Ch, XML_Version);
         when Primitive_NCName   =>
            Error := Validate_NCName (Descr, Symbols, Ch, XML_Version);
         when Primitive_ID       =>
            Error := Validate_NCName (Descr, Symbols, Ch, XML_Version);
            if Error = No_Symbol and then Insert_Id then
               Check_Id (Symbols, Id_Table, Ch, Error);
            end if;
         when Primitive_NCNames  =>
            Error := Validate_NCNames (Descr, Symbols, Ch, XML_Version);
         when Primitive_Name     =>
            Error :=  Validate_Name (Descr, Symbols, Ch, XML_Version);
         when Primitive_Any_URI  =>
            Error := Validate_URI (Descr, Symbols, Ch);
         when Primitive_NMTOKEN  =>
            Error := Validate_NMTOKEN (Descr, Symbols, Ch, XML_Version);
         when Primitive_NMTOKENS =>
            Error := Validate_NMTOKENS (Descr, Symbols, Ch, XML_Version);
         when Primitive_Boolean  =>
            Error := Validate_Boolean (Descr, Symbols, Ch);
         when Primitive_Decimal  =>
            Error := Validate_Decimal (Descr, Symbols, Ch);
         when Primitive_Float | Primitive_Double  =>
            Error := Validate_Double (Descr, Symbols, Ch);
         when Primitive_Time     =>
            Error :=  Validate_Time (Descr, Symbols, Ch);
         when Primitive_DateTime =>
            Error := Validate_Date_Time (Descr, Symbols, Ch);
         when Primitive_GDay =>
            Error := Validate_GDay (Descr, Symbols, Ch);
         when Primitive_GMonthDay =>
            Error := Validate_GMonth_Day (Descr, Symbols, Ch);
         when Primitive_GMonth   =>
            Error := Validate_GMonth (Descr, Symbols, Ch);
         when Primitive_GYearMonth =>
            Error := Validate_GYear_Month (Descr, Symbols, Ch);
         when Primitive_GYear    =>
            Error := Validate_GYear (Descr, Symbols, Ch);
         when Primitive_Date     =>
            Error := Validate_Date (Descr, Symbols, Ch);
         when Primitive_Duration =>
            Error := Validate_Duration (Descr, Symbols, Ch);

         when Primitive_Union    =>
            for S in Descr.Union'Range loop
               if Descr.Union (S) /= No_Simple_Type_Index then
                  Validate_Simple_Type
                    (Simple_Types  => Simple_Types,
                     Enumerations  => Enumerations,
                     Symbols       => Symbols,
                     Notations     => Notations,
                     Id_Table      => Id_Table,
                     Simple_Type   => Descr.Union (S),
                     Ch            => Ch,
                     Error         => Error,
                     XML_Version   => XML_Version);
                  if Error = No_Symbol then
                     return;
                  else
                     if Debug then
                        Debug_Output ("Checking union at index" & S'Img
                                      & " => " & Get (Error).all);
                     end if;
                  end if;
               end if;
            end loop;

            Error := Find (Symbols, "No matching type in the union");

         when Primitive_List     =>
            Validate_List_Items (Ch);
            if Error = No_Symbol then
               Error := Validate_List_Facets
                 (Descr, Symbols, Ch,
                  Descr.List_Length, Descr.List_Min_Length,
                  Descr.List_Max_Length);
            end if;
      end case;
   end Validate_Simple_Type;

   --------------------------
   -- HexBinary_Get_Length --
   --------------------------

   function HexBinary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural is
   begin
      return Sax.Encodings.Encoding.Length (Value) / 2;
   end HexBinary_Get_Length;

   -----------------------------
   -- Base64Binary_Get_Length --
   -----------------------------

   function Base64Binary_Get_Length
     (Value : Unicode.CES.Byte_Sequence) return Natural
   is
      Length : Natural := 0;
      C : Unicode_Char;
      Index : Positive := Value'First;
   begin
      while Index <= Value'Last loop
         Sax.Encodings.Encoding.Read (Value, Index, C);
         if C /= 16#20#
           and then C /= 16#A#
           and then C /= Character'Pos ('=')
         then
            Length := Length + 1;
         end if;
      end loop;
      return Length * 3 / 4;
   end Base64Binary_Get_Length;

   ----------------------
   -- Validate_NMTOKEN --
   ----------------------

   function Validate_NMTOKEN
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol is
   begin
      if not Is_Valid_Nmtoken (Ch, XML_Version) then
         return Find (Symbols, "Invalid NMTOKEN: """ & Ch & """");
      end if;
      return Validate_String (Descr, Symbols, Ch);
   end Validate_NMTOKEN;

   -----------------------
   -- Validate_NMTOKENS --
   -----------------------

   function Validate_NMTOKENS
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol is
   begin
      if not Is_Valid_Nmtokens (Ch, XML_Version) then
         return Find (Symbols, "Invalid NMTOKENS: """ & Ch & """");
      end if;

      return Validate_List_Facets
        (Descr, Symbols, Ch, Descr.String_Length,
         Descr.String_Min_Length, Descr.String_Max_Length);
   end Validate_NMTOKENS;

   -------------------
   -- Validate_Name --
   -------------------

   function Validate_Name
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol is
   begin
      if not Is_Valid_Name (Ch, XML_Version) then
         return Find (Symbols, "Invalid Name: """ & Ch & """");
      end if;
      return Validate_String (Descr, Symbols, Ch);
   end Validate_Name;

   ---------------------
   -- Validate_NCName --
   ---------------------

   function Validate_NCName
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol is
   begin
      if not Is_Valid_NCname (Ch, XML_Version) then
         return Find (Symbols, "Invalid NCName: """ & Ch & """");
      end if;
      return Validate_String (Descr, Symbols, Ch);
   end Validate_NCName;

   ----------------------
   -- Validate_NCNames --
   ----------------------

   function Validate_NCNames
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol is
   begin
      if not Is_Valid_NCnames (Ch, XML_Version) then
         return Find (Symbols, "Invalid NCName: """ & Ch & """");
      end if;

      return Validate_List_Facets
        (Descr, Symbols, Ch, Descr.String_Length,
         Descr.String_Min_Length, Descr.String_Max_Length);
   end Validate_NCNames;

   -----------------------
   -- Validate_Language --
   -----------------------

   function Validate_Language
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol is
   begin
      if not Is_Valid_Language_Name (Ch) then
         return Find (Symbols, "Invalid language: """ & Ch & """");
      end if;
      return Validate_String (Descr, Symbols, Ch);
   end Validate_Language;

   --------------------
   -- Validate_QName --
   --------------------

   function Validate_QName
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      XML_Version   : XML_Versions) return Symbol is
   begin
      if not Is_Valid_QName (Ch, XML_Version) then
         return Find (Symbols, "Invalid QName: """ & Ch & """");
      end if;
      return Validate_String (Descr, Symbols, Ch);
   end Validate_QName;

   ------------------
   -- Validate_URI --
   ------------------

   function Validate_URI
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol is
   begin
      if not Is_Valid_URI (Ch) then
         return Find (Symbols, "Invalid anyURI: """ & Ch & """");
      end if;
      return Validate_String (Descr, Symbols, Ch);
   end Validate_URI;

   ---------------------
   -- Validate_String --
   ---------------------

   function Validate_String
     (Descr   : Simple_Type_Descr;
      Symbols : Sax.Utils.Symbol_Table;
      Ch      : Unicode.CES.Byte_Sequence) return Symbol
   is
      function Internal_Facets is new Validate_Length_Facets
        (Encoding.Length.all);
   begin
      return Internal_Facets
        (Symbols, Ch, Descr.Mask, Descr.String_Length,
         Descr.String_Min_Length, Descr.String_Max_Length);
   end Validate_String;

   -----------------------
   -- Validate_Notation --
   -----------------------

   function Validate_Notation
     (Notations     : Symbol_Htable.HTable;
      Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Error : Symbol;
   begin
      Error := Validate_String (Descr, Symbols, Ch);
      if Error /= No_Symbol then
         return Error;
      end if;

      if Symbol_Htable.Get (Notations, Find (Symbols, Ch)) = No_Symbol then
         Error := Find
           (Symbols, "NOTATION """ & Ch & """ undefined in this document");
      end if;

      return Error;
   end Validate_Notation;

   ------------------------
   -- Validate_HexBinary --
   ------------------------

   function Validate_HexBinary
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol is
   begin
      if Encoding.Length (Ch) mod 2 /= 0 then
         return Find
           (Symbols, "HexBinary length must be an even number of characters");
      end if;

      if not Is_Valid_HexBinary (Ch) then
         return Find (Symbols, "Invalid hexBinary: """ & Ch & """");
      end if;

      return Validate_HexBinary_Facets
        (Symbols, Ch, Descr.Mask,
        Descr.String_Length, Descr.String_Min_Length, Descr.String_Max_Length);
   end Validate_HexBinary;

   ---------------------------
   -- Validate_Base64Binary --
   ---------------------------

   function Validate_Base64Binary
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol is
   begin
      if not Is_Valid_Base64Binary (Ch) then
         return Find (Symbols, "Invalid base64Binary: """ & Ch & """");
      end if;

      return Validate_Base64Binary_Facets
        (Symbols, Ch, Descr.Mask,
        Descr.String_Length, Descr.String_Min_Length, Descr.String_Max_Length);
   end Validate_Base64Binary;

   --------------------------
   -- Validate_List_Facets --
   --------------------------

   function Validate_List_Facets
     (Descr   : Simple_Type_Descr;
      Symbols : Sax.Utils.Symbol_Table;
      Ch      : Unicode.CES.Byte_Sequence;
      Length, Min_Length, Max_Length : Integer) return Symbol
   is
      function List_Get_Length
        (Value : Unicode.CES.Byte_Sequence) return Natural;
      function List_Get_Length
        (Value : Unicode.CES.Byte_Sequence) return Natural
      is
         Length : Natural := 0;
         C      : Unicode_Char;
         Index  : Natural := Value'First;
      begin
         if Value = "" then
            return 0;
         end if;

         while Index <= Value'Last loop
            Encoding.Read (Value, Index, C);
            while C = Unicode.Names.Basic_Latin.Space loop
               Length := Length + 1;
               Encoding.Read (Value, Index, C);
            end loop;
         end loop;

         return Length + 1;
      end List_Get_Length;

      L : Natural;
   begin
      if Descr.Mask (Facet_Length)
        or else Descr.Mask (Facet_Min_Length)
        or else Descr.Mask (Facet_Max_Length)
      then
         L := List_Get_Length (Ch);
      else
         return No_Symbol;
      end if;

      if Descr.Mask (Facet_Length) then
         if L /= Length then
            return Find
              (Symbols,
               "Invalid size, must have"
               & Integer'Image (Length) & " items");
         end if;
      end if;

      if Descr.Mask (Facet_Min_Length) then
         if L < Min_Length then
            return Find
              (Symbols,
               "Not enough items, minimum number is"
               & Integer'Image (Min_Length));
         end if;
      end if;

      if Descr.Mask (Facet_Max_Length) then
         if L > Max_Length then
            return Find
              (Symbols,
               "Too many items, maximum number is"
               & Integer'Image (Max_Length));
         end if;
      end if;

      return No_Symbol;
   end Validate_List_Facets;

   -------------------
   -- Boolean_Value --
   -------------------

   procedure Boolean_Value
     (Symbols : Symbol_Table;
      Ch      : Byte_Sequence;
      Val     : out Boolean;
      Error   : out Symbol)
   is
      First : Integer;
      Index : Integer;
      C     : Unicode_Char;
   begin
      Val := False;

      if Ch = "" then
         Error := Find (Symbols, "Invalid value for boolean type: """"");
         return;
      end if;

      --  Check we do have a valid boolean representation (skip leading spaces)

      First := Ch'First;

      while First <= Ch'Last loop
         Index := First;
         Encoding.Read (Ch, First, C);
         exit when not Is_White_Space (C);
      end loop;

      if C = Digit_Zero or C = Digit_One then
         Val := C = Digit_One;
         if First <= Ch'Last then
            Encoding.Read (Ch, First, C);
         end if;

      elsif Index + True_Sequence'Length - 1 <= Ch'Last
        and then Ch (Index .. Index + True_Sequence'Length - 1) = True_Sequence
      then
         First := Index + True_Sequence'Length;
         Val := True;

      elsif Index + False_Sequence'Length - 1 <= Ch'Last
        and then Ch (Index .. Index + False_Sequence'Length - 1) =
          False_Sequence
      then
         First := Index + False_Sequence'Length;
         Val := False;

      else
         Error := Find
           (Symbols, "Invalid value for boolean type: """ & Ch & """");
         return;
      end if;

      --  Skip trailing spaces

      while First <= Ch'Last loop
         Encoding.Read (Ch, First, C);
         if not Is_White_Space (C) then
            Error := Find
              (Symbols, "Invalid value for boolean type: """ & Ch & """");
            return;
         end if;
      end loop;

      Error := No_Symbol;
   end Boolean_Value;

   ----------------------
   -- Validate_Boolean --
   ----------------------

   function Validate_Boolean
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      pragma Unreferenced (Descr);
      Val   : Boolean;
      Error : Symbol;
   begin
      Boolean_Value (Symbols, Ch, Val, Error);
      if Error /= No_Symbol then
         return Error;
      end if;

      return No_Symbol;
   end Validate_Boolean;

   -----------
   -- Value --
   -----------

   procedure Value (Symbols : Symbol_Table;
                    Ch      : Byte_Sequence;
                    Val     : out XML_Float;
                    Error   : out Symbol) is
   begin
      begin
         Val := Value (Ch);
      exception
         when Constraint_Error =>
            Error := Find (Symbols, "Invalid value: """ & Ch & """");
            return;
      end;
      Error := No_Symbol;
   end Value;

   --------------------
   -- Validate_Range --
   --------------------

   procedure Validate_Range
     (Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence;
      Mask          : Facets_Mask;
      Min_Inclusive : T;
      Min_Exclusive : T;
      Max_Inclusive : T;
      Max_Exclusive : T;
      Error         : out Symbol;
      Val           : out T) is
   begin
      Value
        (Symbols  => Symbols,
         Ch       => Ch,
         Val      => Val,
         Error    => Error);
      if Error /= No_Symbol then
         return;
      end if;

      if Mask (Facet_Min_Inclusive) then
         if Val < Min_Inclusive then
            Error := Find
              (Symbols,
               Ch & " is smaller than minInclusive ("
               & Image (Min_Inclusive) & ")");
            return;
         end if;
      end if;

      if Mask (Facet_Min_Exclusive) then
         if Val <= Min_Exclusive then
            Error := Find
              (Symbols,
               Ch & " is smaller than minExclusive ("
               & Image (Min_Exclusive) & ")");
            return;
         end if;
      end if;

      if Mask (Facet_Max_Inclusive) then
         if Max_Inclusive < Val then
            Error := Find
              (Symbols,
               Ch & " is greater than maxInclusive ("
               & Image (Max_Inclusive) & ")");
            return;
         end if;
      end if;

      if Mask (Facet_Max_Exclusive) then
         if Max_Exclusive <= Val then
            Error := Find
              (Symbols,
               Ch & " is greater than maxExclusive ("
               & Image (Max_Exclusive) & ")");
            return;
         end if;
      end if;
   end Validate_Range;

   procedure Validate_Double_Facets is new Validate_Range (XML_Float);
   procedure Validate_Decimal_Facets is new Validate_Range
     (Arbitrary_Precision_Number, Value => Value_No_Exponent);
   procedure Validate_Duration_Facets is new Validate_Range (Duration_T);
   procedure Validate_Date_Time_Facets is new Validate_Range (Date_Time_T);
   procedure Validate_Date_Facets is new Validate_Range (Date_T);
   procedure Validate_Time_Facets is new Validate_Range (Time_T);
   procedure Validate_GDay_Facets is new Validate_Range (GDay_T);
   procedure Validate_GMonth_Day_Facets is new Validate_Range (GMonth_Day_T);
   procedure Validate_GMonth_Facets is new Validate_Range (GMonth_T);
   procedure Validate_GYear_Facets is new Validate_Range (GYear_T);
   procedure Validate_GYear_Month_Facets is new Validate_Range
     (GYear_Month_T);

   ---------------------
   -- Validate_Double --
   ---------------------

   function Validate_Double
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : XML_Float;
      Error : Symbol;
   begin
      Validate_Double_Facets
        (Symbols, Ch, Descr.Mask, Descr.Float_Min_Inclusive,
         Descr.Float_Min_Exclusive, Descr.Float_Max_Inclusive,
         Descr.Float_Max_Exclusive, Error, Val);
      return Error;
   end Validate_Double;

   -----------------------
   -- Validate_Duration --
   -----------------------

   function Validate_Duration
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : Duration_T;
      Error : Symbol;
   begin
      Validate_Duration_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.Duration_Min_Inclusive, Descr.Duration_Min_Exclusive,
         Descr.Duration_Max_Inclusive, Descr.Duration_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_Duration;

   ------------------------
   -- Validate_Date_Time --
   ------------------------

   function Validate_Date_Time
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : Date_Time_T;
      Error : Symbol;
   begin
      Validate_Date_Time_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.DateTime_Min_Inclusive, Descr.DateTime_Min_Exclusive,
         Descr.DateTime_Max_Inclusive, Descr.DateTime_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_Date_Time;

   -------------------
   -- Validate_Date --
   -------------------

   function Validate_Date
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : Date_T;
      Error : Symbol;
   begin
      Validate_Date_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.Date_Min_Inclusive, Descr.Date_Min_Exclusive,
         Descr.Date_Max_Inclusive, Descr.Date_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_Date;

   -------------------
   -- Validate_Time --
   -------------------

   function Validate_Time
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : Time_T;
      Error : Symbol;
   begin
      Validate_Time_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.Time_Min_Inclusive, Descr.Time_Min_Exclusive,
         Descr.Time_Max_Inclusive, Descr.Time_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_Time;

   -------------------
   -- Validate_GDay --
   -------------------

   function Validate_GDay
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : GDay_T;
      Error : Symbol;
   begin
      Validate_GDay_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.GDay_Min_Inclusive, Descr.GDay_Min_Exclusive,
         Descr.GDay_Max_Inclusive, Descr.GDay_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_GDay;

   -------------------------
   -- Validate_GMonth_Day --
   -------------------------

   function Validate_GMonth_Day
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : GMonth_Day_T;
      Error : Symbol;
   begin
      Validate_GMonth_Day_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.GMonthDay_Min_Inclusive, Descr.GMonthDay_Min_Exclusive,
         Descr.GMonthDay_Max_Inclusive, Descr.GMonthDay_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_GMonth_Day;

   ---------------------
   -- Validate_GMonth --
   ---------------------

   function Validate_GMonth
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : GMonth_T;
      Error : Symbol;
   begin
      Validate_GMonth_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.GMonth_Min_Inclusive, Descr.GMonth_Min_Exclusive,
         Descr.GMonth_Max_Inclusive, Descr.GMonth_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_GMonth;

   --------------------
   -- Validate_GYear --
   --------------------

   function Validate_GYear
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : GYear_T;
      Error : Symbol;
   begin
      Validate_GYear_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.GYear_Min_Inclusive, Descr.GYear_Min_Exclusive,
         Descr.GYear_Max_Inclusive, Descr.GYear_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_GYear;

   --------------------------
   -- Validate_GYear_Month --
   --------------------------

   function Validate_GYear_Month
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Val   : GYear_Month_T;
      Error : Symbol;
   begin
      Validate_GYear_Month_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.GYearMonth_Min_Inclusive, Descr.GYearMonth_Min_Exclusive,
         Descr.GYearMonth_Max_Inclusive, Descr.GYearMonth_Max_Exclusive,
         Error, Val);
      return Error;
   end Validate_GYear_Month;

   ----------------------
   -- Validate_Decimal --
   ----------------------

   function Validate_Decimal
     (Descr         : Simple_Type_Descr;
      Symbols       : Sax.Utils.Symbol_Table;
      Ch            : Unicode.CES.Byte_Sequence) return Symbol
   is
      Error : Symbol;
      Val   : Arbitrary_Precision_Number;
   begin
      Validate_Decimal_Facets
        (Symbols, Ch, Descr.Mask,
         Descr.Decimal_Min_Inclusive, Descr.Decimal_Min_Exclusive,
         Descr.Decimal_Max_Inclusive, Descr.Decimal_Max_Exclusive,
        Error, Val);
      if Error /= No_Symbol then
         return Error;
      end if;

      return Check_Digits
        (Symbols         => Symbols,
         Num             => Val,
         Fraction_Digits => Descr.Fraction_Digits,
         Total_Digits    => Descr.Total_Digits);
   end Validate_Decimal;

   --------------------
   -- Convert_Regexp --
   --------------------

   function Convert_Regexp
     (Regexp : Unicode.CES.Byte_Sequence) return String
   is
      Result : Unbounded_String;
      Tmp    : Unbounded_String;
      Pos    : Integer := Regexp'First;
      C      : Character;

      function Next_Char return Character;
      --  Read the next char from the regexp, and check it is ASCII

      function Next_Char return Character is
         Char   : Unicode_Char;
      begin
         Encoding.Read (Regexp, Pos, Char);

         if Char > 127 then
            Raise_Exception
              (XML_Not_Implemented'Identity,
               "Unicode regexps are not supported");
         end if;

         return Character'Val (Integer (Char));
      end Next_Char;

   begin
      while Pos <= Regexp'Last loop
         C := Next_Char;

         if C = '[' then
            Append (Result, C);
            Tmp := Null_Unbounded_String;

            while Pos <= Regexp'Last loop
               C := Next_Char;

               if C = ']' then
                  Append (Tmp, C);
                  exit;

               elsif C = '\' and then Pos <= Regexp'Last then
                  C := Next_Char;

                  case C is
                     when 'i' =>
                        --  rule [99] in XMLSchema specifications
                        Append (Tmp, "A-Za-z:_");

                     when 'c' =>
                        Append (Tmp, "a-z:A-Z0-9._-");

                     when 'w' =>
                        Append (Tmp, "a-zA-Z0-9`");

                     when 'I' | 'C' | '?' =>
                        Raise_Exception
                          (XML_Not_Implemented'Identity,
                           "Unsupported regexp construct: \" & C);

                     when 'P' | 'p' =>
                        if Pos <= Regexp'Last
                          and then Regexp (Pos) = '{'
                        then
                           Raise_Exception
                             (XML_Not_Implemented'Identity,
                              "Unsupported regexp construct: \P{...}");
                        else
                           Append (Tmp, '\' & C);
                        end if;

                     when others =>
                        Append (Tmp, '\' & C);
                  end case;

               else
                  if C = '-'
                    and then Pos <= Regexp'Last
                    and then Regexp (Pos) = '['
                  then
                     Raise_Exception
                       (XML_Not_Implemented'Identity,
                        "Unsupported regexp construct: [...-[...]]");
                  end if;

                  Append (Tmp, C);
               end if;
            end loop;

            Append (Result, Tmp);

         --  ??? Some tests in the old w3c testsuite seem to imply that
         --  \c and \i are valid even outside character classes. Not sure about
         --  this though

         elsif C = '\' and then Pos <= Regexp'Last then
            C := Next_Char;

            case C is
               when 'i' =>
                  --  rule [99] in XMLSchema specifications
                  Append (Result, "[A-Za-z:_]");

               when 'c' =>
                  Append (Result, "[a-z:A-Z0-9._-]");

               when 'w' =>
                  Append (Result, "[a-zA-Z0-9`]");

               when 'I' | 'C' =>
                  Raise_Exception
                    (XML_Not_Implemented'Identity,
                     "Unsupported regexp construct: \" & C);

               when 'P' | 'p' =>
                  if Pos <= Regexp'Last
                    and then Regexp (Pos) = '{'
                  then
                     Raise_Exception
                       (XML_Not_Implemented'Identity,
                        "Unsupported regexp construct: \P{...}");
                  else
                     Append (Result, '\' & C);
                  end if;

               when others =>
                  Append (Result, '\' & C);
            end case;

         else
            Append (Result, C);
         end if;
      end loop;

      return Anchor (To_String (Result));
   end Convert_Regexp;

   ------------------------
   -- Missing_End_Anchor --
   ------------------------

   function Missing_End_Anchor (Str : String) return Boolean is
   begin
      --  Do not add '$' if Str ends with a single \, since it is
      --  invalid anyway
      return Str'Length = 0
        or else
          (Str (Str'Last) /= '$'
           and then (Str (Str'Last) /= '\'
                     or else (Str'Length /= 1
                              and then Str (Str'Last - 1) = '\')));
   end Missing_End_Anchor;

   --------------------------
   -- Missing_Start_Anchor --
   --------------------------

   function Missing_Start_Anchor (Str : String) return Boolean is
   begin
      --  Do not add '^' if we start with an operator, since Str is invalid
      return Str'Length = 0
        or else not (Str (Str'First) = '^'
                     or else Str (Str'First) = '*'
                     or else Str (Str'First) = '+'
                     or else Str (Str'First) = '?');
   end Missing_Start_Anchor;

   ------------
   -- Anchor --
   ------------

   function Anchor (Str : String) return String is
      Start : constant Boolean := Missing_Start_Anchor (Str);
      Last  : constant Boolean := Missing_End_Anchor (Str);
   begin
      if Start and Last then
         return "^(" & Str & ")$";
      elsif Start then
         return "^" & Str;
      elsif Last then
         return Str & "$";
      else
         return Str;
      end if;
   end Anchor;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Facets       : in out All_Facets;
      Symbols      : Sax.Utils.Symbol_Table;
      Enumerations : in out Enumeration_Tables.Instance;
      Facet_Name   : Sax.Symbols.Symbol;
      Value        : Sax.Symbols.Symbol;
      Loc          : Sax.Locators.Location)
   is
      Val : Symbol;
   begin
      if Get (Facet_Name).all = "pattern" then
         --  Do not normalize the value
         if Facets (Facet_Pattern).Value /= No_Symbol then
            Facets (Facet_Pattern) :=
              (Find
                 (Symbols,
                  '(' & Get (Facets (Facet_Pattern).Value).all
                  & ")|(" & Get (Value).all & ')'),
               No_Enumeration_Index, Loc);
         else
            Facets (Facet_Pattern) := (Value, No_Enumeration_Index, Loc);
         end if;

         return;
      end if;

      Val := Find
        (Symbols, Trim (Get (Value).all, Ada.Strings.Both));

      if Get (Facet_Name).all = "whiteSpace" then
         Facets (Facet_Whitespace) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "enumeration" then
         Append (Enumerations, (Value => Val,
                                Next  => Facets (Facet_Enumeration).Enum));
         Facets (Facet_Enumeration) := (No_Symbol, Last (Enumerations), Loc);
      elsif Get (Facet_Name).all = "minInclusive" then
         Facets (Facet_Min_Inclusive) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "maxInclusive" then
         Facets (Facet_Max_Inclusive) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "minExclusive" then
         Facets (Facet_Min_Exclusive) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "maxExclusive" then
         Facets (Facet_Max_Exclusive) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "length" then
         Facets (Facet_Length) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "minLength" then
         Facets (Facet_Min_Length) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "maxLength" then
         Facets (Facet_Max_Length) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "totalDigits" then
         Facets (Facet_Total_Digits) := (Val, No_Enumeration_Index, Loc);
      elsif Get (Facet_Name).all = "fractionDigits" then
         Facets (Facet_Fraction_Digits) := (Val, No_Enumeration_Index, Loc);
      else
         pragma Assert (False, "Invalid facet:");
         null;
      end if;
   end Add_Facet;

   ---------------------------------
   -- Override_Single_Range_Facet --
   ---------------------------------

   procedure Override_Single_Range_Facet
     (Symbols       : Sax.Utils.Symbol_Table;
      Facets        : All_Facets;
      Facet         : Facet_Enum;
      Mask          : in out Facets_Mask;
      Val           : in out T;
      Error         : in out Symbol;
      Error_Loc     : in out Location) is
   begin
      if Error = No_Symbol and then Facets (Facet) /= No_Facet_Value then
         Value
           (Symbols,
            Ch    => Get (Facets (Facet).Value).all,
            Val   => Val,
            Error => Error);
         if Error /= No_Symbol then
            Error_Loc := Facets (Facet).Loc;
         else
            Mask (Facet) := True;
         end if;
      end if;
   end Override_Single_Range_Facet;

   ---------------------------
   -- Override_Range_Facets --
   ---------------------------

   procedure Override_Range_Facets
     (Symbols       : Sax.Utils.Symbol_Table;
      Facets        : All_Facets;
      Mask          : in out Facets_Mask;
      Min_Inclusive : in out T;
      Min_Exclusive : in out T;
      Max_Inclusive : in out T;
      Max_Exclusive : in out T;
      Error         : out Symbol;
      Error_Loc     : out Location)
   is
      procedure Do_Override is new Override_Single_Range_Facet (T, Value);
   begin
      Do_Override (Symbols, Facets, Facet_Max_Inclusive, Mask,
                   Max_Inclusive, Error, Error_Loc);
      Do_Override (Symbols, Facets, Facet_Max_Exclusive, Mask,
                   Max_Exclusive, Error, Error_Loc);
      Do_Override (Symbols, Facets, Facet_Min_Inclusive, Mask,
                   Min_Inclusive, Error, Error_Loc);
      Do_Override (Symbols, Facets, Facet_Min_Exclusive, Mask,
                   Min_Exclusive, Error, Error_Loc);
   end Override_Range_Facets;

   procedure Override_Decimal_Facets
     is new Override_Range_Facets (Arbitrary_Precision_Number);
   procedure Override_Float_Facets is new Override_Range_Facets (XML_Float);
   procedure Override_Duration_Facets
     is new Override_Range_Facets (Duration_T);
   procedure Override_Date_Time_Facets
     is new Override_Range_Facets (Date_Time_T);
   procedure Override_Date_Facets is new Override_Range_Facets (Date_T);
   procedure Override_Time_Facets is new Override_Range_Facets (Time_T);
   procedure Override_GDay_Facets is new Override_Range_Facets (GDay_T);
   procedure Override_GMonth_Day_Facets
     is new Override_Range_Facets (GMonth_Day_T);
   procedure Override_GMonth_Facets is new Override_Range_Facets (GMonth_T);
   procedure Override_GYear_Facets is new Override_Range_Facets (GYear_T);
   procedure Override_GYear_Month_Facets
     is new Override_Range_Facets (GYear_Month_T);

   ----------------------------
   -- Override_Length_Facets --
   ----------------------------

   procedure Override_Length_Facets
     (Symbols       : Sax.Utils.Symbol_Table;
      Facets        : All_Facets;
      Mask          : in out Facets_Mask;
      Length        : in out Integer;
      Min_Length    : in out Integer;
      Max_Length    : in out Integer;
      Error         : out Symbol;
      Error_Loc     : out Location)
   is
   begin
      if Facets (Facet_Length) /= No_Facet_Value then
         begin
            Length := Natural'Value
              (Get (Facets (Facet_Length).Value).all);
            Mask (Facet_Length) := True;
         exception
            when Constraint_Error =>
               Error := Find
                 (Symbols, "Expecting integer for length facet");
               Error_Loc := Facets (Facet_Length).Loc;
         end;
      end if;

      if Facets (Facet_Min_Length) /= No_Facet_Value then
         begin
            Min_Length := Natural'Value
              (Get (Facets (Facet_Min_Length).Value).all);
            Mask (Facet_Min_Length) := True;
         exception
            when Constraint_Error =>
               Error := Find
                 (Symbols, "Expecting integer for minLength facet");
               Error_Loc := Facets (Facet_Min_Length).Loc;
         end;
      end if;

      if Facets (Facet_Max_Length) /= No_Facet_Value then
         begin
            Max_Length := Natural'Value
              (Get (Facets (Facet_Max_Length).Value).all);
            Mask (Facet_Max_Length) := True;
         exception
            when Constraint_Error =>
               Error := Find
                 (Symbols, "Expecting integer for maxlength facet");
               Error_Loc := Facets (Facet_Max_Length).Loc;
         end;
      end if;
   end Override_Length_Facets;

   --------------
   -- Override --
   --------------

   procedure Override
     (Simple     : in out Simple_Type_Descr;
      Facets     : All_Facets;
      Symbols    : Sax.Utils.Symbol_Table;
      As_Restriction : Boolean;
      Error      : out Sax.Symbols.Symbol;
      Error_Loc  : out Sax.Locators.Location)
   is
      function Compile_Regexp (Str : Symbol) return Pattern_Matcher_Access;

      function Compile_Regexp (Str : Symbol) return Pattern_Matcher_Access is
         Convert : constant String := Convert_Regexp (Get (Str).all);
      begin
         if Debug then
            Debug_Output ("Compiling regexp as " & Convert);
         end if;

         return new Pattern_Matcher'(Compile (Convert));
      exception
         when  GNAT.Regpat.Expression_Error =>
            Error_Loc := Facets (Facet_Pattern).Loc;
            Error := Find
              (Symbols,
               "Invalid regular expression "
               & Get (Str).all & " (converted to " & Convert & ")");
            return null;
      end Compile_Regexp;

      Val : Symbol;
      Tmp : Pattern_Matcher_Array_Access;
   begin
      if Facets (Facet_Whitespace) /= No_Facet_Value then
         Val := Facets (Facet_Whitespace).Value;
         if Get (Val).all = "preserve" then
            Simple.Whitespace := Preserve;
         elsif Get (Val).all = "replace" then
            Simple.Whitespace := Replace;
         elsif Get (Val).all = "collapse" then
            Simple.Whitespace := Collapse;
         else
            Error_Loc := Facets (Facet_Whitespace).Loc;
            Error := Find (Symbols, "Invalid value for whiteSpace facet: "
                           & Get (Val).all);
            return;
         end if;

         Simple.Mask (Facet_Whitespace) := True;
      end if;

      if Facets (Facet_Pattern) /= No_Facet_Value then
         Val := Facets (Facet_Pattern).Value;

         begin
            if As_Restriction then
               --  We must match all patterns (from base and from extension),
               --  and we cannot combine them. So we need to add one more
               --  pattern to the facets.
               --  [Simple] is a copy of the base type, and will be the new
               --  restriction on exit.

               if Simple.Pattern = null then
                  Simple.Pattern := new Pattern_Matcher_Array (1 .. 1);
               else
                  Tmp := Simple.Pattern;
                  Simple.Pattern :=
                    new Pattern_Matcher_Array (Tmp'First .. Tmp'Last + 1);
                  Simple.Pattern (Tmp'Range) := Tmp.all;
                  Unchecked_Free (Tmp);
               end if;

               Simple.Pattern (Simple.Pattern'Last) :=
                 (Str     => Val,
                  Pattern => Compile_Regexp (Val));

            else
               --  We must combine the base's patterns with the extension's
               --  pattern, since the type must match either of those.
               --  The number of patterns does not change

               if Simple.Pattern = null then
                  Simple.Pattern := new Pattern_Matcher_Array'
                    (1 => (Str => Val, Pattern => Compile_Regexp (Val)));
               else
                  for P in Simple.Pattern'Range loop
                     Simple.Pattern (P).Str := Find
                       (Symbols,
                        '(' & Get (Simple.Pattern (P).Str).all
                        & ")|(" & Get (Val).all & ')');
                     Unchecked_Free (Simple.Pattern (P).Pattern);
                     Simple.Pattern (P).Pattern :=
                       Compile_Regexp (Simple.Pattern (P).Str);
                  end loop;
               end if;
            end if;

         exception
            when E : XML_Not_Implemented =>
               Error := Find (Symbols, '#' & Exception_Message (E));
               Free (Simple.Pattern);
         end;

         if Error /= No_Symbol then
            Error_Loc := Facets (Facet_Pattern).Loc;
            return;
         end if;

         Simple.Mask (Facet_Pattern) := True;
      end if;

      if Facets (Facet_Enumeration) /= No_Facet_Value then
         Simple.Enumeration := Facets (Facet_Enumeration).Enum;
         Simple.Mask (Facet_Enumeration) := True;
      end if;

      Error := No_Symbol;

      case Simple.Kind is
         when Primitive_Union =>
            null;

         when Primitive_List =>
            Override_Length_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.List_Length, Simple.List_Min_Length,
               Simple.List_Max_Length, Error, Error_Loc);

         when Primitive_String .. Primitive_HexBinary =>
            Override_Length_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.String_Length, Simple.String_Min_Length,
               Simple.String_Max_Length, Error, Error_Loc);

         when Primitive_Boolean =>
            null;

         when Primitive_Float | Primitive_Double  =>
            Override_Float_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.Float_Min_Inclusive, Simple.Float_Min_Exclusive,
               Simple.Float_Max_Inclusive, Simple.Float_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_Decimal =>
            Override_Decimal_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.Decimal_Min_Inclusive, Simple.Decimal_Min_Exclusive,
               Simple.Decimal_Max_Inclusive, Simple.Decimal_Max_Exclusive,
               Error, Error_Loc);

            if Error = No_Symbol then
               if Facets (Facet_Total_Digits) /= No_Facet_Value then
                  begin
                     Simple.Total_Digits := Positive'Value
                       (Get (Facets (Facet_Total_Digits).Value).all);
                     Simple.Mask (Facet_Total_Digits) := True;
                  exception
                     when Constraint_Error =>
                        Error := Find
                          (Symbols, "Expecting integer for totalDigits facet");
                        Error_Loc := Facets (Facet_Total_Digits).Loc;
                  end;
               end if;

               if Facets (Facet_Fraction_Digits) /= No_Facet_Value then
                  begin
                     Simple.Fraction_Digits := Natural'Value
                       (Get (Facets (Facet_Fraction_Digits).Value).all);
                     Simple.Mask (Facet_Fraction_Digits) := True;
                  exception
                     when Constraint_Error =>
                        Error := Find
                          (Symbols,
                           "Expecting integer for fractionDigits facet");
                        Error_Loc := Facets (Facet_Fraction_Digits).Loc;
                  end;
               end if;

               if Simple.Fraction_Digits /= Natural'Last
                 and then Simple.Total_Digits /= Positive'Last
                 and then Simple.Fraction_Digits > Simple.Total_Digits
               then
                  Error_Loc := Facets (Facet_Fraction_Digits).Loc;
                  Error := Find
                    (Symbols,
                     "fractionDigits cannot be greater than totalDigits");
               end if;
            end if;

         when Primitive_Time =>
            Override_Time_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.Time_Min_Inclusive, Simple.Time_Min_Exclusive,
               Simple.Time_Max_Inclusive, Simple.Time_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_DateTime =>
            Override_Date_Time_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.DateTime_Min_Inclusive, Simple.DateTime_Min_Exclusive,
               Simple.DateTime_Max_Inclusive, Simple.DateTime_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_GDay =>
            Override_GDay_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.GDay_Min_Inclusive, Simple.GDay_Min_Exclusive,
               Simple.GDay_Max_Inclusive, Simple.GDay_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_GMonthDay =>
            Override_GMonth_Day_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.GMonthDay_Min_Inclusive, Simple.GMonthDay_Min_Exclusive,
               Simple.GMonthDay_Max_Inclusive, Simple.GMonthDay_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_GMonth =>
            Override_GMonth_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.GMonth_Min_Inclusive, Simple.GMonth_Min_Exclusive,
               Simple.GMonth_Max_Inclusive, Simple.GMonth_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_GYearMonth =>
            Override_GYear_Month_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.GYearMonth_Min_Inclusive,
               Simple.GYearMonth_Min_Exclusive,
               Simple.GYearMonth_Max_Inclusive,
               Simple.GYearMonth_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_GYear =>
            Override_GYear_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.GYear_Min_Inclusive, Simple.GYear_Min_Exclusive,
               Simple.GYear_Max_Inclusive, Simple.GYear_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_Date =>
            Override_Date_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.Date_Min_Inclusive, Simple.Date_Min_Exclusive,
               Simple.Date_Max_Inclusive, Simple.Date_Max_Exclusive,
               Error, Error_Loc);

         when Primitive_Duration =>
            Override_Duration_Facets
              (Symbols, Facets, Simple.Mask,
               Simple.Duration_Min_Inclusive, Simple.Duration_Min_Exclusive,
               Simple.Duration_Max_Inclusive, Simple.Duration_Max_Exclusive,
               Error, Error_Loc);
      end case;

      --  ??? Should detect unused facets and report errors
   end Override;

   --------------------------
   -- Normalize_Whitespace --
   --------------------------

   procedure Normalize_Whitespace
     (Whitespace : Schema.Simple_Types.Whitespace_Restriction;
      Val        : in out Unicode.CES.Byte_Sequence;
      Last       : in out Natural)
   is
   begin
      case Whitespace is
         when Preserve =>
            return;  --  Nothing to do

         when Replace =>
            declare
               Idx   : Natural := Val'First;
               First : Natural := Last + 1;
               C     : Unicode_Char;
            begin
               while Idx <= Last loop
                  First := Idx;
                  Encoding.Read (Val, Idx, C);

                  if Is_White_Space (C) then
                     --  Assumes all characters we replace are encoded as
                     --  single byte
                     Val (First) := ' ';
                  end if;
               end loop;

               --  Length of string does not change
            end;

         when Collapse =>
            if Val = "" then
               return;  --  nothing to do
            end if;

            declare
               C       : Unicode_Char;
               Idx, Idx_Output : Natural := Val'First;
               First   : Natural := Last + 1;
               Tmp     : Natural;
               Last_Space : Natural := Last + 1;
               Prev_Is_Whitespace : Boolean := False;
            begin
               --  Remove leading spaces.

               loop
                  First := Idx;
                  Encoding.Read (Val, Idx, C);
                  exit when not Is_White_Space (C);

                  if Idx > Last then
                     Last := 0;
                     return;  --  Empty string
                  end if;
               end loop;

               if First /= Val'First then
                  Val (Val'First .. Last - First + Val'First) :=
                    Val (First .. Last);
                  Last := Last - First + Val'First;
               end if;

               Idx := Val'First;
               Idx_Output := Val'First;

               --  Iterate and replace all whitespaces. Mark the spot of the
               --  last whitespace so that we can ignore trailing spaces.
               --  At the same time, we can copy to Idx_Output, since the
               --  output string will always be at least as short as Val.

               while Idx <= Last loop
                  Tmp := Idx;
                  Encoding.Read (Val, Idx, C);

                  --  Copy, if needed, the character we just read
                  if Is_White_Space (C) then
                     if not Prev_Is_Whitespace then
                        Val (Idx_Output) := ' ';
                        Last_Space := Idx_Output;
                        Idx_Output := Idx_Output + 1;
                        Prev_Is_Whitespace := True;
                     end if;
                  else
                     Val (Idx_Output .. Idx_Output + Idx - Tmp - 1) :=
                       Val (Tmp .. Idx - 1);
                     Idx_Output := Idx_Output + Idx - Tmp;
                     Last_Space := Idx_Output;  --  after this char
                     Prev_Is_Whitespace := False;
                  end if;
               end loop;

               --  Now skip trailing whitespaces if any

               Last := Last_Space - 1;
            end;
      end case;
   end Normalize_Whitespace;

   ----------
   -- Copy --
   ----------

   function Copy (Descr : Simple_Type_Descr) return Simple_Type_Descr is
      Result : Simple_Type_Descr := Descr;
   begin
      if Descr.Pattern /= null then
         Result.Pattern := new Pattern_Matcher_Array (Descr.Pattern'Range);
         for P in Descr.Pattern'Range loop
            Result.Pattern (P) :=
              (Str     => Descr.Pattern (P).Str,
               Pattern => new Pattern_Matcher'(Descr.Pattern (P).Pattern.all));
         end loop;
      end if;
      return Result;
   end Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Arr : in out Pattern_Matcher_Array_Access) is
   begin
      if Arr /= null then
         for A in Arr'Range loop
            Unchecked_Free (Arr (A).Pattern);
         end loop;
         Unchecked_Free (Arr);
      end if;
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Id : Symbol) return Symbol is
   begin
      return Id;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (Symbol_Table : in out Symbol_Htable_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Symbol_Htable.HTable, Symbol_Htable_Access);
   begin
      if Symbol_Table /= null then
         Symbol_Htable.Reset (Symbol_Table.all);
         Unchecked_Free (Symbol_Table);
      end if;
   end Free;

   --------------
   -- Check_Id --
   --------------

   procedure Check_Id
     (Symbols   : Symbol_Table;
      Id_Table  : in out Symbol_Htable_Access;
      Value     : Unicode.CES.Byte_Sequence;
      Error     : in out Symbol)
   is
      Val : constant Symbol := Find (Symbols, Value);
   begin
      if Id_Table = null then
         Id_Table := new Symbol_Htable.HTable (101);
      else
         if Symbol_Htable.Get (Id_Table.all, Val) /= No_Symbol then
            Error := Find (Symbols, "ID """ & Value & """ already defined");
            return;
         end if;
      end if;

      Symbol_Htable.Set (Id_Table.all, Val);
      Error := No_Symbol;
   end Check_Id;

end Schema.Simple_Types;
