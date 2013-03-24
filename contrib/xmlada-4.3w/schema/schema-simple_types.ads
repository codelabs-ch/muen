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

with GNAT.Dynamic_Tables;
with GNAT.Regpat;         use GNAT.Regpat;
with Sax.HTable;
with Sax.Locators;        use Sax.Locators;
with Sax.Symbols;         use Sax.Symbols;
with Sax.Utils;           use Sax.Utils;
with Schema.Decimal;      use Schema.Decimal;
with Schema.Date_Time;    use Schema.Date_Time;
with Unicode.CES;         use Unicode.CES;

package Schema.Simple_Types is

   type Simple_Type_Index is new Natural;
   No_Simple_Type_Index : constant Simple_Type_Index := 0;

   type Enumeration_Index is new Natural;
   No_Enumeration_Index : constant Enumeration_Index := 0;

   Max_Types_In_Union : constant := 9;
   --  Maximum number of types in a union.
   --  This is hard-coded to avoid memory allocations as much as possible.
   --  This value is chosen so that the case [Primitive_Union] does not make
   --  [Simple_Type_Descr] bigger than the other cases.

   type Whitespace_Restriction is (Preserve, Replace, Collapse);

   function Convert_Regexp
     (Regexp : Unicode.CES.Byte_Sequence) return String;
   --  Return a regular expresssion that converts the XML-specification
   --  regexp Regexp to a GNAT.Regpat compatible one

   type Primitive_Simple_Type_Kind is
     (Primitive_Boolean, Primitive_Double, Primitive_Decimal,
      Primitive_Float,

      Primitive_String, Primitive_Any_URI, Primitive_QName, Primitive_ID,
      Primitive_Notation, Primitive_NMTOKEN, Primitive_Language,
      Primitive_NMTOKENS, Primitive_Name, Primitive_NCName, Primitive_NCNames,
      Primitive_Base64Binary, Primitive_HexBinary,

      Primitive_Time, Primitive_DateTime, Primitive_GDay, Primitive_GMonthDay,
      Primitive_GMonth, Primitive_GYearMonth, Primitive_GYear, Primitive_Date,
      Primitive_Duration,

      Primitive_Union, Primitive_List
     );

   type Pattern_Matcher_Access is access GNAT.Regpat.Pattern_Matcher;
   type Pattern_Facet is record
      Str     : Sax.Symbols.Symbol;      --  The pattern itself
      Pattern : Pattern_Matcher_Access;  --  The compiled pattern
   end record;
   type Pattern_Matcher_Array is array (Natural range <>) of Pattern_Facet;
   type Pattern_Matcher_Array_Access is access all Pattern_Matcher_Array;
   procedure Free (Arr : in out Pattern_Matcher_Array_Access);
   --  A type might be subject to multiple patterns:
   --    - When we extend a base type, we must match either the base's patterns
   --      or the patterns set in the extenstion. This does not increase the
   --      number of patterns, we just merge them with "|".
   --    - When we restrict a base type, we must match both the base's patterns
   --      and the patterns set in the extenstion. This increases the number of
   --      patterns

   type Simple_Type_Array is array (Natural range <>) of Simple_Type_Index;

   type Facet_Enum is (Facet_Whitespace,
                       Facet_Enumeration,
                       Facet_Pattern,
                       Facet_Min_Inclusive,
                       Facet_Max_Inclusive,
                       Facet_Min_Exclusive,
                       Facet_Max_Exclusive,
                       Facet_Length,
                       Facet_Min_Length,
                       Facet_Max_Length,
                       Facet_Total_Digits,
                       Facet_Fraction_Digits);
   type Facets_Mask is array (Facet_Enum) of Boolean;

   type Simple_Type_Descr
     (Kind : Primitive_Simple_Type_Kind := Primitive_Boolean)
   is record
      Mask           : Facets_Mask            := (others => False);
      Pattern        : Pattern_Matcher_Array_Access := null;
      Whitespace     : Whitespace_Restriction := Collapse;
      Enumeration    : Enumeration_Index      := No_Enumeration_Index;

      case Kind is
         when Primitive_Union =>
            Union : Simple_Type_Array (1 .. Max_Types_In_Union) :=
              (others => No_Simple_Type_Index);

         when Primitive_List =>
            List_Item       : Simple_Type_Index;
            List_Length     : Natural := Natural'Last;
            List_Min_Length : Natural := 0;
            List_Max_Length : Natural := Natural'Last;

         when Primitive_String .. Primitive_HexBinary =>
            String_Length      : Natural := Natural'Last;
            String_Min_Length  : Natural := 0;
            String_Max_Length  : Natural := Natural'Last;

         when Primitive_Boolean =>
            null;

         when Primitive_Float | Primitive_Double  =>  --  float, double
            Float_Min_Inclusive : XML_Float := Unknown_Float;
            Float_Max_Inclusive : XML_Float := Unknown_Float;
            Float_Min_Exclusive : XML_Float := Unknown_Float;
            Float_Max_Exclusive : XML_Float := Unknown_Float;

         when Primitive_Decimal =>  --  decimal
            Total_Digits          : Positive := Positive'Last;
            Fraction_Digits       : Natural  := Natural'Last;
            Decimal_Min_Inclusive, Decimal_Max_Inclusive,
            Decimal_Min_Exclusive, Decimal_Max_Exclusive :
            Arbitrary_Precision_Number := Undefined_Number;

         when Primitive_Time =>
            Time_Min_Inclusive, Time_Min_Exclusive,
            Time_Max_Inclusive, Time_Max_Exclusive  : Time_T := No_Time_T;

         when Primitive_DateTime =>
            DateTime_Min_Inclusive, DateTime_Min_Exclusive,
            DateTime_Max_Inclusive, DateTime_Max_Exclusive  : Date_Time_T :=
              No_Date_Time;

         when Primitive_GDay =>
            GDay_Min_Inclusive, GDay_Min_Exclusive,
            GDay_Max_Inclusive, GDay_Max_Exclusive  : GDay_T := No_GDay;

         when Primitive_GMonthDay =>
            GMonthDay_Min_Inclusive, GMonthDay_Min_Exclusive,
            GMonthDay_Max_Inclusive, GMonthDay_Max_Exclusive : GMonth_Day_T
              := No_Month_Day;

         when Primitive_GMonth =>
            GMonth_Min_Inclusive, GMonth_Min_Exclusive,
            GMonth_Max_Inclusive, GMonth_Max_Exclusive  : GMonth_T :=
              No_Month;

         when Primitive_GYearMonth =>
            GYearMonth_Min_Inclusive, GYearMonth_Min_Exclusive,
            GYearMonth_Max_Inclusive, GYearMonth_Max_Exclusive :
              GYear_Month_T := No_Year_Month;

         when Primitive_GYear =>
            GYear_Min_Inclusive, GYear_Min_Exclusive,
            GYear_Max_Inclusive, GYear_Max_Exclusive  : GYear_T := No_Year;

         when Primitive_Date =>
            Date_Min_Inclusive, Date_Min_Exclusive,
            Date_Max_Inclusive, Date_Max_Exclusive  : Date_T := No_Date_T;

         when Primitive_Duration =>
            Duration_Min_Inclusive, Duration_Min_Exclusive,
            Duration_Max_Inclusive, Duration_Max_Exclusive  : Duration_T :=
              No_Duration;
      end case;
   end record;

   Any_Simple_Type : constant Simple_Type_Descr :=
     (Kind => Primitive_String, Whitespace => Preserve, others => <>);

   function Copy (Descr : Simple_Type_Descr) return Simple_Type_Descr;
   --  return a deep copy of [Copy] (duplicates the pattern)

   package Simple_Type_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Simple_Type_Descr,
      Table_Index_Type     => Simple_Type_Index,
      Table_Low_Bound      => No_Simple_Type_Index + 1,
      Table_Initial        => 100,
      Table_Increment      => 100);

   subtype Simple_Type_Table is Simple_Type_Tables.Instance;

   type Enumeration_Descr is record
      Value : Sax.Symbols.Symbol;
      Next  : Enumeration_Index := No_Enumeration_Index;
   end record;

   package Enumeration_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Enumeration_Descr,
      Table_Index_Type     => Enumeration_Index,
      Table_Low_Bound      => No_Enumeration_Index + 1,
      Table_Initial        => 30,
      Table_Increment      => 20);

   generic
      type Type_Index is private;
      No_Type_Index : Type_Index;
      with function Register
        (Local          : Byte_Sequence;
         Descr          : Simple_Type_Descr;
         Restriction_Of : Type_Index) return Type_Index;
   procedure Register_Predefined_Types (Symbols : Sax.Utils.Symbol_Table);
   --  Register all the predefined types

   function Get_Key (Id : Sax.Symbols.Symbol) return Sax.Symbols.Symbol;
   package Symbol_Htable is new Sax.HTable
     (Element       => Sax.Symbols.Symbol,
      Empty_Element => Sax.Symbols.No_Symbol,
      Key           => Sax.Symbols.Symbol,
      Get_Key       => Get_Key,
      Hash          => Sax.Symbols.Hash,
      Equal         => Sax.Symbols."=");
   type Symbol_Htable_Access is access Symbol_Htable.HTable;
   --  This table is used to store the list of IDs that have been used in the
   --  document so far, and prevent their duplication in the document.

   procedure Free (Symbol_Table : in out Symbol_Htable_Access);

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
      XML_Version   : XML_Versions);
   --  Validate [Ch] for the simple type [Simple_Type].
   --  Returns an error message in case of error, or No_Symbol otherwise.
   --  If [Insert_Id] is True and you are validating an ID, it will be inserted
   --  in Id_Table (and an error reported if it already exists)

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
      XML_Version   : XML_Versions);
   --  Checks whether [Ch1]=[Ch2] according to the type.
   --  (This involves for instance normalizing whitespaces)

   type Facet_Value is record
      Value : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Enum  : Enumeration_Index := No_Enumeration_Index;
      Loc   : Sax.Locators.Location;
   end record;
   No_Facet_Value : constant Facet_Value := (Sax.Symbols.No_Symbol,
                                             No_Enumeration_Index,
                                             Sax.Locators.No_Location);

   type All_Facets is array (Facet_Enum) of Facet_Value;
   No_Facets : constant All_Facets := (others => No_Facet_Value);
   --  A temporary record to hold facets defined in a schema, until we can
   --  merge them with the base's facets. It does not try to interpret the
   --  facets.

   procedure Add_Facet
     (Facets       : in out All_Facets;
      Symbols      : Sax.Utils.Symbol_Table;
      Enumerations : in out Enumeration_Tables.Instance;
      Facet_Name   : Sax.Symbols.Symbol;
      Value        : Sax.Symbols.Symbol;
      Loc          : Sax.Locators.Location);
   --  Set a specific facet in [Simple]

   procedure Override
     (Simple     : in out Simple_Type_Descr;
      Facets     : All_Facets;
      Symbols    : Sax.Utils.Symbol_Table;
      As_Restriction : Boolean;
      Error      : out Sax.Symbols.Symbol;
      Error_Loc  : out Sax.Locators.Location);
   --  Override [Simple] with the facets defined in [Facets], but keep those
   --  facets that are not defined. Sets [Error] to a symbol if one of the
   --  facets is invalid for [Simple].

   procedure Normalize_Whitespace
     (Whitespace : Schema.Simple_Types.Whitespace_Restriction;
      Val        : in out Unicode.CES.Byte_Sequence;
      Last       : in out Natural);
   --  Normalize in place the whitespaces in [Val (1 .. Last)], and change
   --  [Last] as appropriate (always smaller or equal to the input parameter)

end Schema.Simple_Types;
