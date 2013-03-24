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

pragma Ada_05;

with Ada.Characters.Handling;        use Ada.Characters.Handling;
with Ada.Exceptions;                 use Ada.Exceptions;
with Ada.Strings.Unbounded;          use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Task_Lock;
with Interfaces;                     use Interfaces;
with Sax.Attributes;                 use Sax.Attributes;
with Sax.Locators;                   use Sax.Locators;
with Sax.Symbols;                    use Sax.Symbols;
with Sax.Utils;                      use Sax.Utils;
with Schema.Simple_Types;            use Schema.Simple_Types;
with Schema.Validators.XSD_Grammar;  use Schema.Validators.XSD_Grammar;
with Unicode.CES;                    use Unicode.CES;
with Unicode;                        use Unicode;

package body Schema.Validators is
   use XML_Grammars, Attributes_Tables, Enumeration_Tables;
   use Schema_State_Machines_Matchers;

   function To_Graphic_String (Str : Byte_Sequence) return String;
   --  Convert non-graphic characters in Str to make them visible in a display

   type Attribute_Validator_Data is record
      Validator : Named_Attribute_List;  --  Index into the table
      Visited   : Boolean;
   end record;
   type Attribute_Validator_Index is new Natural;
   type Attribute_Validator_Array is array (Attribute_Validator_Index range <>)
     of Attribute_Validator_Data;
   function To_Attribute_Array
     (NFA        : access Schema_NFA'Class;
      Attributes : Attributes_List) return Attribute_Validator_Array;
   --  The data required to validate attributes

   procedure Create_Grammar_If_Needed
     (Grammar : in out XML_Grammar;
      Symbols : Symbol_Table := No_Symbol_Table);
   --  Create the grammar if needed
   --  Symbols is used only when a new grammar is created.

   procedure Validate_Attribute
     (Attr      : Attribute_Descr;
      Reader    : access Abstract_Validation_Reader'Class;
      Atts      : in out Sax_Attribute_List;
      Index     : Natural);
   --  Validate the value of a single attribute

   procedure Reset_Simple_Types
     (NFA : access Schema_NFA'Class;
      To  : Simple_Type_Index := No_Simple_Type_Index);
   --  Resets the contents of G.Simple_Types by resizing the table and freeing
   --  needed data
   --  If [To] is [No_Simple_Type_Index], the table is freed

   function To_String (Any : Any_Descr) return String;
   --  Debug only

   ---------------
   -- To_String --
   ---------------

   function To_String (Any : Any_Descr) return String is
      Str : Unbounded_String;
   begin
      Append (Str, "{" & Any.Process_Contents'Img);
      if Any.Namespaces /= No_Symbol then
         Append (Str, " ns={" & Get (Any.Namespaces).all & "}");
      end if;
      if Any.No_Namespaces /= No_Symbol then
         Append (Str, " no_ns={" & Get (Any.No_Namespaces).all & "}");
      end if;
      return To_String (Str) & "}";
   end To_String;

   ----------------------
   -- Validation_Error --
   ----------------------

   procedure Validation_Error
     (Reader  : access Abstract_Validation_Reader;
      Message : Byte_Sequence;
      Loc     : Sax.Locators.Location := Sax.Locators.No_Location;
      Except  : Exception_Id := XML_Validation_Error'Identity) is
   begin
      if Debug then
         Debug_Output ("Validation_Error: " & Message);
      end if;

      if Loc /= No_Location then
         Reader.Error_Location := Loc;
      else
         Reader.Error_Location := Reader.Current_Location;
      end if;

      if Message (Message'First) = '#' then
         Reader.Error_Msg := Find_Symbol
           (Reader.all, Message (Message'First + 1 .. Message'Last));
         raise XML_Not_Implemented;
      else
         Reader.Error_Msg := Find_Symbol (Reader.all, Message);
         Raise_Exception (Except);
      end if;
   end Validation_Error;

   -----------------------
   -- Get_Error_Message --
   -----------------------

   function Get_Error_Message
     (Reader : Abstract_Validation_Reader) return Unicode.CES.Byte_Sequence
   is
      Loc : Location;
   begin
      if Reader.Error_Msg = No_Symbol then
         return "";

      else
         Loc := Reader.Error_Location;
         if Loc = No_Location then
            Loc := Reader.Current_Location;
         end if;

         declare
            Error : constant Cst_Byte_Sequence_Access :=
              Get (Reader.Error_Msg);
         begin
            if Loc /= No_Location then
               return To_String (Loc, Use_Basename_In_Error_Messages (Reader))
                 & ": " & Error.all;
            else
               return Error.all;
            end if;
         end;
      end if;
   end Get_Error_Message;

   -----------------------
   -- Add_Any_Attribute --
   -----------------------

   procedure Add_Any_Attribute
     (Grammar        : XML_Grammar;
      List           : in out Attributes_List;
      Any            : Internal_Any_Descr;
      As_Restriction : Boolean) is
   begin
      List.Any := Combine
        (Grammar        => Grammar,
         Base_Any       => List.Any,
         Local_Process  => Any.Process_Contents,
         Local          => Any.Namespaces,
         As_Restriction => As_Restriction,
         Target_NS      => Any.Target_NS);
   end Add_Any_Attribute;

   ----------------------------
   -- Normalize_And_Validate --
   ----------------------------

   procedure Normalize_And_Validate
     (Parser  : access Abstract_Validation_Reader'Class;
      Simple  : Schema.Simple_Types.Simple_Type_Index;
      Fixed   : in out Sax.Symbols.Symbol;
      Loc     : Sax.Locators.Location)
   is
   begin
      if Fixed /= No_Symbol
        and then Simple /= No_Simple_Type_Index
      then
         declare
            Simple_Descr : constant Simple_Type_Descr :=
                             Get_NFA (Parser.Grammar).Get_Simple_Type (Simple);
            Norm         : Byte_Sequence := Get (Fixed).all;
            Last         : Integer := Norm'Last;
         begin
            --  Normalize whitespaces, for faster comparison later
            --  on.

            if Simple_Descr.Mask (Facet_Whitespace) then
               Normalize_Whitespace
                 (Simple_Descr.Whitespace, Norm, Last);
               Fixed := Find
                 (Get_Symbol_Table (Parser.Grammar),
                  Norm (Norm'First .. Last));
            end if;

            Validate_Simple_Type
              (Reader      => Parser,
               Simple_Type => Simple,
               Ch          => Norm (Norm'First .. Last),
               Loc         => Loc,
               Insert_Id   => True);
         end;
      end if;
   end Normalize_And_Validate;

   -------------------
   -- Add_Attribute --
   -------------------

   procedure Add_Attribute
     (Parser    : access Abstract_Validation_Reader'Class;
      List      : in out Attributes_List;
      Attribute : Attribute_Descr;
      Ref       : Named_Attribute_List := Empty_Named_Attribute_List;
      Loc       : Sax.Locators.Location)
   is
      NFA  : constant Schema_NFA_Access := Get_NFA (Parser.Grammar);
      L    : Named_Attribute_List := List.Named;
      Tmp  : Named_Attribute_List;
      Attr : Attribute_Descr := Attribute;
   begin
      if Debug then
         Debug_Output
           ("Adding attribute " & To_QName (Attribute.Name)
            & " Use_Type=" & Attribute.Use_Type'Img
            & " local=" & Attribute.Is_Local'Img);
      end if;

      while L /= Empty_Named_Attribute_List loop
         if NFA.Attributes.Table (L).Name = Attribute.Name then
            --  Override use_type, form,... from the <restriction>
            Tmp := NFA.Attributes.Table (L).Next;

            Attr := Attribute;
            Normalize_And_Validate
              (Parser,
               Attr.Simple_Type, Attr.Fixed, Loc);

            NFA.Attributes.Table (L) := Attr;
            NFA.Attributes.Table (L).Next := Tmp;

            return;
         end if;
         L := NFA.Attributes.Table (L).Next;
      end loop;

      if Ref /= Empty_Named_Attribute_List then
         Attr          := NFA.Attributes.Table (Ref);
         Attr.Use_Type := Attribute.Use_Type;
         Attr.Is_Local := Attribute.Is_Local;

         if Attribute.Fixed /= No_Symbol then
            Attr.Fixed    := Attribute.Fixed;
         end if;
      end if;

      Normalize_And_Validate
        (Parser,
         Attr.Simple_Type, Attr.Fixed, Loc);

      Append (NFA.Attributes, Attr);
      NFA.Attributes.Table (Last (NFA.Attributes)).Next := List.Named;
      List.Named := Last (NFA.Attributes);
   end Add_Attribute;

   --------------------
   -- Add_Attributes --
   --------------------

   procedure Add_Attributes
     (Parser         : access Abstract_Validation_Reader'Class;
      List           : in out Attributes_List;
      Attributes     : Attributes_List;
      As_Restriction : Boolean;
      Loc            : Sax.Locators.Location)
   is
      NFA : constant Schema_NFA_Access := Get_NFA (Parser.Grammar);
      L   : Named_Attribute_List := Attributes.Named;
   begin
      while L /= Empty_Named_Attribute_List loop
         Add_Attribute (Parser, List, NFA.Attributes.Table (L), Loc => Loc);
         L := NFA.Attributes.Table (L).Next;
      end loop;

      Add_Any_Attribute
        (Parser.Grammar, List,
         Internal_Any_Descr'
           (Target_NS        => Empty_String,
            Process_Contents => Attributes.Any.Process_Contents,
            Namespaces       => Attributes.Any.Namespaces),
         As_Restriction);
   end Add_Attributes;

   ------------------------
   -- To_Attribute_Array --
   ------------------------

   function To_Attribute_Array
     (NFA        : access Schema_NFA'Class;
      Attributes : Attributes_List) return Attribute_Validator_Array
   is
      Count : Attribute_Validator_Index := 0;
      L     : Named_Attribute_List := Attributes.Named;
   begin
      while L /= Empty_Named_Attribute_List loop
         Count := Count + 1;
         L := NFA.Attributes.Table (L).Next;
      end loop;

      declare
         Result : Attribute_Validator_Array (1 .. Count);
      begin
         Count := Result'First;
         L := Attributes.Named;
         while L /= Empty_Named_Attribute_List loop
            Result (Count) := (Validator => L,
                               Visited   => False);
            Count := Count + 1;
            L := NFA.Attributes.Table (L).Next;
         end loop;

         return Result;
      end;
   end To_Attribute_Array;

   -------------
   -- Combine --
   -------------

   function Combine
     (Grammar        : XML_Grammar;
      Base_Any       : Any_Descr;
      Local_Process  : Process_Contents_Type;
      Local          : Sax.Symbols.Symbol;
      As_Restriction : Boolean;
      Target_NS      : Sax.Symbols.Symbol) return Any_Descr
   is
      use Symbol_Htable;
      Namespaces    : Symbol_Htable.HTable (127);
      No_Namespaces : Symbol_Htable.HTable (127);
      Tmp           : Symbol_Htable.HTable (127);

      Result : Any_Descr;
      Base_Is_Any, Local_Is_Any : Boolean;

      Symbols : constant Symbol_Table := Get (Grammar).Symbols;

      procedure Callback (Str : Byte_Sequence);
      procedure Add_To_Table
        (Sym : Symbol; Table : in out Symbol_Htable.HTable);

      procedure Merge (Sym : Symbol);
      --  Take all namespaces in [Sym], and copy, in [Namespaces], those that
      --  are also in [Tmp], but not in [No_Namespaces]

      function To_Symbol (Table : Symbol_Htable.HTable) return Symbol;
      --  Return the list of strings in Table

      -----------
      -- Merge --
      -----------

      procedure Merge (Sym : Symbol) is

         procedure Callback (Str : Byte_Sequence);
         procedure Do_Merge (S : Symbol);

         --------------
         -- Do_Merge --
         --------------

         procedure Do_Merge (S : Symbol) is
         begin
            if Base_Is_Any
              or else ((Base_Any.Namespaces = No_Symbol
                        or else Get (Tmp, S) /= No_Symbol)
                       and then Get (No_Namespaces, S) = No_Symbol)
            then
               Set (Namespaces, S);
            end if;
         end Do_Merge;

         --------------
         -- Callback --
         --------------

         procedure Callback (Str : Byte_Sequence) is
         begin
            if Str = "##targetNamespace" then
               Do_Merge (Target_NS);

            elsif Str = "##other" then
               if Target_NS /= No_Symbol then
                  Set (No_Namespaces, Target_NS);
               end if;

               Set (No_Namespaces, Find (Symbols, "##local"));

            else
               Do_Merge (Find (Symbols, Str));  --  including ##any, ##local
            end if;
         end Callback;

         procedure All_Add is new For_Each_Item (Callback);
      begin
         if Sym /= No_Symbol then
            All_Add (Get (Sym).all);
         end if;
      end Merge;

      ------------------
      -- Add_To_Table --
      ------------------

      procedure Add_To_Table
        (Sym : Symbol; Table : in out Symbol_Htable.HTable)
      is
         procedure Callback (Str : Byte_Sequence);

         --------------
         -- Callback --
         --------------

         procedure Callback (Str : Byte_Sequence) is
         begin
            Set (Table, Find (Symbols, Str));
         end Callback;

         procedure All_Add is new For_Each_Item (Callback);
      begin
         if Sym /= No_Symbol then
            All_Add (Get (Sym).all);
         end if;
      end Add_To_Table;

      ---------------
      -- To_Symbol --
      ---------------

      function To_Symbol (Table : Symbol_Htable.HTable) return Symbol is
         Str  : Unbounded_String;
         S    : Symbol;
         Iter : Iterator := Symbol_Htable.First (Table);
      begin
         if Iter = No_Iterator then
            return No_Symbol;
         end if;

         while Iter /= No_Iterator loop
            S := Current (Iter);

            if Str = Null_Unbounded_String then
               Append (Str, Get (S).all);
            else
               Append (Str, " " & Get (S).all);
            end if;

            Symbol_Htable.Next (Table, Iter);
         end loop;

         return Find (Get (Grammar).Symbols, To_String (Str));
      end To_Symbol;

      --------------
      -- Callback --
      --------------

      procedure Callback (Str : Byte_Sequence) is
      begin
         if Str = "##targetNamespace" then
            if Target_NS = Empty_String then
               Set (Namespaces, Find (Symbols, "##local"));
            else
               Set (Namespaces, Target_NS);
            end if;

         elsif Str = "##other" then
            if Target_NS /= No_Symbol then
               Set (No_Namespaces, Target_NS);
            end if;

            Set (No_Namespaces, Find (Symbols, "##local"));
         else
            Set (Namespaces, Find (Symbols, Str)); --  including ##any, ##local
         end if;
      end Callback;

      procedure All_Items is new For_Each_Item (Callback);

   begin
      if Base_Any = No_Any_Descr then
         if Local /= No_Symbol then
            All_Items (Get (Local).all);
         end if;

         declare
            Result : constant Any_Descr := Any_Descr'
              (Process_Contents => Local_Process,
               No_Namespaces    => To_Symbol (No_Namespaces),
               Namespaces       => To_Symbol (Namespaces));
         begin
            Reset (Namespaces);
            Reset (No_Namespaces);
            Reset (Tmp);
            return Result;
         end;
      end if;

      Local_Is_Any := Local /= No_Symbol and then Get (Local).all = "##any";
      Base_Is_Any := Base_Any.Namespaces /= No_Symbol
        and then Get (Base_Any.Namespaces).all = "##any";

      if As_Restriction then
         --  The list of "Namespaces" is the intersection of the two (and
         --  empty if local is empty)
         --  From this, remove the list of the base's "No_Namespaces".
         --  We preserve those "No_Namespaces" into the new type, though.

         Add_To_Table (Base_Any.No_Namespaces, No_Namespaces);

         if Local_Is_Any then
            if Base_Any.Namespaces /= No_Symbol then
               Add_To_Table (Base_Any.Namespaces, Namespaces);
            elsif Local /= No_Symbol then
               Add_To_Table (Local, Namespaces);
            end if;
         else
            Add_To_Table (Base_Any.Namespaces, Tmp);
            Merge (Local);
         end if;

      else
         --  If the base type or the extension contains ##any, we will still
         --  accept any namespace

         if Base_Is_Any then
            Add_To_Table (Base_Any.Namespaces, Namespaces); --  ##any

         elsif Local_Is_Any then
            if Base_Any.No_Namespaces /= No_Symbol then
               Add_To_Table (Base_Any.No_Namespaces, No_Namespaces);
            elsif Local /= No_Symbol then
               Add_To_Table (Local, Namespaces);  --  ##any
            end if;

         else
            --  None of the two is ##any, so we just combine them. Since we
            --  have an extension, the attributes will have to match any of
            --  the namespaces.

            Add_To_Table (Base_Any.Namespaces, Namespaces);
            Add_To_Table (Base_Any.No_Namespaces, No_Namespaces);

            if Local /= No_Symbol then
               All_Items (Get (Local).all);
            end if;
         end if;
      end if;

      Result.Process_Contents := Local_Process;
      Result.Namespaces       := To_Symbol (Namespaces);
      Result.No_Namespaces    := To_Symbol (No_Namespaces);

      --  ??? If .Namespaces contain one common NS with .No_Namespaces, then
      --  we really have a ##any

      if Debug then
         if Local /= No_Symbol then
            Debug_Output ("Combine " & To_String (Base_Any)
                          & " and {" & Local_Process'Img
                          & " " & Get (Local).all & " target="
                          & Get (Target_NS).all & "} restr="
                          & As_Restriction'Img & " => "
                          & To_String (Result));
         else
            Debug_Output ("Combine " & To_String (Base_Any)
                          & " and {" & Local_Process'Img & " target="
                          & Get (Target_NS).all & "} restr="
                          & As_Restriction'Img & " => "
                          & To_String (Result));
         end if;
      end if;

      Reset (Namespaces);
      Reset (No_Namespaces);
      Reset (Tmp);

      return Result;

   exception
      when others =>
         Reset (Namespaces);
         Reset (No_Namespaces);
         Reset (Tmp);
         raise;
   end Combine;

   ---------------
   -- Match_Any --
   ---------------

   function Match_Any
     (Any : Any_Descr; Name : Qualified_Name) return Boolean
   is
      Matches       : Boolean := False;
      Invalid_No_NS : Boolean := False;

      procedure Callback (Str : Byte_Sequence);
      procedure Negate_Callback (Str : Byte_Sequence);

      ---------------------
      -- Negate_Callback --
      ---------------------

      procedure Negate_Callback (Str : Byte_Sequence) is
      begin
         if Str = "##local" then
            Invalid_No_NS := Invalid_No_NS or else Name.NS = Empty_String;
         else
            Invalid_No_NS := Invalid_No_NS or else Get (Name.NS).all = Str;
         end if;
      end Negate_Callback;

      --------------
      -- Callback --
      --------------

      procedure Callback (Str : Byte_Sequence) is
      begin
         if Matches then
            null;
         elsif Str = "##local" then
            Matches := Name.NS = Empty_String;
         else
            Matches := Get (Name.NS).all = Str;
         end if;
      end Callback;

      procedure All_Items is new For_Each_Item (Callback);
      procedure Negate_All_Items is new For_Each_Item (Negate_Callback);

   begin
      if Debug then
         Debug_Output
           ("match <any>: " & To_String (Any) & " and " & To_QName (Name));
      end if;

      if Any.Namespaces /= No_Symbol
        and then Get (Any.Namespaces).all = "##any"
      then
         return True;
      end if;

      if Any.Namespaces /= No_Symbol then
         All_Items (Get (Any.Namespaces).all);
      end if;

      if Any.No_Namespaces /= No_Symbol then
         Negate_All_Items (Get (Any.No_Namespaces).all);
      end if;

      if Debug then
         Debug_Output ("Matches=" & Matches'Img
                       & " Invalid_No_NS=" & Invalid_No_NS'Img);
      end if;

      if Any.Namespaces /= No_Symbol
        and then Any.No_Namespaces /= No_Symbol
      then
         return Matches or else not Invalid_No_NS;
      elsif Any.Namespaces /= No_Symbol then
         return Matches;
      elsif Any.No_Namespaces /= No_Symbol then
         return not Invalid_No_NS;
      else
         return False;
      end if;
   end Match_Any;

   -------------------------
   -- Validate_Attributes --
   -------------------------

   procedure Validate_Attributes
     (NFA    : access Schema_NFA'Class;
      Typ    : access Type_Descr;
      Reader : access Abstract_Validation_Reader'Class;
      Atts   : in out Sax.Readers.Sax_Attribute_List;
      Is_Nil : in out Integer)
   is
      Length      : constant Natural := Get_Length (Atts);
      Valid_Attrs : Attribute_Validator_Array :=
                      To_Attribute_Array (NFA, Typ.Attributes);

      type Attr_Status is record
         Prohibited : Boolean := False;
         --  Prohibited explicitly, but it might be allowed through
         --  <anyAttribute>

         Seen  : Boolean := False;
      end record;
      Seen : array (1 .. Length) of Attr_Status := (others => (False, False));

      function Find_Attribute (Attr : Attribute_Descr) return Integer;
      --  Chech whether Named appears in Atts

      procedure Check_Named_Attribute (Index : Attribute_Validator_Index);
      --  Check a named attribute or a wildcard attribute

      procedure Check_Single_ID;
      --  If using XSD 1.0, check that there is a single ID attribute.
      --  This relies on the Sax.Attributes.Get_Type being set correctly.
      --  XSD 1.0 prevents having two such attributes, for easier conversion
      --  to DTD (see G.1.7 ID, IDREF, and related types)

      ---------------------
      -- Check_Single_ID --
      ---------------------

      procedure Check_Single_ID is
         Seen_ID : Boolean := False;
      begin
         for A in 1 .. Length loop
            if Get_Type (Atts, A) = Sax.Attributes.Id then
               if Seen_ID then
                  Validation_Error
                    (Reader,
                     "Elements can have a single ID attribute in XSD 1.0");
               end if;

               Seen_ID := True;
            end if;
         end loop;
      end Check_Single_ID;

      ---------------------------
      -- Check_Named_Attribute --
      ---------------------------

      procedure Check_Named_Attribute (Index : Attribute_Validator_Index) is
         Found  : Integer;
         Attr   : Attribute_Descr
           renames NFA.Attributes.Table (Valid_Attrs (Index).Validator);
      begin
         if not Valid_Attrs (Index).Visited then
            if Debug then
               Debug_Output
                 ("Checking attribute: "
                  & To_QName
                    (NFA.Attributes.Table
                       (Valid_Attrs (Index).Validator).Name)
                  & " " & Attr.Use_Type'Img & " " & Attr.Form'Img);
            end if;

            Valid_Attrs (Index).Visited := True;
            Found := Find_Attribute (Attr);

            if Found = -1 then
               case Attr.Use_Type is
                  when Required =>
                     Validation_Error
                       (Reader, "Attribute """
                        & To_QName (Attr.Name)
                        & """ is required in this context");
                  when Prohibited | Optional | Default =>
                     null;
               end case;

            else
               Seen (Found).Seen := True;

               case Attr.Form is
                  when Qualified =>
                     if Attr.Is_Local
                       and then Get_Prefix (Atts, Found) = Empty_String
                     then
                        Validation_Error
                          (Reader, "Attribute " & Get_Qname (Atts, Found)
                           & " must have a namespace");
                     end if;

                  when Unqualified =>
                     if Attr.Is_Local
                       and then Get_Prefix (Atts, Found) /= Empty_String
                     then
                        Validation_Error
                          (Reader, "Attribute " & Get_Qname (Atts, Found)
                           & " must not have a namespace");
                     end if;
               end case;

               case Attr.Use_Type is
                  when Prohibited =>
                     if Debug then
                        Debug_Output
                          ("Marking as prohibited, might be accepted by"
                           & " <anyAttribute>");
                     end if;
                     Seen (Found) := (Seen       => False,
                                      Prohibited => True);

                  when Optional | Required | Default =>
                     --  We do not need to check id here, since that is
                     --  automatically checked from Validate_Characters for the
                     --  attribute
                     Validate_Attribute (Attr, Reader, Atts, Found);
               end case;
            end if;
         end if;
      end Check_Named_Attribute;

      --------------------
      -- Find_Attribute --
      --------------------

      function Find_Attribute (Attr : Attribute_Descr) return Integer is
         Is_Local : constant Boolean := Attr.Is_Local;
         Matches  : Boolean;
      begin
         for A in 1 .. Length loop
            if not Seen (A).Seen
              and then Get_Name (Atts, A).Local = Attr.Name.Local
            then
               Matches := (Is_Local and Get_Prefix (Atts, A) = Empty_String)
                 or else Get_Name (Atts, A).NS = Attr.Name.NS;

               if Matches then
                  if Debug then
                     Debug_Output ("Found attribute: "
                                   & To_QName (Get_Name (Atts, A))
                                   & " prefix="
                                   & Get (Get_Prefix (Atts, A)).all
                                   & " at index" & A'Img
                                   & " Is_Local=" & Is_Local'Img
                                   & " Form=" & Attr.Form'Img);
                  end if;
                  return A;
               end if;
            end if;
         end loop;
         return -1;
      end Find_Attribute;

   begin
      --  All the xsi:* attributes should be valid, whatever the schema

      for S in Seen'Range loop
         if Get_Name (Atts, S).NS = Reader.XML_Instance_URI then
            if Get_Name (Atts, S).Local = Reader.Nil then
               Is_Nil := S;
               Seen (S).Seen := True;

               --  Following attributes are always valid
               --  See "Element Locally Valid (Complex Type)" 3.4.4.2
            elsif Get_Name (Atts, S).Local = Reader.Typ
              or else Get_Name (Atts, S).Local = Reader.Schema_Location
              or else Get_Name (Atts, S).Local =
                        Reader.No_Namespace_Schema_Location
            then
               Seen (S).Seen := True;
            end if;
         end if;
      end loop;

      for L in Valid_Attrs'Range loop
         Check_Named_Attribute (L);
      end loop;

      declare
         TRef : Global_Reference;
      begin
         for S in Seen'Range loop
            if not Seen (S).Seen then
               Seen (S).Seen := Match_Any
                 (Typ.Attributes.Any, Get_Name (Atts, S));

               if not Seen (S).Seen then
                  if Seen (S).Prohibited then
                     Validation_Error
                       (Reader, "Attribute """ & Get_Qname (Atts, S)
                        & """ is prohibited in this context "
                          & To_QName (Typ.Name));
                  elsif Typ.Attributes.Any = No_Any_Descr then
                     Validation_Error
                       (Reader, "Attribute """ & Get_Qname (Atts, S)
                        & """ invalid for type " & To_QName (Typ.Name));
                  else
                     Validation_Error
                       (Reader, "Attribute """ & Get_Qname (Atts, S)
                        & """ does not match attribute wildcard");
                  end if;
               end if;

               --  If the processing content forces it, we must check that
               --  there is indeed a valid definition for this attribute.

               case Typ.Attributes.Any.Process_Contents is
                  when Process_Skip =>
                     null;  --  Always OK
                     TRef := No_Global_Reference;

                  when Process_Lax =>
                     TRef := Reference_HTables.Get
                       (NFA.References.all,
                        (Get_Name (Atts, S), Ref_Attribute));

                  when Process_Strict =>
                     TRef := Reference_HTables.Get
                       (NFA.References.all,
                        (Get_Name (Atts, S), Ref_Attribute));
                     if TRef = No_Global_Reference then
                        Validation_Error
                          (Reader, "No definition found for """
                           & Get_Qname (Atts, S) & """");
                     end if;
               end case;

               if TRef /= No_Global_Reference then
                  Validate_Attribute
                    (NFA.Attributes.Table (TRef.Attributes.Named),
                     Reader, Atts, S);
               end if;

               Seen (S).Prohibited := False;
            end if;
         end loop;
      end;

      Check_Single_ID;
   end Validate_Attributes;

   -----------------------
   -- To_Graphic_String --
   -----------------------

   function To_Graphic_String (Str : Byte_Sequence) return String is
      To_Hex : constant array (0 .. 15) of Character :=
                 ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                  'A', 'B', 'C', 'D', 'E', 'F');
      Result : String (1 .. 4 * Str'Length);
      Index  : Integer := Result'First;
   begin
      for S in Str'Range loop
         if Character'Pos (Str (S)) >= 32
           and then Character'Pos (Str (S)) <= 128
           and then Is_Graphic (Str (S))
         then
            Result (Index) := Str (S);
            Index := Index + 1;
         else
            Result (Index) := '[';
            Result (Index + 1) := To_Hex (Character'Pos (Str (S)) / 16);
            Result (Index + 2) := To_Hex (Character'Pos (Str (S)) mod 16);
            Result (Index + 3) := ']';
            Index := Index + 4;
         end if;
      end loop;
      return Result (1 .. Index - 1);
   end To_Graphic_String;

   ------------------------
   -- Validate_Attribute --
   ------------------------

   procedure Validate_Attribute
     (Attr   : Attribute_Descr;
      Reader : access Abstract_Validation_Reader'Class;
      Atts   : in out Sax_Attribute_List;
      Index  : Natural)
   is
      Value    : Symbol := Get_Value (Atts, Index);
      Val      : Cst_Byte_Sequence_Access;
      Is_Equal : Boolean;
      Descr    : Simple_Type_Descr;
   begin
      if Debug then
         Debug_Output ("Validate attribute " & To_QName (Attr.Name)
                       & " simpleType=" & Attr.Simple_Type'Img);
      end if;

      if Attr.Simple_Type = No_Simple_Type_Index then
         if Debug then
            Debug_Output ("No simple type defined");
         end if;
      else
         Descr := Get_Simple_Type (Get (Reader.Grammar).NFA, Attr.Simple_Type);
         Normalize_And_Validate
           (Parser => Reader,
            Simple => Attr.Simple_Type,
            Fixed  => Value,
            Loc    => Get_Location (Atts, Index));
         Set_Value (Atts, Index, Value);

         if Descr.Kind = Primitive_ID then
            Set_Type (Atts, Index, Sax.Attributes.Id);
         end if;
      end if;

      Val := Get (Value);

      --  3.2.4 [Attribute Declaration Value] indicates we should check Fixed
      --  with the "actual value" of the attribute, not the "normalized value".
      --  However, we need to match depending on the type of the attribute: if
      --  it is an integer, the whitespaces are irrelevant; likewise for a list

      if Attr.Fixed /= No_Symbol then
         if Debug then
            Debug_Output ("Attribute value must be equal to """
                          & Get (Attr.Fixed).all & """");
         end if;

         if Attr.Simple_Type = No_Simple_Type_Index then
            Is_Equal := Get (Attr.Fixed).all = Val.all;
         else
            Is_Equal := Equal (Reader, Attr.Simple_Type, Attr.Fixed, Val.all);
         end if;

         if not Is_Equal then
            Validation_Error
              (Reader, "value must be """
               & To_Graphic_String (Get (Attr.Fixed).all)
               & """ (found """ & To_Graphic_String (Val.all) & """)",
               Get_Location (Atts, Index));
         end if;
      end if;
   end Validate_Attribute;

   --------------
   -- To_QName --
   --------------

   function To_QName
     (Name : Qualified_Name) return Unicode.CES.Byte_Sequence is
   begin
      if Name = No_Qualified_Name then
         return "";
      else
         return Sax.Readers.To_QName (Name.NS, Name.Local);
      end if;
   end To_QName;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table
     (Grammar : XML_Grammar) return Sax.Utils.Symbol_Table is
   begin
      if Grammar = No_Grammar then
         return Symbol_Table_Pointers.Null_Pointer;
      else
         return Get (Grammar).Symbols;
      end if;
   end Get_Symbol_Table;

   ----------------------
   -- Set_Symbol_Table --
   ----------------------

   procedure Set_Symbol_Table
     (Grammar : XML_Grammar; Symbols : Sax.Utils.Symbol_Table) is
   begin
      if Grammar /= No_Grammar then
         Get (Grammar).Symbols := Symbols;
      end if;
   end Set_Symbol_Table;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out String_List) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (String_List_Record, String_List);
      L : String_List;
   begin
      while List /= null loop
         L := List.Next;
         Unchecked_Free (List);
         List := L;
      end loop;
   end Free;

   ------------------------------
   -- Create_Grammar_If_Needed --
   ------------------------------

   procedure Create_Grammar_If_Needed
     (Grammar : in out XML_Grammar;
      Symbols : Symbol_Table := No_Symbol_Table)
   is
      use Types_Tables;
      G : XML_Grammars.Encapsulated_Access;
   begin
      if Grammar = No_Grammar then
         G     := new XML_Grammar_Record;
         G.Symbols := Symbols;
         G.NFA := new Schema_NFA;
         G.NFA.Initialize (States_Are_Statefull => True);
         Init (G.NFA.Attributes);
         Init (G.NFA.Enumerations);
         Init (G.NFA.Types);
         G.NFA.References :=
           new Reference_HTables.HTable (Size => Reference_HTable_Size);
         Simple_Type_Tables.Init (G.NFA.Simple_Types);
         Grammar  := Allocate (G);
      end if;
   end Create_Grammar_If_Needed;

   ---------------------
   -- Set_XSD_Version --
   ---------------------

   procedure Set_XSD_Version
     (Grammar     : in out XML_Grammar;
      XSD_Version : XSD_Versions) is
   begin
      Create_Grammar_If_Needed (Grammar);
      GNAT.Task_Lock.Lock;
      Get (Grammar).XSD_Version := XSD_Version;
      GNAT.Task_Lock.Unlock;
   end Set_XSD_Version;

   ---------------------
   -- Get_XSD_Version --
   ---------------------

   function Get_XSD_Version (Grammar : XML_Grammar) return XSD_Versions is
      G   : XML_Grammars.Encapsulated_Access;
   begin
      G := Get (Grammar);
      if G = null then
         return XSD_1_1;
      else
         return G.XSD_Version;
      end if;
   end Get_XSD_Version;

   -----------------------------
   -- Create_Global_Attribute --
   -----------------------------

   procedure Create_Global_Attribute
     (Parser    : access Abstract_Validation_Reader'Class;
      Attr      : Attribute_Descr;
      Loc       : Sax.Locators.Location)
   is
      use Reference_HTables;
      NFA : constant Schema_NFA_Access := Get_NFA (Parser.Grammar);
      List : Attributes_List := No_Attributes;
   begin
      Add_Attribute (Parser, List, Attr, Loc => Loc);
      Set
        (NFA.References.all,
         (Kind => Ref_Attribute, Name => Attr.Name, Attributes => List));
   end Create_Global_Attribute;

   ------------------------
   -- Create_Simple_Type --
   ------------------------

   function Create_Simple_Type
     (NFA   : access Schema_NFA'Class;
      Descr : Schema.Simple_Types.Simple_Type_Descr)
      return Schema.Simple_Types.Simple_Type_Index
   is
      use Simple_Type_Tables;
   begin
      Append (NFA.Simple_Types, Descr);
      return Last (NFA.Simple_Types);
   end Create_Simple_Type;

   -----------------
   -- Create_Type --
   -----------------

   function Create_Type
     (NFA   : access Schema_NFA'Class;
      Descr : Type_Descr) return Type_Index
   is
      use Reference_HTables, Types_Tables;
   begin
      Append (NFA.Types, Descr);

      if Descr.Name /= No_Qualified_Name then
         if Debug then
            Debug_Output ("Create_global_type: " & To_QName (Descr.Name)
                          & " at index" & Last (NFA.Types)'Img);
         end if;
         Set (NFA.References.all, (Ref_Type, Descr.Name, Last (NFA.Types)));
      end if;

      return Last (NFA.Types);
   end Create_Type;

   ---------------------
   -- Get_Simple_Type --
   ---------------------

   function Get_Simple_Type
     (NFA    : access Schema_NFA'Class;
      Simple  : Schema.Simple_Types.Simple_Type_Index)
      return Schema.Simple_Types.Simple_Type_Descr is
   begin
      return NFA.Simple_Types.Table (Simple);
   end Get_Simple_Type;

   --------------------
   -- Get_Type_Descr --
   --------------------

   function Get_Type_Descr
     (NFA   : access Schema_NFA'Class;
      Index : Type_Index) return access Type_Descr is
   begin
      return NFA.Types.Table (Index)'Unrestricted_Access;
   end Get_Type_Descr;

   -------------------
   -- Simple_Nested --
   -------------------

   function Simple_Nested
     (NFA : access Schema_NFA'Class) return Schema_State_Machines.State is
   begin
      return NFA.Simple_Nested;
   end Simple_Nested;

   ------------------------
   -- Initialize_Grammar --
   ------------------------

   procedure Initialize_Grammar
     (Reader : in out Abstract_Validation_Reader'Class)
   is
      use Reference_HTables, Simple_Type_Tables;
      G : XML_Grammars.Encapsulated_Access;

      function Register
        (Local          : Byte_Sequence;
         Descr          : Simple_Type_Descr;
         Restriction_Of : Type_Index) return Type_Index;

      function Create_UR_Type
        (Process_Contents : Process_Contents_Type)
         return State;
      --  Return the start state for a nested NFA for ur-type
      --  All children (at any depth level) are allowed.
      --  Any character contents is allowed.

      --------------
      -- Register --
      --------------

      function Register
        (Local          : Byte_Sequence;
         Descr          : Simple_Type_Descr;
         Restriction_Of : Type_Index) return Type_Index
      is
         Simple : Simple_Type_Index;
      begin
         Simple := Create_Simple_Type (G.NFA, Descr);
         return Create_Type
           (G.NFA,
            Type_Descr'
              (Name            =>
                 (NS    => Reader.XML_Schema_URI,
                  Local => Find (G.Symbols, Local)),
               Attributes      => No_Attributes,
               Block           => No_Block,
               Final           => (others => False),
               Restriction_Of  => Restriction_Of,
               Extension_Of    => No_Type_Index,
               Simple_Content  => Simple,
               Mixed           => False,
               Is_Abstract     => False,
               Complex_Content => No_State));
      end Register;

      --------------------
      -- Create_UR_Type --
      --------------------

      function Create_UR_Type
        (Process_Contents : Process_Contents_Type)
         return State
      is
         S1      : constant State := G.NFA.Add_State;
         Ur_Type : constant Nested_NFA := G.NFA.Create_Nested (S1);
         S2, S3  : State;
         List    : Attributes_List := No_Attributes;
         Index   : Type_Index;

      begin
         List.Any := Any_Descr'
           (Process_Contents => Process_Lax,
            No_Namespaces    => No_Symbol,
            Namespaces       => Reader.Any_Namespace);
         Index := Create_Type
           (G.NFA,
            Type_Descr'
              (Name            => (NS    => Reader.XML_Schema_URI,
                                   Local => Reader.Ur_Type),
               Attributes      => List,
               Mixed           => True,
               Complex_Content => S1,
               others          => <>));

         G.NFA.Set_Data (S1, (Simple   => Index,
                              Fixed    => No_Symbol,
                              Default  => No_Symbol,
                              Nillable => True,
                              Block    => No_Block));
         S2 := G.NFA.Add_State ((Simple   => Index,
                                 Fixed    => No_Symbol,
                                 Default  => No_Symbol,
                                 Nillable => True,
                                 Block    => No_Block));

         G.NFA.Set_Nested (S2, Ur_Type);

         pragma Assert (Reader.Any_Namespace /= No_Symbol);
         G.NFA.Add_Transition
           (S1, S2,
            (Kind => Transition_Any,
             Any  => (Process_Contents => Process_Contents,
                      Namespaces       => Reader.Any_Namespace,
                      No_Namespaces    => No_Symbol)));

         S3 := G.NFA.Add_State;
         G.NFA.On_Empty_Nested_Exit (S2, S3);
         G.NFA.Add_Empty_Transition (S3, S1); --  maxOccurs="unbounded"
         G.NFA.Add_Empty_Transition (S1, S3); --  minOccurs="0"
         G.NFA.Add_Transition (S3, Final_State, (Kind => Transition_Close));
         return S1;
      end Create_UR_Type;

      procedure Do_Register is new Register_Predefined_Types
        (Type_Index, No_Type_Index, Register);

      Attr : Attribute_Descr;
   begin
      Create_Grammar_If_Needed (Reader.Grammar, Get_Symbol_Table (Reader));

      --  In the case of a shared grammar, created will always be false (since
      --  it has already been parsed), so the code below will not be called.
      --  As such, it is safe to let it modify the grammar.

      G := Get (Reader.Grammar);

      if Get (G.NFA.References.all,
        (Name => (NS => Reader.XML_Schema_URI,
                  Local => Reader.S_Boolean),
         Kind => Ref_Type)) = No_Global_Reference
      then
         Do_Register (G.Symbols);   --  Simple types

         Attr := (Name => (NS => Reader.XML_URI, Local => Reader.Lang),
                  Form => Qualified,
                  others => <>);
         Create_Global_Attribute (Reader'Access, Attr, No_Location);

         Attr := (Name =>
                    (NS => Reader.XML_URI, Local => Find (G.Symbols, "space")),
                  Form => Qualified,
                  others => <>);
         Create_Global_Attribute (Reader'Access, Attr, No_Location);

         Attr := (Name => (NS => Reader.XML_URI, Local => Reader.Base),
                  Form => Qualified,
                  others => <>);
         Create_Global_Attribute (Reader'Access, Attr, No_Location);

         --  Added support for <ur-Type>

         G.NFA.Ur_Type := Create_UR_Type (Process_Lax);
         G.NFA.Ur_Type_Skip := Create_UR_Type (Process_Skip);

         Add_Schema_For_Schema (Reader);

         --  The simple nested NFA

         G.NFA.Simple_Nested := G.NFA.Add_State;
         G.NFA.Add_Transition
           (G.NFA.Simple_Nested, Final_State,
            (Kind => Transition_Close));

         --  Save the current state, so that we can restore the grammar to just
         --  this metaschema.

         G.NFA.Metaschema_NFA_Last := Get_Snapshot (G.NFA);
         G.NFA.Metaschema_Simple_Types_Last :=
           Simple_Type_Tables.Last (G.NFA.Simple_Types);
         G.NFA.Metaschema_Attributes_Last :=
           Attributes_Tables.Last (G.NFA.Attributes);
         G.NFA.Metaschema_Enumerations_Last :=
           Enumeration_Tables.Last (G.NFA.Enumerations);
         G.NFA.Metaschema_Types_Last := Types_Tables.Last (G.NFA.Types);
      end if;
   end Initialize_Grammar;

   -------------
   -- Ur_Type --
   -------------

   function Ur_Type
     (NFA              : access Schema_NFA'Class;
      Process_Contents : Process_Contents_Type)
      return Schema_State_Machines.Nested_NFA
   is
   begin
      case Process_Contents is
         when Process_Skip =>
            return NFA.Create_Nested (NFA.Ur_Type_Skip);
         when others =>
            return NFA.Create_Nested (NFA.Ur_Type);
      end case;
   end Ur_Type;

   ----------------
   -- Debug_Dump --
   ----------------

   procedure Debug_Dump (Grammar : XML_Grammar) is
      Str : String_List;
      G    : constant XML_Grammars.Encapsulated_Access := Get (Grammar);
   begin
      if Debug then
         Str := G.Parsed_Locations;
         while Str /= null loop
            Debug_Output ("   Parsed location: " & Get (Str.Str).all);
            Str := Str.Next;
         end loop;
      end if;
   end Debug_Dump;

   ----------
   -- Free --
   ----------

   procedure Free (Grammar : in out XML_Grammar_Record) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Reference_HTables.HTable, Reference_HTable);
   begin
      if Debug then
         Debug_Output ("Freeing grammar");
      end if;

      Reset_Simple_Types (Grammar.NFA, No_Simple_Type_Index);
      Enumeration_Tables.Free (Grammar.NFA.Enumerations);
      Free (Grammar.NFA.Attributes);
      Reference_HTables.Reset (Grammar.NFA.References.all);
      Unchecked_Free (Grammar.NFA.References);
      Types_Tables.Free (Grammar.NFA.Types);
      Symbol_Htable.Reset (Grammar.NFA.Notations);
      Free (NFA_Access (Grammar.NFA));
      Free (Grammar.Parsed_Locations);
   end Free;

   ------------------------
   -- Reset_Simple_Types --
   ------------------------

   procedure Reset_Simple_Types
     (NFA : access Schema_NFA'Class;
      To  : Simple_Type_Index := No_Simple_Type_Index) is
   begin
      for S in To + 1 .. Simple_Type_Tables.Last (NFA.Simple_Types) loop
         Free (NFA.Simple_Types.Table (S).Pattern);
      end loop;

      if To = No_Simple_Type_Index then
         Simple_Type_Tables.Free (NFA.Simple_Types);
      else
         Simple_Type_Tables.Set_Last (NFA.Simple_Types, To);
      end if;
   end Reset_Simple_Types;

   -----------
   -- Reset --
   -----------

   procedure Reset (Grammar : in out XML_Grammar) is
      use Reference_HTables;
      G   : constant XML_Grammars.Encapsulated_Access := Get (Grammar);
      NFA : Schema_NFA_Access;

      function Preserve (TRef : Global_Reference) return Boolean;

      --------------
      -- Preserve --
      --------------

      function Preserve (TRef : Global_Reference) return Boolean is
         R : Boolean;
      begin
         case TRef.Kind is
            when Ref_Element =>
               R := Exists (NFA.Metaschema_NFA_Last, TRef.Element);
            when Ref_Type =>
               R := TRef.Typ <= NFA.Metaschema_Types_Last;
            when Ref_Group =>
               R := Exists (NFA.Metaschema_NFA_Last, TRef.Gr_Start);
            when Ref_Attribute | Ref_AttrGroup =>
               R := TRef.Attributes.Named <= NFA.Metaschema_Attributes_Last;
         end case;
         return R;
      end Preserve;

      procedure Remove_All is new Reference_HTables.Remove_All (Preserve);

   begin
      if Debug then
         Debug_Output ("Partial reset of the grammar");
      end if;

      if G = null then
         return;
      end if;

      NFA := G.NFA;
      Free (G.Parsed_Locations);

      if NFA.Metaschema_NFA_Last /= No_NFA_Snapshot then
         if Debug then
            Debug_Output ("Preserve metaschema information");
         end if;
         Enumeration_Tables.Set_Last
           (NFA.Enumerations, NFA.Metaschema_Enumerations_Last);
         Attributes_Tables.Set_Last
           (NFA.Attributes, NFA.Metaschema_Attributes_Last);
         Types_Tables.Set_Last (NFA.Types, NFA.Metaschema_Types_Last);

         Reset_Simple_Types (NFA, NFA.Metaschema_Simple_Types_Last);
         Remove_All (NFA.References.all);

         --  From the toplevel choice (ie the list of valid global elements),
         --  we need to keep only those belonging to our metaschema, not those
         --  from grammars loaded afterward

         Reset_To_Snapshot (NFA, NFA.Metaschema_NFA_Last);
      end if;
   end Reset;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Ref : Global_Reference) return Reference_Name is
   begin
      return (Kind => Ref.Kind, Name => Ref.Name);
   end Get_Key;

   --------------------
   -- URI_Was_Parsed --
   --------------------

   function URI_Was_Parsed
     (Grammar : XML_Grammar; URI : Symbol) return Boolean
   is
      L : String_List;
   begin
      if Grammar /= No_Grammar then
         L := Get (Grammar).Parsed_Locations;
         while L /= null loop
            if Debug then
               Debug_Output ("URI_Was_Parsed ("
                             & Get (URI).all & ") ? Compare with "
                             & Get (L.Str).all);
            end if;
            if L.Str = URI then
               if Debug then
                  Debug_Output ("    => Yes, already parsed");
               end if;
               return True;
            end if;
            L := L.Next;
         end loop;
      end if;
      return False;
   end URI_Was_Parsed;

   --------------------
   -- Set_Parsed_URI --
   --------------------

   procedure Set_Parsed_URI
     (Reader : in out Abstract_Validation_Reader'Class;
      URI    : Symbol) is
   begin
      Initialize_Grammar (Reader);

      if Debug then
         Debug_Output ("Set_Parsed_UI: " & Get (URI).all);
      end if;
      Get (Reader.Grammar).Parsed_Locations := new String_List_Record'
        (Str  => URI,
         Next => Get (Reader.Grammar).Parsed_Locations);
   end Set_Parsed_URI;

   -------------
   -- Get_NFA --
   -------------

   function Get_NFA (Grammar : XML_Grammar) return Schema_NFA_Access is
   begin
      return Get (Grammar).NFA;
   end Get_NFA;

   --------------------
   -- Get_References --
   --------------------

   function Get_References (Grammar : XML_Grammar) return Reference_HTable is
   begin
      return Get (Grammar).NFA.References;
   end Get_References;

   ------------------------
   -- Initialize_Symbols --
   ------------------------

   overriding procedure Initialize_Symbols
     (Parser : in out Abstract_Validation_Reader)
   is
      use Symbol_Table_Pointers;
   begin
      Initialize_Symbols (Sax_Reader (Parser));

      if Parser.Grammar /= No_Grammar then
         if Get (Parser.Grammar).Symbols =
           Symbol_Table_Pointers.Null_Pointer
         then
            if Debug then
               Debug_Output ("Initialze_Symbols, set grammar's symbol table");
            end if;
            Get (Parser.Grammar).Symbols := Get_Symbol_Table (Parser);
         end if;
      end if;

      if Parser.Xmlns /= No_Symbol then
         return;
      end if;

      Parser.All_NNI                := Find_Symbol (Parser, "allNNI");
      Parser.Annotated              := Find_Symbol (Parser, "annotated");
      Parser.Annotation             := Find_Symbol (Parser, "annotation");
      Parser.Any                    := Find_Symbol (Parser, "any");
      Parser.Any_Attribute          := Find_Symbol (Parser, "anyAttribute");
      Parser.Any_Namespace          := Find_Symbol (Parser, "##any");
      Parser.Any_Simple_Type        := Find_Symbol (Parser, "anySimpleType");
      Parser.Anytype                := Find_Symbol (Parser, "anyType");
      Parser.Appinfo                := Find_Symbol (Parser, "appinfo");
      Parser.Attr_Decls             := Find_Symbol (Parser, "attrDecls");
      Parser.Attribute              := Find_Symbol (Parser, "attribute");
      Parser.Attribute_Group        := Find_Symbol (Parser, "attributeGroup");
      Parser.Attribute_Group_Ref  :=
        Find_Symbol (Parser, "attributeGroupRef");
      Parser.Base                   := Find_Symbol (Parser, "base");
      Parser.Block                  := Find_Symbol (Parser, "block");
      Parser.Block_Default          := Find_Symbol (Parser, "blockDefault");
      Parser.Block_Set              := Find_Symbol (Parser, "blockSet");
      Parser.Choice                 := Find_Symbol (Parser, "choice");
      Parser.Complex_Content        := Find_Symbol (Parser, "complexContent");
      Parser.Complex_Extension_Type :=
        Find_Symbol (Parser, "complexExtensionType");
      Parser.Complex_Restriction_Type :=
        Find_Symbol (Parser, "complexRestrictionType");
      Parser.Complex_Type           := Find_Symbol (Parser, "complexType");
      Parser.Complex_Type_Model     :=
        Find_Symbol (Parser, "complexTypeModel");
      Parser.Def_Ref                := Find_Symbol (Parser, "defRef");
      Parser.Default                := Find_Symbol (Parser, "default");
      Parser.Derivation_Control     :=
        Find_Symbol (Parser, "derivationControl");
      Parser.Derivation_Set         := Find_Symbol (Parser, "derivationSet");
      Parser.Documentation          := Find_Symbol (Parser, "documentation");
      Parser.Element                := Find_Symbol (Parser, "element");
      Parser.Enumeration            := Find_Symbol (Parser, "enumeration");
      Parser.Explicit_Group         := Find_Symbol (Parser, "explicitGroup");
      Parser.Extension              := Find_Symbol (Parser, "extension");
      Parser.Extension_Type         := Find_Symbol (Parser, "extensionType");
      Parser.Facet                  := Find_Symbol (Parser, "facet");
      Parser.Field                  := Find_Symbol (Parser, "field");
      Parser.Final                  := Find_Symbol (Parser, "final");
      Parser.Final_Default          := Find_Symbol (Parser, "finalDefault");
      Parser.Fixed                  := Find_Symbol (Parser, "fixed");
      Parser.Form                   := Find_Symbol (Parser, "form");
      Parser.Form_Choice            := Find_Symbol (Parser, "formChoice");
      Parser.Fraction_Digits        := Find_Symbol (Parser, "fractionDigits");
      Parser.Group                  := Find_Symbol (Parser, "group");
      Parser.Group_Def_Particle     :=
        Find_Symbol (Parser, "groupDefParticle");
      Parser.Group_Ref              := Find_Symbol (Parser, "groupRef");
      Parser.Id                     := Find_Symbol (Parser, "id");
      Parser.IDREF                  := Find_Symbol (Parser, "IDREF");
      Parser.IDREFS                 := Find_Symbol (Parser, "IDREFS");
      Parser.Identity_Constraint    :=
        Find_Symbol (Parser, "identityConstraint");
      Parser.Import                 := Find_Symbol (Parser, "import");
      Parser.Include                := Find_Symbol (Parser, "include");
      Parser.Item_Type              := Find_Symbol (Parser, "itemType");
      Parser.Key                    := Find_Symbol (Parser, "key");
      Parser.Keybase                := Find_Symbol (Parser, "keybase");
      Parser.Keyref                 := Find_Symbol (Parser, "keyref");
      Parser.Lang                   := Find_Symbol (Parser, "lang");
      Parser.Lax                    := Find_Symbol (Parser, "lax");
      Parser.Length                 := Find_Symbol (Parser, "length");
      Parser.List                   := Find_Symbol (Parser, "list");
      Parser.Local                  := Find_Symbol (Parser, "##local");
      Parser.Local_Complex_Type     :=
        Find_Symbol (Parser, "localComplexType");
      Parser.Local_Element          := Find_Symbol (Parser, "localElement");
      Parser.Local_Simple_Type      := Find_Symbol (Parser, "localSimpleType");
      Parser.MaxExclusive           := Find_Symbol (Parser, "maxExclusive");
      Parser.MaxInclusive           := Find_Symbol (Parser, "maxInclusive");
      Parser.MaxOccurs              := Find_Symbol (Parser, "maxOccurs");
      Parser.Max_Bound              := Find_Symbol (Parser, "maxBound");
      Parser.Maxlength              := Find_Symbol (Parser, "maxLength");
      Parser.Member_Types           := Find_Symbol (Parser, "memberTypes");
      Parser.MinExclusive           := Find_Symbol (Parser, "minExclusive");
      Parser.MinInclusive           := Find_Symbol (Parser, "minInclusive");
      Parser.MinOccurs              := Find_Symbol (Parser, "minOccurs");
      Parser.Min_Bound              := Find_Symbol (Parser, "minBound");
      Parser.Minlength              := Find_Symbol (Parser, "minLength");
      Parser.Mixed                  := Find_Symbol (Parser, "mixed");
      Parser.NCName                 := Find_Symbol (Parser, "NCName");
      Parser.NMTOKEN                := Find_Symbol (Parser, "NMTOKEN");
      Parser.Name                   := Find_Symbol (Parser, "name");
      Parser.Named_Attribute_Group  :=
        Find_Symbol (Parser, "namedAttributeGroup");
      Parser.Named_Group            := Find_Symbol (Parser, "namedGroup");
      Parser.Namespace              := Find_Symbol (Parser, "namespace");
      Parser.Namespace_List         := Find_Symbol (Parser, "namespaceList");
      Parser.Nested_Particle        := Find_Symbol (Parser, "nestedParticle");
      Parser.Nil                    := Find_Symbol (Parser, "nil");
      Parser.Nillable               := Find_Symbol (Parser, "nillable");
      Parser.No_Namespace_Schema_Location :=
        Find_Symbol (Parser, "noNamespaceSchemaLocation");
      Parser.Non_Negative_Integer   :=
        Find_Symbol (Parser, "nonNegativeInteger");
      Parser.Notation               := Find_Symbol (Parser, "notation");
      Parser.Num_Facet              := Find_Symbol (Parser, "numFacet");
      Parser.Occurs                 := Find_Symbol (Parser, "occurs");
      Parser.Open_Attrs             := Find_Symbol (Parser, "openAttrs");
      Parser.Optional               := Find_Symbol (Parser, "optional");
      Parser.Other_Namespace        := Find_Symbol (Parser, "##other");
      Parser.Particle               := Find_Symbol (Parser, "particle");
      Parser.Pattern                := Find_Symbol (Parser, "pattern");
      Parser.Positive_Integer       := Find_Symbol (Parser, "positiveInteger");
      Parser.Precision_Decimal      :=
        Find_Symbol (Parser, "precisionDecimal");
      Parser.Process_Contents       := Find_Symbol (Parser, "processContents");
      Parser.Prohibited             := Find_Symbol (Parser, "prohibited");
      Parser.Public                 := Find_Symbol (Parser, "public");
      Parser.QName                  := Find_Symbol (Parser, "QName");
      Parser.Qualified              := Find_Symbol (Parser, "qualified");
      Parser.Real_Group             := Find_Symbol (Parser, "realGroup");
      Parser.Redefinable            := Find_Symbol (Parser, "redefinable");
      Parser.Redefine               := Find_Symbol (Parser, "redefine");
      Parser.Reduced_Derivation_Control :=
        Find_Symbol (Parser, "reducedDerivationControl");
      Parser.Ref                    := Find_Symbol (Parser, "ref");
      Parser.Refer                  := Find_Symbol (Parser, "refer");
      Parser.Required               := Find_Symbol (Parser, "required");
      Parser.Restriction            := Find_Symbol (Parser, "restriction");
      Parser.Restriction_Type       := Find_Symbol (Parser, "restrictionType");
      Parser.S_1                    := Find_Symbol (Parser, "1");
      Parser.S_Abstract             := Find_Symbol (Parser, "abstract");
      Parser.S_All                  := Find_Symbol (Parser, "all");
      Parser.S_Attribute_Form_Default :=
        Find_Symbol (Parser, "attributeFormDefault");
      Parser.S_Boolean              := Find_Symbol (Parser, "boolean");
      Parser.S_Element_Form_Default :=
        Find_Symbol (Parser, "elementFormDefault");
      Parser.S_False                := Find_Symbol (Parser, "false");
      Parser.S_Schema               := Find_Symbol (Parser, "schema");
      Parser.S_String               := Find_Symbol (Parser, "string");
      Parser.S_Use                  := Find_Symbol (Parser, "use");
      Parser.Schema_Location        := Find_Symbol (Parser, "schemaLocation");
      Parser.Schema_Top             := Find_Symbol (Parser, "schemaTop");
      Parser.Selector               := Find_Symbol (Parser, "selector");
      Parser.Sequence               := Find_Symbol (Parser, "sequence");
      Parser.Simple_Content         := Find_Symbol (Parser, "simpleContent");
      Parser.Simple_Derivation      :=
        Find_Symbol (Parser, "simpleDerivation");
      Parser.Simple_Derivation_Set  :=
        Find_Symbol (Parser, "simpleDerivationSet");
      Parser.Simple_Extension_Type  :=
        Find_Symbol (Parser, "simpleExtensionType");
      Parser.Simple_Restriction_Model :=
        Find_Symbol (Parser, "simpleRestrictionModel");
      Parser.Simple_Restriction_Type  :=
        Find_Symbol (Parser, "simpleRestrictionType");
      Parser.Simple_Type            := Find_Symbol (Parser, "simpleType");
      Parser.Source                 := Find_Symbol (Parser, "source");
      Parser.Strict                 := Find_Symbol (Parser, "strict");
      Parser.Substitution_Group     :=
        Find_Symbol (Parser, "substitutionGroup");
      Parser.System                 := Find_Symbol (Parser, "system");
      Parser.Target_Namespace       :=
        Find_Symbol (Parser, "##targetNamespace");
      Parser.Namespace_Target       := Find_Symbol (Parser, "targetNamespace");
      Parser.Token                  := Find_Symbol (Parser, "token");
      Parser.Top_Level_Attribute    :=
        Find_Symbol (Parser, "topLevelAttribute");
      Parser.Top_Level_Complex_Type :=
        Find_Symbol (Parser, "topLevelComplexType");
      Parser.Top_Level_Element      :=
        Find_Symbol (Parser, "topLevelElement");
      Parser.Top_Level_Simple_Type  :=
        Find_Symbol (Parser, "topLevelSimpleType");
      Parser.Total_Digits           := Find_Symbol (Parser, "totalDigits");
      Parser.Typ                    := Find_Symbol (Parser, "type");
      Parser.Type_Def_Particle      := Find_Symbol (Parser, "typeDefParticle");
      Parser.UC_ID                  := Find_Symbol (Parser, "ID");
      Parser.URI_Reference          := Find_Symbol (Parser, "uriReference");
      Parser.Unbounded              := Find_Symbol (Parser, "unbounded");
      Parser.Union                  := Find_Symbol (Parser, "union");
      Parser.Unique                 := Find_Symbol (Parser, "unique");
      Parser.Unqualified            := Find_Symbol (Parser, "unqualified");
      Parser.Ur_Type                := Find_Symbol (Parser, "ur-Type");
      Parser.Value                  := Find_Symbol (Parser, "value");
      Parser.Version                := Find_Symbol (Parser, "version");
      Parser.Whitespace             := Find_Symbol (Parser, "whiteSpace");
      Parser.Wildcard               := Find_Symbol (Parser, "wildcard");
      Parser.XML_Instance_URI       := Find_Symbol (Parser, XML_Instance_URI);

      Parser.XML_Schema_URI         := Find_Symbol (Parser, XML_Schema_URI);
      Parser.XML_URI                := Find_Symbol (Parser, XML_URI);
      Parser.XPath                  := Find_Symbol (Parser, "xpath");
      Parser.XPath_Expr_Approx      := Find_Symbol (Parser, "XPathExprApprox");
      Parser.XPath_Spec             := Find_Symbol (Parser, "XPathSpec");
      Parser.Xmlns                  := Find_Symbol (Parser, "xmlns");
   end Initialize_Symbols;

   -----------
   -- Image --
   -----------

   function Image (Trans : Transition_Descr) return String is
   begin
      case Trans.Kind is
         when Transition_Symbol | Transition_Symbol_From_All =>
            if Trans.Name.Local = No_Symbol then
               return "";
            else
               return To_QName (Trans.Name);
            end if;
         when Transition_Close | Transition_Close_From_All =>
            return "close parent";
         when Transition_Any   => return "<any>";
      end case;
   end Image;

   -----------
   -- Image --
   -----------

   function Image
     (Self : access NFA'Class;
      S    : Schema_State_Machines.State;
      Data : State_Data) return String
   is
      pragma Unreferenced (S);
      Local : Symbol;
   begin
      if Data.Simple = No_Type_Index then
         return "";
      else
         Local :=
           Schema_NFA_Access (Self).Types.Table (Data.Simple).Name.Local;
         if Local = No_Symbol then
            return "";
         else
            return Get (Local).all;
         end if;
      end if;
   end Image;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Reference_Name) return Interfaces.Unsigned_32 is
   begin
      return Interfaces.Unsigned_32
        (Hash (Name.Name) + Reference_Kind'Pos (Name.Kind));
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Qualified_Name) return Header_Num is
   begin
      return (Hash (Name.NS) + Hash (Name.Local)) / 2;
   end Hash;

   ----------
   -- Hash --
   ----------

   function Hash (Name : Sax.Symbols.Symbol) return Header_Num is
   begin
      if Name = No_Symbol then
         return 0;
      else
         return Header_Num
           (Sax.Symbols.Hash (Name)
            mod Interfaces.Unsigned_32 (Header_Num'Last));
      end if;
   end Hash;

   --------------------------
   -- Validate_Simple_Type --
   --------------------------

   procedure Validate_Simple_Type
     (Reader      : access Abstract_Validation_Reader'Class;
      Simple_Type : Schema.Simple_Types.Simple_Type_Index;
      Ch          : Unicode.CES.Byte_Sequence;
      Loc         : Sax.Locators.Location;
      Insert_Id   : Boolean := True)
   is
      Error : Symbol;
      G : constant XML_Grammars.Encapsulated_Access := Get (Reader.Grammar);
   begin
      Validate_Simple_Type
        (Simple_Types  => G.NFA.Simple_Types,
         Enumerations  => G.NFA.Enumerations,
         Notations     => G.NFA.Notations,
         Symbols       => G.Symbols,
         Id_Table      => Reader.Id_Table,
         Insert_Id     => Insert_Id,
         Simple_Type   => Simple_Type,
         Ch            => Ch,
         Error         => Error,
         XML_Version   => Get_XML_Version (Reader.all));

      if Error /= No_Symbol then
         Validation_Error (Reader, Get (Error).all, Loc);
      end if;
   end Validate_Simple_Type;

   -----------
   -- Equal --
   -----------

   function Equal
     (Reader        : access Abstract_Validation_Reader'Class;
      Simple_Type   : Simple_Type_Index;
      Ch1           : Sax.Symbols.Symbol;
      Ch2           : Unicode.CES.Byte_Sequence) return Boolean
   is
      Is_Equal : Boolean;
      G : constant XML_Grammars.Encapsulated_Access := Get (Reader.Grammar);
   begin
      Equal
        (Simple_Types  => G.NFA.Simple_Types,
         Enumerations  => G.NFA.Enumerations,
         Notations     => G.NFA.Notations,
         Symbols       => G.Symbols,
         Id_Table      => Reader.Id_Table,
         Simple_Type   => Simple_Type,
         Ch1           => Ch1,
         Ch2           => Ch2,
         Is_Equal      => Is_Equal,
         XML_Version   => Get_XML_Version (Reader.all));
      return Is_Equal;
   end Equal;

   ---------------
   -- Add_Facet --
   ---------------

   procedure Add_Facet
     (Grammar      : XML_Grammar;
      Facets       : in out Schema.Simple_Types.All_Facets;
      Facet_Name   : Sax.Symbols.Symbol;
      Value        : Sax.Symbols.Symbol;
      Loc          : Sax.Locators.Location) is
   begin
      Add_Facet
        (Facets,
         Symbols      => Get (Grammar).Symbols,
         Enumerations => Get (Grammar).NFA.Enumerations,
         Facet_Name   => Facet_Name,
         Value        => Value,
         Loc          => Loc);
   end Add_Facet;

   ---------------
   -- To_String --
   ---------------

   function To_String (Blocks : Block_Status) return String is
   begin
      return "{restr=" & Blocks (Block_Restriction)'Img
        & " ext=" & Blocks (Block_Extension)'Img
        & " sub=" & Blocks (Block_Substitution)'Img & '}';
   end To_String;

   ---------------------------------
   -- Check_Substitution_Group_OK --
   ---------------------------------

   procedure Check_Substitution_Group_OK
     (Handler            : access Abstract_Validation_Reader'Class;
      New_Type, Old_Type : Type_Index;
      Loc                : Sax.Locators.Location;
      Element_Block      : Block_Status)
   is
      NFA       : constant Schema_NFA_Access := Get_NFA (Handler.Grammar);
      Old_Descr : constant access Type_Descr := NFA.Get_Type_Descr (Old_Type);
      New_Descr : constant access Type_Descr := NFA.Get_Type_Descr (New_Type);
      Has_Restriction, Has_Extension : Boolean := False;

      Simple_Old_Type : Simple_Type_Index := No_Simple_Type_Index;
      --  Current target for [Old_Type], when considered a simple type

      function From_Descr_To_Old
        (Index : Type_Index; Descr : access Type_Descr) return Boolean;
      --  Try moving from [Descr] to [Old_Descr] through a series of extensions
      --  or restrictions. [False] is returned if we could not reach the old
      --  description.

      -----------------------
      -- From_Descr_To_Old --
      -----------------------

      function From_Descr_To_Old
        (Index : Type_Index; Descr : access Type_Descr) return Boolean
      is
         Result : Boolean := False;
         R      : access Type_Descr;
      begin
         if Index = Old_Type
           or else (Simple_Old_Type /= No_Simple_Type_Index
                    and then Descr.Simple_Content = Simple_Old_Type)
         then
            return True;
         end if;

         if Descr.Restriction_Of /= No_Type_Index then
            R := NFA.Get_Type_Descr (Descr.Restriction_Of);
            Has_Restriction := True;
            Result := From_Descr_To_Old (Descr.Restriction_Of, R);
         end if;

         if not Result and then Descr.Extension_Of /= No_Type_Index then
            R := NFA.Get_Type_Descr (Descr.Extension_Of);
            Has_Extension := True;
            Result := From_Descr_To_Old (Descr.Extension_Of, R);
         end if;

         return Result;
      end From_Descr_To_Old;

   begin
      if New_Type = Old_Type
        or else Old_Type = NFA.Get_Data (NFA.Ur_Type).Simple
        or else Old_Descr.Name = (NS    => Handler.XML_Schema_URI,
                                  Local => Handler.Anytype)
      then
         return;
      end if;

      if Element_Block (Block_Substitution) then
         Validation_Error (Handler, "Element blocks substitutions", Loc);
      end if;

      if Old_Descr.Simple_Content /= No_Simple_Type_Index then
         declare
            Simple : constant Simple_Type_Descr :=
              NFA.Get_Simple_Type (Old_Descr.Simple_Content);
         begin
            case Simple.Kind is
            when Primitive_Union =>
               for U in Simple.Union'Range loop
                  if Simple.Union (U) /= No_Simple_Type_Index then
                     Simple_Old_Type := Simple.Union (U);
                     if From_Descr_To_Old (New_Type, New_Descr) then
                        return;
                     end if;
                  end if;
               end loop;

               Validation_Error
                 (Handler,
                  To_QName (New_Descr.Name)
                  & " is not a derivation of union "
                  & To_QName (Old_Descr.Name),
                  Loc);

            when Primitive_List =>
               Validation_Error
                 (Handler,
                  To_QName (New_Descr.Name)
                  & " is not a derivation of list "
                  & To_QName (Old_Descr.Name),
                  Loc);

            when others =>
               null;   --  Will be dealt with below
            end case;
         end;
      end if;

      if not From_Descr_To_Old (New_Type, New_Descr) then
         Validation_Error
           (Handler,
            To_QName (New_Descr.Name)
            & " is not a derivation of " & To_QName (Old_Descr.Name), Loc);
      end if;

      if Has_Restriction and then Old_Descr.Block (Block_Restriction) then
         Validation_Error
           (Handler, To_QName (Old_Descr.Name) & " blocks restrictions", Loc);
      end if;

      if Has_Restriction and then Element_Block (Block_Restriction) then
         Validation_Error (Handler, "Element blocks restrictions", Loc);
      end if;

      if Has_Extension and then Old_Descr.Block (Block_Extension) then
         Validation_Error
           (Handler, To_QName (Old_Descr.Name) & " blocks extensions", Loc);
      end if;

      if Has_Extension and then Element_Block (Block_Extension) then
         Validation_Error (Handler, "Element blocks extensions", Loc);
      end if;
   end Check_Substitution_Group_OK;

   ------------------
   -- Dump_Dot_NFA --
   ------------------

   function Dump_Dot_NFA
     (Grammar            : XML_Grammar;
      Nested             : Nested_NFA := No_Nested) return String
   is
      NFA : constant Schema_NFA_Access := Get (Grammar).NFA;
   begin
      if Nested = No_Nested then
         return Schema_State_Machines_PP.Dump
           (NFA,
            Mode                => Dump_Dot_Compact,
            Show_Details        => True,
            Show_Isolated_Nodes => False,
            Since               => NFA.Metaschema_NFA_Last);
      else
         return Schema_State_Machines_PP.Dump
           (NFA,
            Nested              => Nested,
            Mode                => Dump_Dot_Compact);
      end if;
   end Dump_Dot_NFA;

   --------------
   -- Expected --
   --------------

   function Expected
     (Self       : Abstract_NFA_Matcher'Class;
      From_State, To_State : State;
      Parent_Data : access Active_State_Data;
      Trans      : Transition_Descr) return String
   is
      pragma Unreferenced (Self, From_State, To_State);
      Mask : Visited_All_Children;
   begin
      case Trans.Kind is
         when Transition_Symbol_From_All =>
            --  Only if the element has not been visited yet

            Mask := 2 ** Trans.All_Child_Index;
            if (Parent_Data.Visited and Mask) = 0 then
               return Image (Trans);
            end if;

         when Transition_Close_From_All =>
            --  Only if all children have been visited.
            if (Parent_Data.Visited and Trans.Mask) = Trans.Mask then
               return "close parent";
            end if;

         when others =>
            return Image (Trans);
      end case;

      return "";
   end Expected;

   -----------
   -- Match --
   -----------

   function Match
     (Self       : access Abstract_NFA_Matcher'Class;
      From_State, To_State : State;
      Parent_Data : access Active_State_Data;
      Trans      : Transition_Descr;
      Sym        : Transition_Event) return Boolean
   is
      pragma Unreferenced (To_State);
      Result : Boolean;
      Mask   : Visited_All_Children;
   begin
      case Trans.Kind is
         when Transition_Close =>
            Result := Sym.Closing;

         when Transition_Symbol | Transition_Symbol_From_All =>
            if Sym.Closing then
               Result := False;
            else
               if From_State = Start_State then
                  --  At toplevel, always qualified
                  Result := Trans.Name = Sym.Name;
               else
                  case Trans.Form is
                  when Unqualified =>
                     Result := (NS => Empty_String, Local => Trans.Name.Local)
                       = Sym.Name;
                  when Qualified =>
                     Result := Trans.Name = Sym.Name;
                  end case;
               end if;
            end if;

            if Result and then Trans.Kind = Transition_Symbol_From_All then
               --  Check that the transition hasn't been visited yet

               Mask := 2 ** Trans.All_Child_Index;

               if (Parent_Data.Visited and Mask) = 1 then
                  Result := False;
               else
                  Parent_Data.Visited := Parent_Data.Visited or Mask;
                  Result := True;
               end if;
            end if;

         when Transition_Close_From_All =>
            --  Check that all children have been visited or are optional
            Result := ((Parent_Data.Visited and Trans.Mask) = Trans.Mask)
              and then Sym.Closing;

         when Transition_Any =>
            if Sym.Closing then
               Result := False;
            else
               Result := Match_Any (Trans.Any, Sym.Name);
               if Result then
                  Schema_NFA_Matcher (Self.all).Matched_Through_Any := True;
                  Schema_NFA_Matcher (Self.all).Matched_Process_Content :=
                    Trans.Any.Process_Contents;
               end if;
            end if;
      end case;

      return Result;
   end Match;

   --------------
   -- Do_Match --
   --------------

   procedure Do_Match
     (Matcher         : in out Schema_NFA_Matcher;
      Sym             : Transition_Event;
      Success         : out Boolean;
      Through_Any     : out Boolean;
      Through_Process : out Process_Contents_Type)
   is
   begin
      Matcher.Matched_Through_Any := False;
      Process (Matcher, Input => Sym, Success => Success);
      Through_Any     := Matcher.Matched_Through_Any;
      Through_Process := Matcher.Matched_Process_Content;
   end Do_Match;

   ------------------
   -- Add_Notation --
   ------------------

   procedure Add_Notation
     (NFA : access Schema_NFA'Class; Name : Sax.Symbols.Symbol) is
   begin
      Symbol_Htable.Set (NFA.Notations, Name);
   end Add_Notation;

   ----------
   -- Free --
   ----------

   procedure Free (Reader : in out Abstract_Validation_Reader) is
   begin
      Free (Reader.Id_Table);
   end Free;

end Schema.Validators;
