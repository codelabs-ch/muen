------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Locators;      use Sax.Locators;
with Sax.Utils;         use Sax.Utils;
with Sax.Readers;       use Sax.Readers;
with Sax.Symbols;       use Sax.Symbols;
with Schema.Simple_Types; use Schema.Simple_Types;
with Schema.Validators;   use Schema.Validators;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Schema.Schema_Readers; use Schema.Schema_Readers;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Schema.Readers is
   use Schema_State_Machines, Schema_State_Machines_PP;
   use Schema_State_Machines_Matchers;

   procedure Print is new Schema_State_Machines_Matchers.Debug_Print
     (Schema_State_Machines_PP.Node_Label);

   procedure Internal_Characters
     (Handler : access Validating_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   --  Store Ch in the current sequence of characters. This is needed to
   --  collapse multiple calls to Characters and Ignorable_Whitespace into a
   --  single string, for validation purposes.

   procedure Validate_Current_Characters
     (Handler : access Validating_Reader'Class;
      Loc     : Location);
   --  Validate the current set of characters

   procedure Reset (Parser : in out Validating_Reader);
   --  Reset the state of the parser so that we can parse other documents.
   --  This doesn't reset the grammar

   procedure Hook_Start_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access;
      Atts    : in out Sax_Attribute_List);
   procedure Hook_End_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access);
   procedure Hook_Characters
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Hook_Ignorable_Whitespace
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence);
   procedure Hook_Notation_Decl
     (Handler       : access Sax_Reader'Class;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence);
   --  See for the corresponding primitive operations. These provide the
   --  necessary validation hooks.

   -----------------
   -- Set_Grammar --
   -----------------

   procedure Set_Grammar
     (Reader  : in out Validating_Reader;
      Grammar : Schema.Validators.XML_Grammar)
   is
      use Symbol_Table_Pointers;
   begin
      if Debug then
         Debug_Output ("Set_Grammar");
      end if;

      if Grammar /= No_Grammar then
         if Get (Get_Symbol_Table (Reader)) = null then
            if Debug then
               Debug_Output ("Set reader's symbol table from grammar");
            end if;

            Set_Symbol_Table (Reader, Get_Symbol_Table (Grammar));

         elsif Get_Symbol_Table (Grammar) =
           Symbol_Table_Pointers.Null_Pointer
         then
            if Debug then
               Debug_Output ("Set grammar's symbol table from reader");
            end if;
            Set_Symbol_Table (Grammar, Get_Symbol_Table (Reader));

         elsif Get_Symbol_Table (Reader) /= Get_Symbol_Table (Grammar) then
            raise XML_Fatal_Error with
              "The grammar and the reader must use the same symbol table";
         end if;
      end if;

      Reader.Grammar := Grammar;
   end Set_Grammar;

   ----------------------
   -- Set_Symbol_Table --
   ----------------------

   overriding procedure Set_Symbol_Table
     (Parser  : in out Validating_Reader;
      Symbols : Sax.Utils.Symbol_Table)
   is
      use Symbol_Table_Pointers;
   begin
      if Parser.Grammar /= No_Grammar
        and then Get_Symbol_Table (Parser.Grammar) /= Symbols
      then
         raise XML_Fatal_Error with
           "The grammar and the reader must use the same symbol table";
      end if;

      if Symbols /= Get_Symbol_Table (Parser) then
         Parser.Xmlns := No_Symbol;  --  Will force another lookup next time
         Set_Symbol_Table (Sax_Reader (Parser), Symbols);
      end if;
   end Set_Symbol_Table;

   -----------------
   -- Get_Grammar --
   -----------------

   function Get_Grammar
     (Reader  : Validating_Reader) return Schema.Validators.XML_Grammar is
   begin
      return Reader.Grammar;
   end Get_Grammar;

   ---------------------
   -- To_Absolute_URI --
   ---------------------

   function To_Absolute_URI
     (Handler : Validating_Reader;
      URI     : Symbol) return Symbol
   is
      U : constant Cst_Byte_Sequence_Access := Get (URI);
   begin
      if URI = Empty_String then
         return URI;
      elsif U (U'First) /= '/'
        and then U (U'First) /= '\'
      then
         return Find_Symbol
           (Handler,
            Dir_Name
              (Get (Handler.Current_Location.System_Id).all) & U.all);
      else
         return URI;
      end if;
   end To_Absolute_URI;

   --------------------
   -- Parse_Grammars --
   --------------------

   procedure Parse_Grammars
     (Handler         : access Validating_Reader'Class;
      Schema_Location : Symbol;
      Do_Create_NFA : Boolean)
   is
      URI : Symbol := No_Symbol;

      procedure Callback (Ch : Byte_Sequence);
      procedure Callback (Ch : Byte_Sequence) is
      begin
         if URI = No_Symbol then
            URI := Find_Symbol (Handler.all, Ch);
         else
            Parse_Grammar
              (Handler,
               URI           => URI,
               Xsd_File      => Find_Symbol (Handler.all, Ch),
               Do_Create_NFA => Do_Create_NFA);
            URI := No_Symbol;
         end if;
      end Callback;

      procedure For_Each is new For_Each_Item (Callback);

   begin
      For_Each (Get (Schema_Location).all);
   end Parse_Grammars;

   ---------------------------------
   -- Validate_Current_Characters --
   ---------------------------------

   procedure Validate_Current_Characters
     (Handler : access Validating_Reader'Class;
      Loc     : Location)
   is
      Is_Empty   : Boolean;
      Whitespace : Whitespace_Restriction := Preserve;
      NFA  : constant Schema_NFA_Access := Get_NFA (Handler.Grammar);
      S    : State;
      Descr : access Type_Descr;
      Fixed   : Symbol := No_Symbol;
      Default : Symbol := No_Symbol;
      Data  : State_Data;
      Ty    : Type_Index;
      Is_Equal : Boolean;

   begin
      if Debug then
         Print (Handler.Matcher, Dump_Compact, "Validate_Current_Char: ");
      end if;

      --  Handling of nil elements

      if Handler.Is_Nil then
         if Handler.Characters_Count /= 0 then
            Validation_Error
              (Handler,
               "No character content allowed because the element is 'nilled'",
               Loc);
         end if;

         return;  --  Content is always considered valid
      end if;

      --  Check all active states to find our whitespace normalization rules,
      --  and whether elements have fixed values. Note that the fixed value
      --  is attached to an state with a nested state (ie the state
      --  representing the element itself).

      declare
         Iter : Active_State_Iterator :=
           For_Each_Active_State (Handler.Matcher,
                                  Ignore_If_Nested  => True,
                                  Ignore_If_Default => True);
      begin
         loop
            S := Current (Handler.Matcher, Iter);
            exit when S = No_State;

            Data := Current_Data (Handler.Matcher, Iter);

            if Fixed = No_Symbol then
               Fixed := Data.Fixed;

               --  Get the "fixed" value from the element
               --   (if it has a complexType)
               if Fixed = No_Symbol and then Has_Parent (Iter) then
                  Fixed := Current_Data (Handler.Matcher, Parent (Iter)).Fixed;
               end if;
            end if;

            if Default = No_Symbol then
               Default := Data.Default;
               if Default = No_Symbol and then Has_Parent (Iter) then
                  Default :=
                    Current_Data (Handler.Matcher, Parent (Iter)).Default;
               end if;
            end if;

            --  Unless we have a <any> type
            if Data.Simple /= No_Type_Index then
               Descr := Get_Type_Descr (NFA, Data.Simple);
               if Descr.Simple_Content /= No_Simple_Type_Index then
                  Whitespace := Get_Simple_Type
                    (Get_NFA (Handler.Grammar), Descr.Simple_Content)
                    .Whitespace;
               end if;
            end if;

            Next (Handler.Matcher, Iter);
         end loop;
      end;

      Is_Empty := Handler.Characters_Count = 0;

      if not Is_Empty then
         if Debug then
            Debug_Output ("Normalize whitespace: " & Whitespace'Img);
         end if;

         Normalize_Whitespace
           (Whitespace, Handler.Characters.all, Handler.Characters_Count);
      end if;

      --  in 3.3.1: if the element is empty, the "fixed" value
      --  should be used for it, just as for "default"
      --     Characters (Handler.all, Get (Get_Fixed (Handler)).all);

      if Is_Empty and then Fixed /= No_Symbol then
         Internal_Characters (Handler, Get (Fixed).all);
         Is_Empty := Handler.Characters_Count = 0;
         if Debug then
            Debug_Output
              ("Substitute fixed value for empty characters:"
               & Get (Fixed).all);
         end if;
      end if;

      --  If still empty, use the default value

      if Is_Empty and then Default /= No_Symbol then
         Internal_Characters (Handler, Get (Default).all);
         Is_Empty := Handler.Characters_Count = 0;
         if Debug then
            Debug_Output
              ("Substitute default value for empty characters:"
               & Get (Default).all);
         end if;
      end if;

      declare
         Iter : Active_State_Iterator :=
           For_Each_Active_State (Handler.Matcher,
                                  Ignore_If_Nested => True,
                                  Ignore_If_Default => True);
      begin
         loop
            S := Current (Handler.Matcher, Iter);
            exit when S = No_State;

            Ty := Current_Data (Handler.Matcher, Iter).Simple;

            if Ty /= No_Type_Index then
               Descr := Get_Type_Descr (NFA, Ty);

               if Descr.Simple_Content /= No_Simple_Type_Index then
                  if Debug and not Is_Empty then
                     Debug_Output
                       ("Validate characters ("
                        & To_QName (Descr.Name) & "): "
                        & Handler.Characters (1 .. Handler.Characters_Count)
                        & "--");
                  end if;

                  if Handler.Characters_Count = 0 then
                     Validate_Simple_Type
                       (Handler, Descr.Simple_Content,
                        "",
                        Loc           => Loc);
                  else
                     Validate_Simple_Type
                       (Handler, Descr.Simple_Content,
                        Handler.Characters (1 .. Handler.Characters_Count),
                        Loc           => Loc);
                  end if;

               elsif not Descr.Mixed
                 and then not Is_Empty
               then
                  if Debug then
                     Debug_Output ("No character data for "
                                   & To_QName (Descr.Name) & S'Img);
                     Debug_Output
                       ("Got "
                        & Handler.Characters
                          (1 .. Integer'Min (20, Handler.Characters_Count))
                        & "--");
                  end if;
                  Validation_Error
                    (Handler,
                     "No character data allowed by content model",
                     Loc);
               end if;

               --  We now know we have a valid character content, and we need
               --  to check it is equal to the fixed value. We also know that
               --  fixed matches the type, since it was checked when the XSD
               --  was parsed.

               if Fixed /= No_Symbol then
                  if Debug then
                     Debug_Output ("Element has fixed value: """
                                   & Get (Fixed).all & '"');
                  end if;

                  if Descr.Simple_Content /= No_Simple_Type_Index then
                     Is_Equal := Equal
                       (Reader      => Handler,
                        Simple_Type => Descr.Simple_Content,
                        Ch1 => Fixed,
                        Ch2 =>
                          Handler.Characters (1 .. Handler.Characters_Count));
                  else
                     Is_Equal := Get (Fixed).all =
                       Handler.Characters (1 .. Handler.Characters_Count);
                  end if;

                  if not Is_Equal then
                     Validation_Error
                       (Handler,
                        "Invalid character content (fixed to """
                        & Get (Fixed).all & """)");
                  end if;
               end if;
            end if;

            Next (Handler.Matcher, Iter);
         end loop;
      end;

      Handler.Characters_Count := 0;
   end Validate_Current_Characters;

   ------------------------
   -- Hook_Notation_Decl --
   ------------------------

   procedure Hook_Notation_Decl
     (Handler       : access Sax_Reader'Class;
      Name          : Unicode.CES.Byte_Sequence;
      Public_Id     : Unicode.CES.Byte_Sequence;
      System_Id     : Unicode.CES.Byte_Sequence)
   is
      pragma Unreferenced (Public_Id, System_Id);
      H : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);
   begin
      Add_Notation (Get_NFA (H.Grammar), Find_Symbol (H.all, Name));
   end Hook_Notation_Decl;

   ------------------------
   -- Hook_Start_Element --
   ------------------------

   procedure Hook_Start_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access;
      Atts    : in out Sax_Attribute_List)
   is
      H : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);
      No_Index       : constant Integer := Get_Index
        (Atts, H.XML_Instance_URI, H.No_Namespace_Schema_Location);
      Location_Index : constant Integer := Get_Index
        (Atts, H.XML_Instance_URI, H.Schema_Location);
      NFA            : constant Schema_NFA_Access := Get_NFA (H.Grammar);

      procedure Compute_Type_From_Attribute
        (Result_Index : out Type_Index;
         Result       : out Type_Descr_Access);
      --  If xsi:type was specified, verify that the given type is a valid
      --  substitution for the original type in the NFA, and replace the
      --  current nested automaton with the one for the type. The replacement
      --  does not affect the NFA itself, but the NFA_Matcher, so is only
      --  temporary and does not affect over running matchers.
      --
      --  Return the first state in the nested NFA to represent that type

      procedure Replace_State
        (Check_Substitution : Boolean;
         Nested_Start       : State;
         Simple             : Type_Index);
      --  Replace the current most nested NFA with [Nested_Start], to override
      --  the type. This might mean replacing a nested NFA or a state data,
      --  depending on whether we have a simpleType or complexType

      function Simple_Type_Data
        (Iter : Active_State_Iterator) return State_Data;
      --  return the simpleType data for the current state. This is either
      --  queries from the current state itself, or from its superstate if
      --  we are currently on the first state of the nested NFA.

      ----------------------
      -- Simple_Type_Data --
      ----------------------

      function Simple_Type_Data
        (Iter : Active_State_Iterator) return State_Data
      is
         S : constant State := Current (H.Matcher, Iter);
      begin
         if Has_Parent (Iter)
           and then Get_Start_State
             (NFA.Get_Nested (Current (H.Matcher, Parent (Iter)))) = S
         then
            return Current_Data (H.Matcher, Parent (Iter));
         else
            return Current_Data (H.Matcher, Iter);
         end if;
      end Simple_Type_Data;

      -------------------
      -- Replace_State --
      -------------------

      procedure Replace_State
        (Check_Substitution : Boolean;
         Nested_Start       : State;
         Simple             : Type_Index)
      is
         S         : State := No_State;
         Data      : State_Data;
         Iter      : Active_State_Iterator :=
           For_Each_Active_State
             (H.Matcher, Ignore_If_Default => True, Ignore_If_Nested => True);
         Internal_New_Nested : State := Nested_Start;
      begin
         loop
            S := Current (H.Matcher, Iter);
            exit when S = No_State;

            Data := Current_Data (H.Matcher, Iter);

            if Check_Substitution then
               Check_Substitution_Group_OK
                 (H, Simple,
                  Data.Simple,
                  Loc           => H.Current_Location,
                  Element_Block => Simple_Type_Data (Iter).Block);
            end if;

            if Nested_Start = No_State then
               --  Need to modify the nested NFA too: if we replaced a
               --  complexType ("anyType" for instance) with a simple type,
               --  we should no longer accept any element.
               --  However, if we simply disable all states in the nested
               --  NFA, that doesn't work either, since we will not accept
               --  the "close element" for the simpleType. But we cannot
               --  modify the NFA either, which should remain static.

               if Debug then
                  Debug_Output ("Override state data"
                                &  Current (H.Matcher, Iter)'Img
                                & " to type" & Simple'Img);
               end if;
               Override_Data
                 (H.Matcher, Iter,
                  State_Data'
                    (Simple   => Simple,
                     Nillable => Data.Nillable,
                     Fixed    => Data.Fixed,
                     Default  => Data.Default,
                     Block    => Data.Block));

               Internal_New_Nested := NFA.Simple_Nested;
               if Debug then
                  Debug_Output
                    ("Will replace nested complexType, to accept <close>");
               end if;
            end if;

            if Internal_New_Nested /= No_State then
               --  If we are on the first state of the parent, that means
               --  we just entered the parent (which is the element having
               --  the xsi:type). So we substitute the nested NFA *for the
               --  parent*.

               if Has_Parent (Iter) then
                  if Get_Start_State
                    (NFA.Get_Nested (Current (H.Matcher, Parent (Iter)))) = S
                  then
                     if Debug then
                        Debug_Output ("Replacing nested NFA");
                     end if;
                     Replace_State (H.Matcher, Iter, Internal_New_Nested);
                  end if;
               end if;
            end if;

            Next (H.Matcher, Iter);
         end loop;

         if Debug then
            Print (H.Matcher, Dump_Compact, "After substitution:");
         end if;
      end Replace_State;

      ---------------------------------
      -- Compute_Type_From_Attribute --
      ---------------------------------

      procedure Compute_Type_From_Attribute
        (Result_Index : out Type_Index;
         Result       : out Type_Descr_Access)
      is
         Xsi_Type_Index     : constant Integer := Get_Index
           (Atts, H.XML_Instance_URI, H.Typ);
         TRef : Global_Reference;
      begin
         if Xsi_Type_Index = -1 then
            Result_Index := No_Type_Index;
            Result       := null;
         else
            declare
               Qname : constant Byte_Sequence :=
                 Ada.Strings.Fixed.Trim
                   (Get (Get_Value (Atts, Xsi_Type_Index)).all,
                    Ada.Strings.Both);
               Separator : constant Integer := Split_Qname (Qname);
               Prefix    : Symbol;
               NS        : XML_NS;
               Typ       : Qualified_Name;
            begin
               Prefix := Find_Symbol
                 (H.all, Qname (Qname'First .. Separator - 1));
               Get_Namespace_From_Prefix (H.all, Prefix, NS);

               Typ := (NS    => Get_URI (NS),
                       Local => Find_Symbol
                         (H.all, Qname (Separator + 1 .. Qname'Last)));

               if Debug then
                  Debug_Output
                    ("Getting element definition from type attribute: "
                     & To_QName (Typ));
               end if;

               TRef := Reference_HTables.Get
                 (Get_References (H.Grammar).all, (Typ, Ref_Type));

               if TRef = No_Global_Reference then
                  Validation_Error (H, "Unknown type " & To_QName (Typ));
               end if;

               Result_Index := TRef.Typ;
               Result := Type_Descr_Access (Get_Type_Descr (NFA, TRef.Typ));
               Replace_State
                 (Check_Substitution => True,
                  Nested_Start => Result.Complex_Content,
                  Simple       => TRef.Typ);
            end;
         end if;
      end Compute_Type_From_Attribute;

      Success : Boolean;
      Nil_Index : Integer := -1;
      Nillable : Boolean := False;
      S       : State;
      Through_Any : Boolean;
      Through_Process : Process_Contents_Type;
      TRef    : Global_Reference;
      Xsi_Descr   : Type_Descr_Access;
      Xsi_Index   : Type_Index;

      Had_Matcher : constant Boolean := Is_Initialized (H.Matcher);

      Element_QName : constant Qualified_Name :=
        (NS    => Get_URI (Get_NS (Elem)),
         Local => Get_Local_Name (Elem));

   begin
      if Debug then
         Output_Seen ("Start_Element: " & To_QName (Element_QName)
                      & " " & To_String (H.Current_Location));
      end if;

      --  We should get the location of the enclosing element

      Validate_Current_Characters (H, Loc => Start_Tag_Location (Elem));

      --  Get the name of the grammar to use from the element's attributes

      if No_Index /= -1 then
         Parse_Grammar
           (H,
            URI      => Empty_String,
            Xsd_File => Get_Value (Atts, No_Index),
            Do_Create_NFA => True);
      end if;

      if Location_Index /= -1 then
         Parse_Grammars
           (H, Get_Value (Atts, Location_Index), Do_Create_NFA => True);
      end if;

      --  If we have an inline schema, we must check that the target NS
      --  is not used yet

      if Element_QName = (NS => H.XML_Schema_URI, Local => H.S_Schema)
        and then Had_Matcher
      then
         --  ??? Would need to include the contents into the NFA
         --  ??? And check that no element from the same namespace was seen
         Validation_Error
           (H,
            "Inline schema not supported",
            Except => XML_Not_Implemented'Identity);
      end if;

      if H.Grammar = No_Grammar then
         return;  --  Always valid, since we have no grammar anyway
      end if;

      --  Create the NFA matcher now if not done yet. This has to be done after
      --  we have seen the toplevel element, which might result in parsing
      --  additional grammars, and finding the target NS

      if not Had_Matcher then
         if Debug then
            Debug_Output ("Creating NFA matcher");
         end if;

         H.Matcher.Start_Match
           (On       => Get_NFA (H.Grammar),
            Start_At => Start_State);
      end if;

      Do_Match
        (Matcher         => H.Matcher,
         Sym             => (Closing => False, Name => Element_QName),
         Success         => Success,
         Through_Any     => Through_Any,
         Through_Process => Through_Process);

      if Debug then
         Print (H.Matcher, Dump_Compact, "After: ");
      end if;

      if not Had_Matcher and not Success then
         --  Seeing the toplevel is never incorrect. We just need to find
         --  out what its type would be, and use this for the matcher

         declare
            Descr : Type_Descr_Access;
            Index : Type_Index;
         begin
            Compute_Type_From_Attribute (Index, Descr);
            if Descr = null then
               Validation_Error
                 (H, "No type found for " & To_QName (Element_QName));
            elsif Descr.Complex_Content /= No_State then
               H.Matcher.Start_Match
                 (On       => Get_NFA (H.Grammar),
                  Start_At => Descr.Complex_Content);
            else
               --  Just expect a "close". The current active state, however,
               --  ends up with no state data, and we need to set it to the
               --  appropriate simpleType. Can't use Replace_State for this.

               H.Matcher.Start_Match
                 (Get_NFA (H.Grammar),
                  Start_At => NFA.Simple_Nested);

               declare
                  Iter : constant Active_State_Iterator :=
                    For_Each_Active_State
                      (H.Matcher, Ignore_If_Default => False,
                       Ignore_If_Nested => True);
                  Data      : State_Data;
               begin
                  Data := Current_Data (H.Matcher, Iter);
                  Override_Data
                    (H.Matcher, Iter,
                     State_Data'
                       (Simple   => Index,
                        Nillable => Data.Nillable,
                        Fixed    => Data.Fixed,
                        Default  => Data.Default,
                        Block    => Data.Block));
               end;
            end if;
         end;

      elsif not Success then
         Validation_Error
           (H, "Unexpected element """
            & To_QName (Element_QName) & """: expecting """
            & Expected (H.Matcher) & '"');
      end if;

      --  If we have a xsi:type attribute, modify the NFA to use that type

      Compute_Type_From_Attribute (Xsi_Index, Xsi_Descr);

      --  If the element matched a <any>, we might have to look it up to get
      --  its type. However, if a type was given through xsi:type, this is
      --  not needed since we already have a type.

      if Through_Any and then Xsi_Descr = null then
         case Through_Process is
            when Process_Skip =>
               --  Need to lookup the element to see whether it is nillable.
               --  Apparently, this aspect must be checked.
               --  Apart from that, this case is already handled in the NFA,
               --  and the state is setup as ur-Type
               TRef := Reference_HTables.Get
                 (Get_References (H.Grammar).all,
                  (Element_QName, Ref_Element));

               if TRef /= No_Global_Reference then
                  Nillable := NFA.Get_Data (TRef.Element).Nillable;
                  if Debug then
                     Debug_Output ("Getting nillable status from schema"
                                   & " even though we are in a <any skip>");
                  end if;
               end if;

            when Process_Lax =>
               TRef := Reference_HTables.Get
                 (Get_References (H.Grammar).all,
                  (Element_QName, Ref_Element));

            when Process_Strict =>
               --  Find the definition for this element, if possible

               TRef := Reference_HTables.Get
                 (Get_References (H.Grammar).all,
                  (Element_QName, Ref_Element));

               if TRef = No_Global_Reference then
                  Validation_Error
                    (H, "No definition found for "
                     & To_QName (Element_QName));
               end if;
         end case;

         if Through_Process /= Process_Skip
           and then TRef /= No_Global_Reference
         then
            --  Replace the current most nested state in the machine with the
            --  new type

            if Debug then
               Debug_Output ("Found valid declaration for "
                             & To_QName (Element_QName));
            end if;

            Replace_State
              (Check_Substitution => False,
               Nested_Start => Get_Start_State (NFA.Get_Nested (TRef.Element)),
               Simple       => NFA.Get_Data (TRef.Element).Simple);
         end if;

      else
         Through_Process := Process_Strict;
      end if;

      --  Validate the attributes

      declare
         Iter : Active_State_Iterator :=
           For_Each_Active_State
             (H.Matcher, Ignore_If_Nested => True, Ignore_If_Default => True);
         Data : State_Data;
         Fixed : Symbol := No_Symbol;
      begin
         loop
            S := Current (H.Matcher, Iter);
            exit when S = No_State;

            --  The list of valid attributes is attached to the type, that is
            --  to the nested NFA.

            Data := Simple_Type_Data (Iter);

            if Fixed = No_Symbol then
               Fixed := Data.Fixed;
            end if;

            if Debug then
               Debug_Output ("Checking attributes for state" & S'Img
                             & " type_index=" & Data.Simple'Img);
            end if;

            Nillable := Nillable or Data.Nillable;

            --  otherwise with have a <any> type
            if Data.Simple /= No_Type_Index then

               --  Check whether the actual type is abstract. This cannot be
               --  checked when the grammar is created because of
               --  substitutionGroup and xsi:type

               declare
                  Descr : constant access Type_Descr :=
                    NFA.Get_Type_Descr (Data.Simple);
               begin
                  if Descr.Is_Abstract then
                     if Descr.Name /= No_Qualified_Name then
                        Validation_Error
                          (H,
                           "Type " & To_QName (Descr.Name) & " is abstract");
                     else
                        Validation_Error (H, "Type is abstract");
                     end if;
                  end if;

                  Validate_Attributes
                    (Get_NFA (H.Grammar),
                     Descr,
                     H, Atts,
                     Is_Nil    => Nil_Index);
               end;
            else
               if Debug then
                  Debug_Output ("A <anyType>, all attributes are valid");
               end if;

               Nil_Index :=
                 Get_Index (Atts, H.XML_Instance_URI, H.Nil);
            end if;

            Next (H.Matcher, Iter);
         end loop;

         if Through_Process = Process_Skip then
            --  In this case, we do not want to check the contents. Even if
            --  xsi:nil="true" was specified, we still need to accept when
            --  contents was provided.
            H.Is_Nil := False;

         else
            if Nil_Index /= -1 then
               if not Nillable then
                  Validation_Error (H, "Element cannot be nil");
               end if;
               H.Is_Nil := Get_Value_As_Boolean (Atts, Nil_Index);
            else
               H.Is_Nil := False;
            end if;

            if H.Is_Nil then
               if Fixed /= No_Symbol then
                  Validation_Error
                    (H, "Element cannot be nilled because"
                     & " a fixed value is defined for it");
               end if;

               if Debug then
                  Debug_Output
                    ("Element is nil, should we replace nested NFA");
               end if;

               Replace_State
                 (Check_Substitution => False,
                  Nested_Start       => NFA.Simple_Nested,
                  Simple             => Data.Simple);
            end if;
         end if;
      end;
   end Hook_Start_Element;

   ----------------------
   -- Hook_End_Element --
   ----------------------

   procedure Hook_End_Element
     (Handler : access Sax_Reader'Class;
      Elem    : Element_Access)
   is
      H : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);
      Success : Boolean;
      Through_Any : Boolean;
      Through_Process : Process_Contents_Type;
   begin
      if Debug then
         Output_Seen
           ("End_Element: "
            & To_QName (Elem) & " " & To_String (H.Current_Location));
      end if;

      Validate_Current_Characters (H, Loc => Start_Tag_End_Location (Elem));

      Do_Match
        (H.Matcher,
         Sym => (Closing => True,
                 Name    => (NS    => Get_URI (Get_NS (Elem)),
                             Local => Get_Local_Name (Elem))),
         Success         => Success,
         Through_Any     => Through_Any,
         Through_Process => Through_Process);

      if Debug then
         Print (H.Matcher, Dump_Compact, "After end element: ");
      end if;

      if not Success then
         Validation_Error
           (H,
            "Unexpected end of sequence, expecting """
            & Expected (H.Matcher) & '"');
      end if;

      --  We know the parent wasn't nil, since the child was accepted
      H.Is_Nil := False;
   end Hook_End_Element;

   -------------------------
   -- Internal_Characters --
   -------------------------

   procedure Internal_Characters
     (Handler : access Validating_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      Tmp : Byte_Sequence_Access;
      Max : constant Natural := Handler.Characters_Count + Ch'Length;
   begin
      --  Preserve the characters, but avoid allocating every time. We
      --  therefore reuse the buffer as much as possible, and only extend it
      --  when needed.

      if Handler.Characters = null then
         Handler.Characters_Count := Ch'Length;
         Handler.Characters := new String (1 .. Ch'Length);
         Handler.Characters.all := Ch;

      elsif Max <= Handler.Characters'Last then
         Handler.Characters (Handler.Characters_Count + 1 .. Max) := Ch;
         Handler.Characters_Count := Max;

      else
         Tmp := new String (1 .. Max);
         Tmp (1 .. Handler.Characters_Count) :=
           Handler.Characters (1 .. Handler.Characters_Count);
         Tmp (Handler.Characters_Count + 1 .. Max) := Ch;
         Handler.Characters_Count := Max;
         Free (Handler.Characters);
         Handler.Characters := Tmp;
      end if;
   end Internal_Characters;

   ---------------------
   -- Hook_Characters --
   ---------------------

   procedure Hook_Characters
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence) is
   begin
      Internal_Characters (Validating_Reader_Access (Handler), Ch);
   end Hook_Characters;

   -------------------------------
   -- Hook_Ignorable_Whitespace --
   -------------------------------

   procedure Hook_Ignorable_Whitespace
     (Handler : access Sax_Reader'Class;
      Ch      : Unicode.CES.Byte_Sequence)
   is
      H     : constant Validating_Reader_Access :=
        Validating_Reader_Access (Handler);
      NFA   : constant Schema_NFA_Access := Get_NFA (H.Grammar);
      S     : State;
      Descr : access Type_Descr;
      Iter  : Active_State_Iterator := For_Each_Active_State
        (H.Matcher, Ignore_If_Nested => True, Ignore_If_Default => True);

   begin
      loop
         S := Current (H.Matcher, Iter);
         exit when S = No_State;

         Descr := Get_Type_Descr (NFA, Current_Data (H.Matcher, Iter).Simple);
         if Descr.Simple_Content /= No_Simple_Type_Index
           or else Descr.Mixed
         then
            Internal_Characters (H, Ch);
            return;
         end if;

         Next (H.Matcher, Iter);
      end loop;
   end Hook_Ignorable_Whitespace;

   -----------
   -- Reset --
   -----------

   procedure Reset (Parser : in out Validating_Reader) is
   begin
      --  Save current location, for retrieval by Get_Error_Message
      Free (Parser.Id_Table);
      Free (Parser.Matcher);
      Free (Parser.Characters);
      Parser.Characters_Count := 0;
   end Reset;

   -----------
   -- Parse --
   -----------

   overriding procedure Parse
     (Parser : in out Validating_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      if Debug then
         Output_Action
           ("Parsing XML file " & Input_Sources.Get_System_Id (Input));
      end if;

      Initialize_Symbols (Parser);
      Initialize_Grammar (Parser);

      if Get_Feature (Parser, Schema_Validation_Feature) then
         Set_Hooks (Parser,
                    Start_Element => Hook_Start_Element'Access,
                    End_Element   => Hook_End_Element'Access,
                    Characters    => Hook_Characters'Access,
                    Whitespace    => Hook_Ignorable_Whitespace'Access,
                    Notation_Decl => Hook_Notation_Decl'Access);
         Free (Parser.Matcher);
      else
         Set_Hooks (Parser,
                    Start_Element => null,
                    End_Element   => null,
                    Characters    => null,
                    Whitespace    => null,
                    Doc_Locator   => null);
      end if;

      --  Not a dispatching call
      Parse (Schema.Validators.Abstract_Validation_Reader (Parser), Input);

      if not In_Final (Parser.Matcher) then
         Validation_Error
           (Parser'Access,
            "Unexpected end of file: expecting "
            & Expected (Parser.Matcher));
      end if;

      Reset (Parser);

   exception
      when others =>
         Reset (Parser);
         raise;
   end Parse;

   -------------------------------
   -- Get_Namespace_From_Prefix --
   -------------------------------

   procedure Get_Namespace_From_Prefix
     (Handler  : in out Validating_Reader;
      Prefix   : Symbol;
      NS       : out Sax.Utils.XML_NS) is
   begin
      Find_NS
        (Parser  => Handler,
         Prefix  => Prefix,
         NS      => NS);
      if Get_URI (NS) = Empty_String then
         NS := No_XML_NS;
      end if;
   end Get_Namespace_From_Prefix;

   ----------
   -- Free --
   ----------

   procedure Free (Reader : in out Validating_Reader_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Validating_Reader'Class, Validating_Reader_Access);
   begin
      if Reader /= null then
         Free (Reader.all);
         Unchecked_Free (Reader);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Reader : in out Validating_Reader) is
   begin
      Free (Schema.Validators.Abstract_Validation_Reader (Reader));
   end Free;
end Schema.Readers;
