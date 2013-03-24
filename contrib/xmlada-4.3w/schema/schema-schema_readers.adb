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

with Ada.Exceptions;    use Ada.Exceptions;
with GNAT.Task_Lock;    use GNAT.Task_Lock;
with Unicode;           use Unicode;
with Unicode.CES;       use Unicode.CES;
with Sax.Encodings;     use Sax.Encodings;
with Sax.Exceptions;    use Sax.Exceptions;
with Sax.Locators;      use Sax.Locators;
with Sax.Symbols;       use Sax.Symbols;
with Sax.Utils;         use Sax.Utils;
with Schema.Simple_Types; use Schema.Simple_Types;
with Schema.Readers;      use Schema.Readers;
with Ada.Unchecked_Deallocation;
with Ada.IO_Exceptions;
with Input_Sources.File;  use Input_Sources.File;

package body Schema.Schema_Readers is
   use Schema_State_Machines, Schema_State_Machines_PP;
   use Type_Tables, Element_HTables, Group_HTables;
   use AttrGroup_HTables, Reference_HTables, Attribute_HTables;

   Default_Contexts : constant := 30;
   --  Default number of nested levels in a schema.
   --  If the actual schema uses more, we will simply reallocate some memory.

   Max_Max_Occurs : constant := 300;
   --  Maximum value for maxOccurs.
   --  Higher values result in an explosion in the number of states in the NFA,
   --  so should not be used for now.

   procedure Push_Context
     (Handler : access Schema_Reader'Class; Ctx : Context);
   --  Add a new context to the list

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Attr_Array, Attr_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Context_Array, Context_Array_Access);

   procedure Free (Shared : in out XSD_Data_Access);
   --  Free [Shared] and its htables

   procedure Free (Self : in out Type_Details_Access);
   --  Free [Self], [Self.Next] and so on

   procedure Create_NFA (Parser : access Schema_Reader);
   --  Create the state machine from the registered elements and types
   --  Return the start state for the current grammar (we do not use the
   --  NFA's default start state, since each grammar has its own list of valid
   --  toplevel elements.

   function To_String (Final  : Final_Status) return String;

   function In_Redefine_Context
     (Handler : Schema_Reader'Class) return Boolean;
   --  Whether we are currently processing a <redefine> tag

   function Resolve_QName
     (Handler     : access Schema_Reader'Class;
      QName       : Sax.Symbols.Symbol;
      NS_If_Empty : Sax.Symbols.Symbol := Empty_String;
      Loc         : Location) return Qualified_Name;
   --  Resolve namespaces for QName.
   --  [NS_If_Empty] is used if no namespace was found for the element. This
   --  will often be the target namespace of the schema.

   procedure Internal_Parse
     (Parser            : in out Schema_Reader;
      Input             : in out Input_Sources.Input_Source'Class;
      Default_Namespace : Symbol;
      Do_Create_NFA     : Boolean;
      Do_Initialize_Shared : Boolean);
   --  Internal version of [Parse], which allows reuse of the shared data.
   --  This is useful while parsing a <include> XSD

   procedure Insert_In_Type
     (Handler : access Schema_Reader'Class;
      Element : in out Type_Details_Access);
   --  Insert Element in the type definition in [Handler.Contexts].
   --  If there is an error inserting the element, an exception is raised and
   --  [Element] is freed.

   procedure Prepare_Type
     (Handler   : access Schema_Reader'Class;
      Atts      : Sax_Attribute_List;
      Is_Simple : Boolean);
   --  Prepare a type context (simpleType or complexType)

   procedure Add_Type_Member
     (Handler : access Schema_Reader'Class;
      List    : in out Type_Member_Array;
      Member  : Type_Member;
      Loc     : Location);
   --  Add a new item in [List], and raise an exception if [List] is full.

   procedure Compute_Blocks
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Blocks  : out Block_Status;
      Is_Set  : out Boolean;
      Index   : Integer);
   --  Compute the list of blocked elements from the attribute "block".

   function Compute_Final
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Index   : Integer) return Final_Status;
   --  Compute the list of final attributes from value. Value is a list similar
   --  to what is used for the "final" attribute of elements in a schema

   function Compute_Form
     (Atts      : Sax_Attribute_List;
      Handler   : access Schema_Reader'Class;
      Index     : Integer) return Form_Type;
   --  Parse the given attribute

   procedure Append (List : in out Attr_Array_Access; Attr : Attr_Descr);
   --  Add an attribute to the list

   procedure Insert_Attribute
     (Handler        : access Schema_Reader'Class;
      In_Context     : Natural;
      Attribute      : Attr_Descr);
   --  Insert attribute at the right location in In_Context.

   function Process_Contents_From_Atts
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Index   : Integer) return Process_Contents_Type;
   --  Get the value of processContents from the attributes

   procedure Create_Element
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Notation
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Complex_Type
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Simple_Type
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Restriction
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_All
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Sequence
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Attribute
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Schema
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Extension
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_List
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Union
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Choice
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Redefine
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Include
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Group
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Attribute_Group
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Any
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Import
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   procedure Create_Any_Attribute
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List);
   --  Create a new context for a specific tag:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <attribute>, <schema>, <extension>, <list>, <union>, <choice>,
   --  <redefine>, <group>, <attributeGroup>, <any>, <import>, <anyAttribute>

   procedure Finish_Element      (Handler : access Schema_Reader'Class);
   procedure Finish_Complex_Type (Handler : access Schema_Reader'Class);
   procedure Finish_Simple_Type  (Handler : access Schema_Reader'Class);
   procedure Finish_Restriction  (Handler : access Schema_Reader'Class);
   procedure Finish_Extension    (Handler : access Schema_Reader'Class);
   procedure Finish_Attribute    (Handler : access Schema_Reader'Class);
   procedure Finish_Union        (Handler : access Schema_Reader'Class);
   procedure Finish_List         (Handler : access Schema_Reader'Class);
   procedure Finish_Group        (Handler : access Schema_Reader'Class);
   procedure Finish_Attribute_Group (Handler : access Schema_Reader'Class);
   --  Finish the handling of various tags:
   --  resp. <element>, <complexType>, <restriction>, <all>, <sequence>,
   --  <extension>, <union>, <list>, <choice>, <group>

   procedure Get_Occurs
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Min_Occurs, Max_Occurs : out Occurrences);
   --  Get the "minOccurs" and "maxOccurs" attributes

   ---------------
   -- To_String --
   ---------------

   function To_String (Final : Final_Status) return String is
   begin
      return "restr=" & Final (Final_Restriction)'Img
        & " ext=" & Final (Final_Extension)'Img
        & " union=" & Final (Final_Union)'Img
        & " list=" & Final (Final_List)'Img;
   end To_String;

   -------------------------
   -- In_Redefine_Context --
   -------------------------

   function In_Redefine_Context
     (Handler : Schema_Reader'Class) return Boolean is
   begin
      for J in 1 .. Handler.Contexts_Last loop
         if Handler.Contexts (J).Typ = Context_Redefine then
            return True;
         end if;
      end loop;
      return False;
   end In_Redefine_Context;

   ----------------
   -- Create_NFA --
   ----------------

   procedure Create_NFA (Parser : access Schema_Reader) is
      NFA : constant Schema_NFA_Access := Get_NFA (Get_Grammar (Parser.all));
      Ref : constant Reference_HTable :=
        Get_References (Get_Grammar (Parser.all));

      Any_Simple_Type_Index : constant Type_Index :=
        Get (Ref.all,
          ((Local => Parser.Any_Simple_Type,
            NS    => Parser.XML_Schema_URI), Ref_Type)).Typ;

      Shared : XSD_Data_Access renames Parser.Shared;

      package Type_HTables is new GNAT.Dynamic_HTables.Simple_HTable
        (Header_Num => Header_Num,
         Element    => Internal_Type_Index,
         No_Element => No_Internal_Type_Index,
         Key        => Qualified_Name,
         Hash       => Hash,
         Equal      => "=");
      use Type_HTables;
      Types : Type_HTables.Instance;

      procedure Process_Global_Element
        (Info : in out Element_Descr; Start_At : State);
      procedure Process_Type (Info : in out Internal_Type_Descr);
      procedure Process_Details
        (In_Type    : access Type_Descr;
         Details    : Type_Details_Access;
         Start, From : State;
         Nested_End : out State;
         Mask        : out Visited_All_Children);
      --  [Start] is the start of the current nested.
      --  [Mask] is the mask to apply to a closing transition out of a <all>
      --  node. It is only set if Details.Kind = Type_All.

      procedure Create_Element_State
        (Info : in out Element_Descr;
         Start, From : State;
         Global : Boolean;
         S1, S2 : out State;
         Trans_Kind : Transition_Kind;
         All_Child_Index : Integer := 0);
      --  Create (and decorate) the nodes [S1]..[S2] corresponding to an
      --  <element>. A link is created [From]->[S1].
      --  [Start] is the first element of the current nested machine.
      --  Trans_Kind is the kind of the first transition, from From to the
      --  first created state. This transition is associated with the name of
      --  the element.
      --  All_Child_Index indicates the index of the created transition in all
      --  the children of an <all> node. This is used to memory efficiently
      --  which transitions have already been visited for this <all>.

      type Type_Descr_Access is access all Type_Descr;
      procedure Create_Simple_Type
        (J     : Internal_Type_Index;
         Descr : out Type_Descr_Access;
         Index : out Type_Index);
      --  Create the new simple type info at index [J]

      procedure Get_Type_Descr
        (Name          : Qualified_Name;
         Loc           : Location;
         NFA_Type      : out Type_Index;
         Internal_Type : out Internal_Type_Index);
      --  Lookup the type information in the grammar. This type information
      --  could be in several places:
      --    - Either defined in the current XSD and its <input>: in that case,
      --      [Internal_Type] will be set.
      --    - Or in a previously loaded XSD. In that case, it is set to
      --      [No_Internal_Type_Index]

      procedure Lookup_Simple_Type
        (Name  : Qualified_Name;
         Loc   : Location;
         Descr : out Type_Descr_Access;
         Index : out Type_Index);
      --  Search for the simpleType [Name]

      procedure Add_Attributes
        (List             : in out Attributes_List;
         Attrs            : Attr_Array_Access;
         Processed_Groups : in out AttrGroup_HTables.Instance;
         As_Restriction   : Boolean;
         Had_Any          : in out Boolean);
      --  Create List from the list of attributes or attribute groups in
      --  [Attrs]. [Had_Any] is set to true if a <anyAttribute> was
      --  encountered. The caller should first set it to False.

      procedure Check_Unique_Particle_Attribution
        (Details : Type_Details_Access);
      --  Check that all elements in the content model are unique (we must not
      --  have two elements that compete with each other).

      procedure Create_Global_Attributes (Attr : Internal_Attribute_Descr);
      --  Register a global attribute.

      procedure Resolve_Attribute_Type
        (Attr : in out Internal_Attribute_Descr;
         Loc  : Sax.Locators.Location);
      --  Set [Attr.Descr.Simple_Type] to the appropriate value, after looking
      --  up the type

      --------------------------
      -- Create_Element_State --
      --------------------------

      procedure Create_Element_State
        (Info : in out Element_Descr;
         Start, From : State;
         Global : Boolean;
         S1, S2 : out State;
         Trans_Kind : Transition_Kind;
         All_Child_Index : Integer := 0)
      is
         pragma Unreferenced (Start);
         Real      : Element_Descr;
         Trans     : Transition_Descr;
         TRef      : Global_Reference := No_Global_Reference;
         S         : State := No_State;

         function Build_Trans
           (N : Qualified_Name; F : Form_Type) return Transition_Descr;
         function Build_Trans
           (N : Qualified_Name; F : Form_Type) return Transition_Descr is
         begin
            case Trans_Kind is
               when Transition_Symbol =>
                  return (Transition_Symbol, N, F, All_Child_Index => 0);
               when Transition_Symbol_From_All =>
                  return (Transition_Symbol_From_All, N, F,
                          All_Child_Index => All_Child_Index);
               when others =>
                  raise Program_Error with "Invalid transition type";
            end case;
         end Build_Trans;

      begin
         if Info.Is_Abstract then
            --  ??? Should use the substitutionGroup elements, instead
            S1 := No_State;
            S2 := No_State;

            Validation_Error
              (Parser,
               "Abstract elements not handled yet",
               Except => XML_Not_Implemented'Identity);
            return;
         end if;

         S1 := NFA.Add_State;
         Info.S := S1;

         if Debug then
            Debug_Output ("Create_Element_State S1 for element "
                          & To_QName (Info.Name) & To_QName (Info.Ref)
                          & S1'Img);
         end if;

         --  Resolve element references

         if Info.Name = No_Qualified_Name then
            Real := Get (Shared.Global_Elements, Info.Ref);
            if Real = No_Element_Descr then
               TRef := Get (Ref.all, (Info.Ref, Ref_Element));
               if TRef = No_Global_Reference then
                  Validation_Error
                    (Parser, "Unknown refed element " & To_QName (Info.Ref),
                     Info.Loc);
               end if;

               S := TRef.Element;
               Trans := Build_Trans (Info.Ref, Info.Form);

            else
               Trans := Build_Trans (Real.Name, Info.Form);
               S := Real.S;
            end if;

            if S /= No_State then
               if Debug then
                  Debug_Output ("copying data from" & S'Img & " to" & S1'Img);
               end if;
               NFA.Get_Data (S1).all := NFA.Get_Data (S).all;
               NFA.Set_Nested (S1, NFA.Get_Nested (S));
            end if;

         else
            declare
               NFA_Type      : Type_Index;
               Internal_Type : Internal_Type_Index;
               Data          : access State_Data;
            begin
               Real := Info;
               Trans := Build_Trans (Real.Name, Info.Form);

               --  Create nested NFA for the type, if needed

               if Real.Typ /= No_Qualified_Name then
                  Get_Type_Descr (Real.Typ, Info.Loc, NFA_Type, Internal_Type);

               elsif Real.Local_Type /= No_Internal_Type_Index then
                  Internal_Type := Real.Local_Type;
                  NFA_Type := Shared.Types.Table (Internal_Type).In_NFA;
               else
                  --  "<anyType>" (3.3.2.1 {type definition})
                  NFA_Type := No_Type_Index;
               end if;

               if Info.Substitution_Group /= No_Qualified_Name then
                  --  ??? Handling of substitutionGroup: the type of the
                  --  element is the same as the head unless overridden.
                  Validation_Error
                    (Parser,
                     "substitutionGroup not supported",
                     Except => XML_Not_Implemented'Identity);
               end if;

               Data := NFA.Get_Data (S1);

               if NFA_Type /= No_Type_Index then
                  Data.all := State_Data'(Simple   => NFA_Type,
                                          Fixed    => No_Symbol, --  See below
                                          Default  => Info.Default,
                                          Nillable => Info.Nillable,
                                          Block    => Info.Block);
                  NFA.Set_Nested
                    (S1, NFA.Create_Nested
                       (Get_Type_Descr (NFA, NFA_Type).Complex_Content));
               else
                  NFA.Set_Nested (S1, NFA.Ur_Type (Process_Lax));
                  Data.Default  := Info.Default;
                  Data.Block    := Info.Block;
                  Data.Nillable := Info.Nillable;
               end if;

               --  Check that the fixed value is valid

               if Info.Fixed /= No_Symbol
                 and then NFA.Get_Data (S1).Simple /= No_Type_Index
               then
                  Normalize_And_Validate
                    (Parser => Parser,
                     Simple => Get_Type_Descr
                       (NFA, NFA.Get_Data (S1).Simple).Simple_Content,
                     Fixed  => Info.Fixed,
                     Loc    => Info.Loc);
               end if;

               Data.Fixed := Info.Fixed;
            end;
         end if;

         --  Link with previous element

         NFA.Add_Transition (From, S1, Trans);

         S2 := NFA.Add_State;

         if NFA.Get_Nested (S1) /= No_Nested then
            NFA.On_Empty_Nested_Exit (S1, S2);  --  a complexType
         else
            NFA.Add_Transition (S1, S2, (Kind => Transition_Close));
         end if;

         --  Save this element for later reuse in other namespaces

         if Global then
            if Debug then
               Debug_Output ("Global elem: " & To_QName (Real.Name));
            end if;
            Set (Ref.all,
                 (Kind => Ref_Element, Name => Real.Name, Element => S1));
         end if;
      end Create_Element_State;

      ----------------------------
      -- Process_Global_Element --
      ----------------------------

      procedure Process_Global_Element
        (Info : in out Element_Descr; Start_At : State)
      is
         S1, S2 : State;
      begin
         Create_Element_State
           (Info, Start_At, Start_At, True, S1, S2, Transition_Symbol);

         if S2 /= No_State then
            NFA.Add_Empty_Transition (S2, Final_State);
         end if;
      end Process_Global_Element;

      --------------------
      -- Get_Type_Descr --
      --------------------

      procedure Get_Type_Descr
        (Name          : Qualified_Name;
         Loc           : Location;
         NFA_Type      : out Type_Index;
         Internal_Type : out Internal_Type_Index)
      is
         TRef : Global_Reference;
      begin
         Internal_Type := Get (Types, Name);
         if Internal_Type = No_Internal_Type_Index then
            TRef := Get (Ref.all, (Name, Ref_Type));
            if TRef = No_Global_Reference then
               Validation_Error
                 (Parser, "Unknown type " & To_QName (Name), Loc);
            end if;

            NFA_Type := TRef.Typ;
         else
            NFA_Type := Shared.Types.Table (Internal_Type).In_NFA;
         end if;

         if Name = (NS => Parser.XML_Schema_URI, Local => Parser.IDREF)
           or else
             Name = (NS => Parser.XML_Schema_URI, Local => Parser.IDREFS)
         then
            Validation_Error
              (Parser,
               "Unsupported type IDREF and IDREFS",
               Loc => Loc,
               Except => XML_Not_Implemented'Identity);
         end if;
      end Get_Type_Descr;

      ------------------------
      -- Lookup_Simple_Type --
      ------------------------

      procedure Lookup_Simple_Type
        (Name  : Qualified_Name;
         Loc   : Location;
         Descr : out Type_Descr_Access;
         Index : out Type_Index)
      is
         TRef     : Global_Reference;
         Internal : Internal_Type_Index;
      begin
         TRef := Get (Ref.all, (Name, Ref_Type));
         if TRef = No_Global_Reference then
            Validation_Error
              (Parser, "Unknown type " & To_QName (Name), Loc);
         end if;

         Index := TRef.Typ;
         Descr := Type_Descr_Access (Get_Type_Descr (NFA, Index));

         if Descr.Simple_Content = No_Simple_Type_Index then
            if Debug then
               Debug_Output ("Lookup_Simple_Type: generate "
                             & To_QName (Name) & " early");
            end if;

            Internal := Get (Types, Name);
            if Internal = No_Internal_Type_Index then
               Validation_Error
                 (Parser,
                  "Type is not a simple type: " & To_QName (Name), Loc);
            end if;

            Create_Simple_Type (Internal, Descr, Index);
         end if;
      end Lookup_Simple_Type;

      ---------------------------------------
      -- Check_Unique_Particle_Attribution --
      ---------------------------------------

      procedure Check_Unique_Particle_Attribution
        (Details : Type_Details_Access)
      is
         Duplicates : Element_HTables.Instance;
         --  Used to check for duplicate elements within a sequence or choice.
         --  ??? Could use a htable of locations, so that we can point to the
         --  two duplicate declarations.

         T     : Type_Details_Access;
         Elem  : Element_Descr;

      begin
         case Details.Kind is
            when Type_Sequence =>
               T := Details.First_In_Seq;
            when Type_Choice =>
               T := Details.First_In_Choice;
            when Type_All =>
               T := Details.First_In_All;
            when others =>
               raise Program_Error with "Internal error";
         end case;

         while T /= null loop
            case T.Kind is
               when Type_Element =>
                  if T.Element.Name /= No_Qualified_Name then
                     Elem := Element_HTables.Get (Duplicates, T.Element.Name);

                     if Elem /= No_Element_Descr then
                        --  It is always invalid to have an element with the
                        --  same name but with different types in the same
                        --  model, even if there is no ambiguity (for instance
                        --  in a sequence).

                        if Elem.Typ /= No_Qualified_Name
                          and then Elem.Typ /= T.Element.Typ
                        then
                           Validation_Error
                             (Parser,
                              "Multiple elements with name '"
                              & To_QName (T.Element.Name)
                              & "', with different types, appear in the model"
                              & " group",
                              Details.Loc);
                        end if;

                        --  In a <choice> or <all>, we cannot have the same
                        --  element multiple times, since that would be
                        --  ambiguous.

                        if Details.Kind = Type_Choice
                          or else Details.Kind = Type_All
                        then
                           Validation_Error
                             (Parser,
                              "'" & To_QName (T.Element.Name) & "' and '"
                              & To_QName (Elem.Name) & "' violate the Unique"
                              & " Particle Attribution rule, creating an"
                              & " ambiguity for the validation",
                              Details.Loc);
                        end if;

                     else
                        Element_HTables.Set
                          (Duplicates, T.Element.Name, T.Element);
                     end if;
                  end if;

               when Type_Group =>
                  null;

               when others =>
                  --  ??? Should check, in particular for groups or nested
                  --  sequences (we can have a Sequence that contains choices)
                  null;
            end case;

            T := T.Next;
         end loop;

         Reset (Duplicates);

      exception
         when others =>
            Reset (Duplicates);
            raise;
      end Check_Unique_Particle_Attribution;

      ---------------------
      -- Process_Details --
      ---------------------

      procedure Process_Details
        (In_Type     : access Type_Descr;
         Details     : Type_Details_Access;
         Start, From : State;
         Nested_End  : out State;
         Mask        : out Visited_All_Children)
      is
         S, S1 : State;
         T     : Type_Details_Access;
         Gr    : Group_Descr;

      begin
         Nested_End := From;
         Mask := 0;

         if Details = null then
            return;
         end if;

         Details.In_Process := True;

         case Details.Kind is
            when Type_Empty =>
               null;

            when Type_Sequence =>
               Check_Unique_Particle_Attribution (Details);

               S := From;
               T := Details.First_In_Seq;
               while T /= null loop
                  Process_Details (In_Type, T, Start, S, Nested_End, Mask);
                  S := Nested_End;
                  T := T.Next;
               end loop;

            when Type_Choice =>
               Check_Unique_Particle_Attribution (Details);

               T := Details.First_In_Choice;
               Nested_End := NFA.Add_State;
               while T /= null loop
                  Process_Details (In_Type, T, Start, From, S, Mask);
                  NFA.Add_Empty_Transition (S, Nested_End);
                  T := T.Next;
               end loop;

            when Type_All =>
               Check_Unique_Particle_Attribution (Details);

               if Details.First_In_All /= null then
                  declare
                     Count : Natural := 0;
                  begin
                     Mask := 0;
                     T := Details.First_In_All;
                     while T /= null loop
                        pragma Assert
                          (T.Kind = Type_Element,
                           "Children of <all> must be simple elements");

                        --  If the element has maxOccurs=0, there is no need to
                        --  put it in the state machine.

                        if T.Max_Occurs.Value /= 0 then
                           Create_Element_State
                             (T.Element, Start, From, False,
                              S1 => S1,
                              S2 => S,
                              Trans_Kind => Transition_Symbol_From_All,
                              All_Child_Index => Count);

                           if T.Min_Occurs.Value = 1 then
                              --  The child is mandatory
                              Mask := Mask or (2 ** Count);
                           end if;

                           Count := Count + 1;

                           --  All elements, after being processed, come back
                           --  to <all> itself. The latter is in charge of
                           --  deciding, through the subprogram Match, which
                           --  transitions are valid in the current state.
                           NFA.Add_Empty_Transition (S, Start);
                        end if;

                        T := T.Next;
                     end loop;

                     --  The actual end of <all> is through a conditional empty
                     --  transition controlled by Match.

                     Nested_End := NFA.Add_State;
                     NFA.Add_Empty_Transition (From, Nested_End);
                  end;
               end if;

            when Type_Element =>
               Create_Element_State
                 (Details.Element, Start, From, False, S1, Nested_End,
                  Transition_Symbol);

            when Type_Group =>
               Gr := Get (Parser.Shared.Global_Groups, Details.Group.Ref);
               if Gr = No_Group_Descr then
                  Validation_Error
                    (Parser,
                     "No group """ & To_QName (Details.Group.Ref) & '"',
                     Details.Group.Loc);

               elsif Gr.Details.In_Process then
                  Validation_Error
                    (Parser,
                     "Circular group reference for "
                     & To_QName (Details.Group.Ref),
                     Details.Group.Loc);
               end if;

               Process_Details
                 (In_Type, Gr.Details, Start, From, Nested_End, Mask);

            when Type_Extension =>
               declare
                  NFA_Type      : Type_Index;  --  Attributes of the base type
                  Internal_Type : Internal_Type_Index;
                  Base_Descr    : access Type_Descr;
               begin
                  Get_Type_Descr
                    (Name          => Details.Extension.Base,
                     Loc           => Details.Extension.Loc,
                     NFA_Type      => NFA_Type,
                     Internal_Type => Internal_Type);

                  Base_Descr := Get_Type_Descr (NFA, NFA_Type);

                  if Base_Descr.Final (Final_Extension) then
                     Validation_Error
                       (Parser,
                        To_QName (Base_Descr.Name)
                        & " is final for extensions",
                        Details.Extension.Loc);
                  end if;

                  if Base_Descr.Simple_Content = No_Simple_Type_Index then
                     if Internal_Type /= No_Internal_Type_Index then
                        --  We have all the details, and just have to copy them
                        --  Details might be null, for instance for an
                        --  <extension> that just adds attributes

                        if Shared.Types.Table (Internal_Type).Details /= null
                          and then
                            Shared.Types.Table (Internal_Type)
                            .Details.In_Process
                        then
                           Validation_Error
                             (Parser,
                              "Circular inheritance of type "
                              & To_QName (Details.Extension.Base),
                              Details.Extension.Loc);
                        end if;

                        Process_Details
                          (In_Type,
                           Shared.Types.Table (Internal_Type).Details,
                           Start, From, S, Mask);

                     else
                        --  ??? Should copy the nested NFA for TyS
                        Validation_Error
                          (Parser,
                           "Extension's base in a different file "
                           & To_QName (Details.Extension.Base),
                           Details.Extension.Loc,
                           XML_Not_Implemented'Identity);
                     end if;

                     Process_Details
                       (In_Type,
                        Details.Extension.Details, Start, S, Nested_End,
                        Mask);
                  else
                     --  ??? Should handle simple types
                     Nested_End := Start;

                     --  The test is correct. However, it makes
                     --  msData/particles/particlesZ031.xsd fails, because the
                     --  test is incorrect. The test pretends that the XSD is
                     --  valid, but later checkins in the testsuite have proven
                     --  it incorrect. The following Ada test would make the
                     --  test pass, but then
                     --     MS-Additional2006-07-15/addB036
                     --  fails
                     --
                     --  and then
                     --    (Internal_Type = No_Internal_Type_Index
                     --   or else Shared.Types.Table (Internal_Type).Is_Simple)

                     if not Details.Simple_Content then
                        Validation_Error
                          (Parser,
                           "base type specified in complexContent definition"
                           & " must be a complex type",
                           Details.Extension.Loc);
                     end if;
                  end if;

                  In_Type.Extension_Of   := NFA_Type;
                  In_Type.Restriction_Of := No_Type_Index;
               end;

            when Type_Restriction =>
               declare
                  Internal : Internal_Type_Index;
                  NFA_Type : Type_Index;  --  Attributes of the base type
                  Base_Descr : access Type_Descr;
               begin
                  Get_Type_Descr
                    (Name          => Details.Restriction.Base,
                     Loc           => No_Location,
                     NFA_Type      => NFA_Type,
                     Internal_Type => Internal);

                  Base_Descr := Get_Type_Descr (NFA, NFA_Type);

                  if Base_Descr.Final (Final_Restriction) then
                     Validation_Error
                       (Parser,
                        To_QName (Base_Descr.Name)
                        & " is final for restrictions",
                        Details.Restriction.Loc);
                  end if;

                  if Base_Descr.Simple_Content = No_Simple_Type_Index then
                     if Internal /= No_Internal_Type_Index
                       and then Shared.Types.Table (Internal).Details /= null
                       and then
                         Shared.Types.Table (Internal).Details.In_Process
                     then
                        Validation_Error
                          (Parser,
                           "Circular inheritance of type "
                           & To_QName (Details.Restriction.Base),
                           Details.Restriction.Loc);
                     end if;

                     Process_Details
                       (In_Type,
                        Details.Restriction.Details, Start, From, Nested_End,
                        Mask);
                  else
                     --  ??? Should handle simple types
                     Nested_End := Start;

                     if not Details.Simple_Content_Restriction then
                        Validation_Error
                          (Parser,
                           "base type specified in complexContent definition"
                           & " must be a complex type",
                           Details.Restriction.Loc);
                     end if;
                  end if;

                  In_Type.Restriction_Of := NFA_Type;
                  In_Type.Extension_Of   := No_Type_Index;
               end;

            when Type_Any =>
               S := NFA.Add_State; --   ((Simple => 1, others => <>));
               NFA.Set_Nested (S, Ur_Type (NFA, Details.Any.Process_Contents));

               NFA.Add_Transition
                 (From, S, (Transition_Any,
                  Combine (Parser.Grammar, No_Any_Descr,
                           Local_Process  => Details.Any.Process_Contents,
                           Local          => Details.Any.Namespaces,
                           As_Restriction => False,
                           Target_NS      => Details.Any.Target_NS)));

               Nested_End := NFA.Add_State;
               NFA.On_Empty_Nested_Exit (S, Nested_End);
         end case;

         --  For <all> elements, we can only have maxOccurs=1 and minOccurs=0
         --  or 1. In the case of minOccurs=0, and since we use conditional
         --  links there, we cannot create a direct empty transition from the
         --  start state to the final state (since the
         --  transition_close_from_all is on exit of that final state, and
         --  thus would be *after* the new empty transition). So this case
         --  is handled specially in Process_Type.

         if Details.Kind /= Type_All then
            if Details.Max_Occurs.Unbounded then
               Nested_End := NFA.Repeat
                 (From, Nested_End, Details.Min_Occurs.Value,
                  Natural'Last);
            else
               Nested_End := NFA.Repeat
                 (From, Nested_End, Details.Min_Occurs.Value,
                  Details.Max_Occurs.Value);
            end if;
         end if;

         Details.In_Process := False;
      end Process_Details;

      ----------------------------
      -- Resolve_Attribute_Type --
      ----------------------------

      procedure Resolve_Attribute_Type
        (Attr : in out Internal_Attribute_Descr;
         Loc  : Sax.Locators.Location)
      is
         TRef     : Global_Reference;
         NFA_Type : Type_Index;  --  In NFA
      begin
         if Attr.Local_Type /= No_Internal_Type_Index then
            NFA_Type := Shared.Types.Table (Attr.Local_Type).In_NFA;
            Attr.Descr.Simple_Type :=
              Get_Type_Descr (NFA, NFA_Type).Simple_Content;

         elsif Attr.Typ = No_Qualified_Name then
               --  ??? Type should be ur-type (3.2.2)
               null;

         else
            TRef := Get (Ref.all, (Attr.Typ, Ref_Type));
            if TRef = No_Global_Reference then
               Validation_Error
                 (Parser,
                  "Unknown type: " & To_QName (Attr.Typ),
                  Loc);

            else
               Attr.Descr.Simple_Type :=
                 Get_Type_Descr (NFA, TRef.Typ).Simple_Content;
            end if;
         end if;
      end Resolve_Attribute_Type;

      --------------------
      -- Add_Attributes --
      --------------------

      procedure Add_Attributes
        (List             : in out Attributes_List;
         Attrs            : Attr_Array_Access;
         Processed_Groups : in out AttrGroup_HTables.Instance;
         As_Restriction   : Boolean;
         Had_Any          : in out Boolean)
      is
         Gr   : AttrGroup_Descr;
         TRef : Global_Reference;
      begin
         if Attrs /= null then
            for A in Attrs'Range loop
               case Attrs (A).Kind is
                  when Kind_Unset =>
                     null;
                  when Kind_Group =>
                     Gr := Get (Shared.Global_AttrGroups, Attrs (A).Group_Ref);
                     if Gr = No_AttrGroup_Descr then
                        Validation_Error
                          (Parser,
                           "Reference to undefined attributeGroup: "
                           & To_QName (Attrs (A).Group_Ref),
                           Attrs (A).Loc);

                     elsif Get (Processed_Groups, Gr.Name) /=
                       No_AttrGroup_Descr
                     then
                        Validation_Error
                          (Parser,
                           "attributeGroup """ & To_QName (Attrs (A).Group_Ref)
                           & """ has circular reference",
                           Attrs (A).Loc);
                     else
                        Set (Processed_Groups, Gr.Name, Gr);
                        Add_Attributes
                          (List, Gr.Attributes, Processed_Groups,
                           As_Restriction, Had_Any);
                     end if;

                  when Kind_Attribute =>
                     if Attrs (A).Attr.Ref /= No_Qualified_Name then
                        TRef := Get
                          (Ref.all, (Attrs (A).Attr.Ref, Ref_Attribute));

                        if TRef = No_Global_Reference then
                           Validation_Error
                             (Parser,
                              "Unknown referenced attribute: "
                              & To_QName (Attrs (A).Attr.Ref),
                              Attrs (A).Loc);
                        end if;

                        Add_Attribute
                          (Parser, List,
                           Attribute => Attrs (A).Attr.Descr,
                           Ref       => TRef.Attributes.Named,
                           Loc       => Attrs (A).Loc);

                     else
                        Resolve_Attribute_Type (Attrs (A).Attr, Attrs (A).Loc);

                        if Attrs (A).Attr.Any /= No_Internal_Any_Descr then
                           Had_Any := True;
                           Add_Any_Attribute
                             (Parser.Grammar, List, Attrs (A).Attr.Any,
                              As_Restriction);

                        else
                           Add_Attribute
                             (Parser, List, Attrs (A).Attr.Descr,
                              Loc => Attrs (A).Loc);
                        end if;
                     end if;
               end case;
            end loop;
         end if;
      end Add_Attributes;

      ------------------------
      -- Create_Simple_Type --
      ------------------------

      procedure Create_Simple_Type
        (J     : Internal_Type_Index;
         Descr : out Type_Descr_Access;
         Index : out Type_Index)
      is
         Info : Internal_Type_Descr renames Shared.Types.Table (J);
         Simple : Simple_Type_Descr;
         Index_In_Simple : Natural;
         Internal : Internal_Simple_Type_Descr;
         Result : Simple_Type_Index;
      begin
         Index := Info.In_NFA;
         Descr := Type_Descr_Access (NFA.Get_Type_Descr (Index));

         Result := Descr.Simple_Content;
         if Result /= No_Simple_Type_Index then
            if Debug then
               Debug_Output ("Create_Simple_Type: already done "
                             & To_QName (Info.Properties.Name));
            end if;
            return;
         end if;

         Internal := Info.Simple;
         if Internal.Kind = Simple_Type_None then
            --  Not a simple type, nothing to do
            Descr := null;
            return;
         end if;

         if Internal.In_Process then
            Validation_Error
              (Parser,
               "Circular inheritance of type "
               & To_QName (Info.Properties.Name),
               Info.Loc);
         end if;

         if Debug then
            Debug_Output
              ("Create_Simple_Type " & To_QName (Info.Properties.Name)
               & " " & Internal.Kind'Img);
         end if;

         Info.Simple.In_Process := True;

         case Internal.Kind is
            when Simple_Type_None =>
               Descr := null;
               return;

            when Simple_Type =>
               --  ??? Shouldn't we set Simple_Content as well ?

               Descr.Restriction_Of := Any_Simple_Type_Index;

            when Simple_Type_Union =>
               Simple := (Kind => Primitive_Union,
                          Union => (others => No_Simple_Type_Index),
                          others => <>);
               Index_In_Simple := Simple.Union'First;

               for U in Internal.Union_Items'Range loop
                  declare
                     Member : constant Type_Member := Internal.Union_Items (U);
                     Item   : Type_Descr_Access;
                     Index  : Type_Index;
                  begin
                     exit when Member = No_Type_Member;

                     if Member.Name /= No_Qualified_Name then
                        Lookup_Simple_Type
                          (Member.Name, Internal.Loc, Item, Index);
                        if Item.Final (Final_Union) then
                           Validation_Error
                             (Parser,
                              To_QName (Member.Name) & " is final for union",
                              Internal.Loc);
                        end if;

                     else
                        Create_Simple_Type (Member.Local, Item, Index);
                     end if;

                     Simple.Union (Index_In_Simple) := Item.Simple_Content;
                     Index_In_Simple := Index_In_Simple + 1;
                  end;
               end loop;

               Result := Create_Simple_Type (NFA, Simple);
               Descr.Simple_Content := Result;
               Descr.Restriction_Of := Any_Simple_Type_Index;

            when Simple_Type_List =>
               Simple := (Kind      => Primitive_List,
                          List_Item => No_Simple_Type_Index,
                          others    => <>);

               for U in Internal.List_Items'Range loop
                  declare
                     Member : constant Type_Member := Internal.List_Items (U);
                     Item   : Type_Descr_Access;
                     Index  : Type_Index;
                  begin
                     exit when Member = No_Type_Member;
                     if Member.Name /= No_Qualified_Name then
                        Lookup_Simple_Type
                          (Member.Name, Internal.Loc, Item, Index);
                        if Item.Final (Final_List) then
                           Validation_Error
                             (Parser,
                              To_QName (Member.Name) & " is final for list",
                              Internal.Loc);
                        end if;
                     else
                        Create_Simple_Type (Member.Local, Item, Index);
                     end if;

                     Simple.List_Item := Item.Simple_Content;
                  end;
               end loop;

               Result := Create_Simple_Type (NFA, Simple);
               Descr.Simple_Content := Result;
               Descr.Restriction_Of := Any_Simple_Type_Index;

            when Simple_Type_Restriction | Simple_Type_Extension =>
               declare
                  Base  : Simple_Type_Descr;
                  Error : Symbol;
                  Loc   : Location;
                  NFA_Simple : Type_Descr_Access;
                  NFA_Type   : Type_Index;
               begin
                  if Internal.Base.Name /= No_Qualified_Name then
                     Lookup_Simple_Type
                       (Internal.Base.Name, Internal.Loc,
                        NFA_Simple, NFA_Type);
                  else
                     Create_Simple_Type
                       (Internal.Base.Local, NFA_Simple, NFA_Type);
                  end if;

                  if Internal.Base = No_Type_Member then
                     Base     := Any_Simple_Type;
                     NFA_Type := Any_Simple_Type_Index;

                  elsif NFA_Simple = null
                    or else NFA_Simple.Simple_Content = No_Simple_Type_Index
                  then
                     Validation_Error
                       (Parser,
                        "base type specified in simpleContent definition must"
                        & " be a simple type",
                        Loc);

                  else
                     Base := Copy
                       (Get_Simple_Type (NFA, NFA_Simple.Simple_Content));
                     Override (Simple  => Base,
                               Facets  => Internal.Facets,
                               Symbols => Get_Symbol_Table (Parser.all),
                               As_Restriction => Internal.Kind =
                                 Simple_Type_Restriction,
                               Error   => Error,
                               Error_Loc => Loc);
                     if Error /= No_Symbol then
                        Validation_Error (Parser, Get (Error).all, Loc);
                     end if;
                  end if;

                  case Internal.Kind is
                     when Simple_Type_Restriction =>
                        Descr.Restriction_Of := NFA_Type;
                     when Simple_Type_Extension =>
                        Descr.Extension_Of := NFA_Type;
                     when others =>
                        null;
                  end case;

                  Result := Create_Simple_Type (NFA, Base);
                  Descr.Simple_Content := Result;
               end;
         end case;

         Info.Simple.In_Process := False;
      end Create_Simple_Type;

      ------------------------------
      -- Create_Global_Attributes --
      ------------------------------

      procedure Create_Global_Attributes (Attr : Internal_Attribute_Descr) is
         Attr2 : Internal_Attribute_Descr;
      begin
         pragma Assert (Attr.Ref = No_Qualified_Name,
                        "A global attribute cannot define ref");
         pragma Assert (Attr.Descr.Name /= No_Qualified_Name,
                        "A global attribute must have a name");
         pragma Assert (Attr.Any = No_Internal_Any_Descr,
                        "A global attribute is not <anyAttribute>");
         pragma Assert (Attr.Descr.Next = Empty_Named_Attribute_List,
                        "Global attributes cannot be in a list");
         pragma Assert (Attr.Descr.Simple_Type = No_Simple_Type_Index,
                        "Type of global attributes should be undefined here");

         Attr2 := Attr;
         Resolve_Attribute_Type (Attr2, No_Location);

         Create_Global_Attribute (Parser, Attr2.Descr, No_Location);
      end Create_Global_Attributes;

      ------------------
      -- Process_Type --
      ------------------

      procedure Process_Type (Info : in out Internal_Type_Descr) is
         S1    : State;
         List  : Attributes_List := No_Attributes;

         Processed_Groups : AttrGroup_HTables.Instance;
         --  ??? If this table is here, we can't have an <extension> with the
         --  same attributeGroup as its base type. Maybe this should be local
         --  to Recursive_Add_Attributes instead

         procedure Recursive_Add_Attributes (Info : Internal_Type_Descr);
         procedure Recursive_Add_Attributes (Info : Internal_Type_Descr) is
            Ty    : Global_Reference;
            Index : Internal_Type_Index;
            Had_Any : Boolean := False;
            Base    : Qualified_Name;
            As_Restriction : Boolean;
         begin
            if Info.Is_Simple then
               --  A simpleType has no attribute
               return;
            elsif Info.Details = null then
               Add_Attributes (List, Info.Attributes, Processed_Groups,
                               As_Restriction => True, Had_Any => Had_Any);
               Reset (Processed_Groups);
               return;
            end if;

            if Info.Details.Kind = Type_Extension then
               Base           := Info.Details.Extension.Base;
               As_Restriction := False;
            elsif Info.Details.Kind = Type_Restriction then
               Base           := Info.Details.Restriction.Base;
               As_Restriction := True;
            else
               Base           := No_Qualified_Name;
            end if;

            if Base = No_Qualified_Name then
               --  No character data is allowed, but we might have attributes
               Add_Attributes (List, Info.Attributes, Processed_Groups,
                               As_Restriction => True, Had_Any => Had_Any);

            elsif not As_Restriction then
               Ty := Get (Ref.all, (Base, Ref_Type));
               if Ty = No_Global_Reference then
                  Validation_Error
                    (Parser, "No type """ & To_QName (Base) & """", Info.Loc);
               end if;

               --  If the base type is in the current package, we might not
               --  have computed all its attributes. Otherwise, get the list of
               --  attributes already computed in the grammar, since it is
               --  complete.

               Index := Get (Types, Base);
               if Index /= No_Internal_Type_Index then
                  Recursive_Add_Attributes (Shared.Types.Table (Index));
               else
                  Add_Attributes
                    (Parser, List,
                     Get_Type_Descr (NFA, Ty.Typ).Attributes,
                     As_Restriction => False, Loc => Info.Loc);
               end if;

               Add_Attributes (List, Info.Attributes,
                               Processed_Groups, As_Restriction => False,
                               Had_Any => Had_Any);

            else
               Ty := Get (Ref.all, (Base, Ref_Type));
               if Ty = No_Global_Reference then
                  Validation_Error
                    (Parser, "No type """ & To_QName (Base) & """", Info.Loc);
               end if;

               Index := Get (Types, Base);
               if Index /= No_Internal_Type_Index then
                  Recursive_Add_Attributes (Shared.Types.Table (Index));
               else
                  Add_Attributes
                    (Parser, List,
                     Get_Type_Descr (NFA, Ty.Typ).Attributes,
                     As_Restriction => True, Loc => Info.Loc);
               end if;

               Had_Any := False;
               Add_Attributes (List, Info.Attributes,
                               Processed_Groups, As_Restriction => True,
                               Had_Any => Had_Any);

               --  Always add <anyAttribute>, even if none was given in the
               --  restriction (in which case none should exist for the type
               --  either);

               if not Had_Any then
                  List.Any := No_Any_Descr;  --  Nothing matches
               end if;
            end if;
         end Recursive_Add_Attributes;

      begin
         if Info.Is_Simple then
            null; --  Already done in Process_Type
         else
            if Debug then
               Debug_Output ("Process complexType "
                             & To_QName (Info.Properties.Name));
            end if;

            declare
               Descr : constant access Type_Descr :=
                 Get_Type_Descr (NFA, Info.In_NFA);
               Mask : Visited_All_Children;
               Start : State := Descr.Complex_Content;
               Is_All : constant Boolean :=
                 Info.Details /= null and then Info.Details.Kind = Type_All;
            begin
               pragma Assert (Descr.Complex_Content /= No_State);

               if Is_All and then Info.Details.Min_Occurs.Value = 0 then
                  --  See comment in Process_Details as to why this is
                  --  handled here.
                  --  We should not make the transition directly from the
                  --  start node (Descr.Complex_Content), because it would
                  --  mean a user can start a <all minOccurs="0"> and stop
                  --  in the middle with no error

                  Start := NFA.Add_State;
                  NFA.Add_Empty_Transition (Descr.Complex_Content, Start);
               end if;

               Process_Details
                 (In_Type    => Descr,
                  Details    => Info.Details,
                  Start      => Start, --  Descr.Complex_Content,
                  From       => Start,
                  Nested_End => S1,
                  Mask       => Mask);

               --  Add the attributes only after we did the details, so that we
               --  know there is no infinite recursion between the base types
               --  of extensions and restrictions

               if Debug then
                  Debug_Output ("Process attributes for complexType "
                                & To_QName (Info.Properties.Name) & " State="
                                & Descr.Complex_Content'Img
                                & " type_index=" & Info.In_NFA'Img);
               end if;

               Recursive_Add_Attributes (Info);
               Descr.Attributes := List;

               if Is_All then
                  NFA.Add_Transition
                    (S1, Final_State,
                     (Kind => Transition_Close_From_All,
                      Mask => Mask));

                  if Info.Details.Min_Occurs.Value = 0 then
                     NFA.Add_Transition
                       (Descr.Complex_Content, Final_State,
                        (Kind => Transition_Close));
                  end if;

               else
                  NFA.Add_Transition
                    (S1, Final_State, (Kind => Transition_Close));
               end if;
            end;
         end if;

         Reset (Processed_Groups);

      exception
         when others =>
            Reset (Processed_Groups);
            raise;
      end Process_Type;

      Element_Info : Element_Descr;
      Attr : Internal_Attribute_Descr;
      S : State;

      Ignored       : Type_Descr_Access;
      Ignored_Index : Type_Index;
      pragma Unreferenced (Ignored, Ignored_Index);

   begin
      if Debug then
         Debug_Output ("Create_NFA");
      end if;

      --  Prepare the entries for the types. These are empty to start with, but
      --  they are needed to be able to create the more complex types, and the
      --  global element.

      for J in Type_Tables.First .. Last (Shared.Types) loop
         --  Create the empty nested NFA if needed

         S := No_State;

         if not Shared.Types.Table (J).Is_Simple then
            S := NFA.Add_State;
            Shared.Types.Table (J).Properties.Complex_Content := S;
            if Debug then
               Debug_Output
                 ("Created state for complexContent "
                  & To_QName (Shared.Types.Table (J).Properties.Name)
                  & " type=" & J'Img & " state=" & S'Img);
            end if;
         end if;

         Shared.Types.Table (J).In_NFA :=
           Create_Type (NFA, Shared.Types.Table (J).Properties);

         if S /= No_State then
            --  At least .Complex_Content has changed, so we need to reset data
            NFA.Set_Data
              (S,
               State_Data'
                 (Simple   => Shared.Types.Table (J).In_NFA,
                  Block    => No_Block,
                  Nillable => False,
                  Default  => No_Symbol,
                  Fixed    => No_Symbol));
         end if;

         if Shared.Types.Table (J).Properties.Name /= No_Qualified_Name then
            Set (Types, Shared.Types.Table (J).Properties.Name, J);
         end if;
      end loop;

      --  Process the simple types (must be in a separate loop, since a
      --  restriction or a union needs to know about its base type)

      for J in Type_Tables.First .. Last (Shared.Types) loop
         Create_Simple_Type (J, Ignored, Ignored_Index);
      end loop;

      --  Prepare the entries for the global attributes

      Attr := Get_First (Shared.Global_Attributes);
      while Attr /= No_Internal_Attribute loop
         Create_Global_Attributes (Attr);
         Attr := Get_Next (Shared.Global_Attributes);
      end loop;

      --  Prepare schema for global elements

      if Debug then
         Debug_Output ("Create_NFA: adding global elements");
      end if;

      Element_Info := Get_First (Shared.Global_Elements);
      while Element_Info /= No_Element_Descr loop
         Process_Global_Element (Element_Info, Start_State);

         --  Save the state
         Set (Shared.Global_Elements, Element_Info.Name, Element_Info);

         Element_Info := Get_Next (Shared.Global_Elements);
      end loop;

      if Debug then
         Debug_Output ("Create_NFA: complete type definition");
      end if;

      --  Finally, complete the definition of complexTypes

      for J in Type_Tables.First .. Last (Shared.Types) loop
         Process_Type (Shared.Types.Table (J));
      end loop;

      if Debug then
         Output_Action ("NFA: " & Dump_Dot_NFA (Get_Grammar (Parser.all)));
      end if;

      Reset (Types);

   exception
      when others =>
         Reset (Types);
         raise;
   end Create_NFA;

   ----------
   -- Free --
   ----------

   procedure Free (Shared : in out XSD_Data_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (XSD_Data, XSD_Data_Access);
      Attr         : AttrGroup_Descr;
      Gr           : Group_Descr;
   begin
      if Shared /= null then

         --  Free all data structures, no longer needed

         Reset (Shared.Global_Elements);

         Gr := Get_First (Shared.Global_Groups);
         while Gr /= No_Group_Descr loop
            Free (Gr.Details);
            Gr := Get_Next (Shared.Global_Groups);
         end loop;
         Reset (Shared.Global_Groups);

         Attr := Get_First (Shared.Global_AttrGroups);
         while Attr /= No_AttrGroup_Descr loop
            Unchecked_Free (Attr.Attributes);
            Attr := Get_Next (Shared.Global_AttrGroups);
         end loop;
         Reset (Shared.Global_AttrGroups);

         for T in Type_Tables.First .. Last (Shared.Types) loop
            if Shared.Types.Table (T).Is_Simple then
               null;
            else
               Unchecked_Free (Shared.Types.Table (T).Attributes);
               Free (Shared.Types.Table (T).Details);
            end if;
         end loop;
         Free (Shared.Types);

         Reset (Shared.Global_Attributes);

         Unchecked_Free (Shared);
      end if;
   end Free;

   -------------------
   -- Parse_Grammar --
   -------------------

   procedure Parse_Grammar
     (Handler  : access Validating_Reader'Class;
      URI      : Symbol;
      Xsd_File : Symbol;
      Do_Create_NFA : Boolean)
   is
      File     : File_Input;
      Schema   : Schema_Reader;
      S_File_Full : constant Symbol := To_Absolute_URI (Handler.all, Xsd_File);
      Need_To_Initialize : Boolean := True;
   begin
      GNAT.Task_Lock.Lock;
      Set_XML_Version (Schema, Get_XML_Version (Handler.all));
      if URI_Was_Parsed (Get_Grammar (Handler.all), S_File_Full) then
         if Debug then
            Debug_Output ("Parse_Grammar " & Get (S_File_Full).all
                          & " already parsed");
         end if;
         GNAT.Task_Lock.Unlock;
         return;
      end if;

      if Debug then
         Debug_Output ("Parse_Grammar NS={" & Get (URI).all
                       & "} XSD={" & Get (Xsd_File).all & "} "
                       & Get (S_File_Full).all);
      end if;

      if Get_XSD_Version (Handler.Grammar) = XSD_1_0 then
         --  Must check that no element of the same namespace was seen
         --  already (as per 4.3.2 (4) in the XSD 1.0 norm, which was
         --  changed in XSD 1.1).

         declare
            NS : XML_NS;
         begin
            Find_NS_From_URI
              (Handler.all,
               URI     => URI,
               NS      => NS);

            if NS /= No_XML_NS
              and then Element_Count (NS) > 0
              and then S_File_Full /= Get_System_Id (NS)
              and then Get_Feature
                (Handler.all, Sax.Readers.Schema_Validation_Feature)
            then
               Validation_Error
                 (Handler,
                  "schemaLocation for """
                  & Get (URI).all
                  & """ cannot occur after the first"
                  & " element of that namespace in XSD 1.0");
            end if;
         end;
      end if;

      if Debug then
         Output_Seen ("Parsing grammar: " & Get (S_File_Full).all);
      end if;

      Open (Get (S_File_Full).all, File);
      Set_Public_Id (File, Get (S_File_Full).all);
      Set_System_Id (File, Get (S_File_Full).all);

      --  Add_To will likely already contain the grammar for the
      --  schema-for-schema, and we won't have to recreate it in most cases.

      Set_Symbol_Table (Schema, Get_Symbol_Table (Handler.all));
      Set_Grammar (Schema, Handler.Grammar);
      Use_Basename_In_Error_Messages
        (Schema, Use_Basename_In_Error_Messages (Handler.all));

      Set_Feature
        (Schema,
         Sax.Readers.Schema_Validation_Feature,
         Get_Feature (Handler.all, Sax.Readers.Schema_Validation_Feature));

      if Handler.all in Schema_Reader'Class then
         Schema.Shared := Schema_Reader (Handler.all).Shared;
         Need_To_Initialize := False;
      end if;

      begin
         Internal_Parse
           (Schema, File,
            Default_Namespace    => URI,
            Do_Initialize_Shared => Need_To_Initialize,
            Do_Create_NFA        => Need_To_Initialize and Do_Create_NFA);
      exception
         when XML_Not_Implemented | XML_Validation_Error =>
            --  Copy the error message and location from Schema to Handler
            Close (File);
            Handler.Error_Msg := Schema.Error_Msg;
            Handler.Error_Location := Schema.Error_Location;
            raise;
      end;

      Free (Schema);
      Close (File);

      if Debug then
         Output_Seen ("Done parsing new grammar: " & Get (Xsd_File).all);
      end if;

      GNAT.Task_Lock.Unlock;

   exception
      when Ada.IO_Exceptions.Name_Error =>
         Free (Schema);
         Close (File);
         GNAT.Task_Lock.Unlock;

         if Debug then
            Debug_Output
              (ASCII.LF
               & "!!!! Could not open file " & Get (S_File_Full).all
               & ASCII.LF);
         end if;

         --  According to XML Schema Primer 0, section 5.6, this is not an
         --  error when we do not find the schema, since this attribute is only
         --  a hint.
         Warning
           (Handler.all,
            Create (Message => "Could not open file " & Get (S_File_Full).all,
                    Loc     => Handler.Current_Location));

      when others =>
         GNAT.Task_Lock.Unlock;
         Close (File);
         raise;
   end Parse_Grammar;

   --------------------
   -- Internal_Parse --
   --------------------

   procedure Internal_Parse
     (Parser            : in out Schema_Reader;
      Input             : in out Input_Sources.Input_Source'Class;
      Default_Namespace : Symbol;
      Do_Create_NFA     : Boolean;
      Do_Initialize_Shared : Boolean)
   is
      Grammar : constant XML_Grammar := Get_Grammar (Parser);
      URI     : Symbol;
   begin
      if Debug then
         Output_Action
           ("Parsing schema "& Input_Sources.Get_System_Id (Input));
      end if;

      Initialize_Symbols (Parser);

      URI := Find_Symbol (Parser, Input_Sources.Get_System_Id (Input));

      if not URI_Was_Parsed (Grammar, URI) then

         if Do_Initialize_Shared then
            Parser.Shared := new XSD_Data;
            Init (Parser.Shared.Types);
         end if;

         Initialize_Grammar (Parser);

         Parser.Target_NS := Default_Namespace;

         Set_Grammar (Parser, Grammar); --  In case it was not initialized yet
         Set_Parsed_URI (Parser, URI);

         Schema.Readers.Parse (Validating_Reader (Parser), Input);

         if Do_Create_NFA then
            Create_NFA (Parser'Access);
         end if;

         if Do_Initialize_Shared then
            Free (Parser.Shared);
         end if;

         Unchecked_Free (Parser.Contexts);
      end if;

   exception
      when others =>
         Unchecked_Free (Parser.Contexts);

         if Do_Initialize_Shared then
            Free (Parser.Shared);
         end if;

         raise;
   end Internal_Parse;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      Internal_Parse
        (Parser,
         Input,
         Default_Namespace => Empty_String,
         Do_Create_NFA     => True,
         Do_Initialize_Shared => True);
   end Parse;

   -------------------
   -- Resolve_QName --
   -------------------

   function Resolve_QName
     (Handler     : access Schema_Reader'Class;
      QName       : Sax.Symbols.Symbol;
      NS_If_Empty : Sax.Symbols.Symbol := Empty_String;
      Loc         : Location) return Qualified_Name
   is
      Val       : Cst_Byte_Sequence_Access;
      Separator : Integer;
      NS        : XML_NS;
      Prefix    : Symbol;
   begin
      if QName = No_Symbol then
         return No_Qualified_Name;
      else
         Val       := Get (QName);
         Separator := Split_Qname (Val.all);

         Prefix :=
           Find_Symbol (Handler.all, Val (Val'First .. Separator - 1));
         Get_Namespace_From_Prefix
           (Handler  => Handler.all,
            Prefix   => Prefix,
            NS       => NS);

         if NS = No_XML_NS then
            if Prefix /= Empty_String then
               Validation_Error
                 (Handler,
                  "Cannot resolve namespace prefix "
                  & Val (Val'First .. Separator - 1),
                  Loc);
               return No_Qualified_Name;
            else
               return
                 (NS    => NS_If_Empty,
                  Local =>
                   Find_Symbol (Handler.all, Val (Separator + 1 .. Val'Last)));
            end if;

         else
            return
              (NS    => Get_URI (NS),
               Local =>
                 Find_Symbol (Handler.all, Val (Separator + 1 .. Val'Last)));
         end if;
      end if;
   end Resolve_QName;

   ----------------
   -- Get_Occurs --
   ----------------

   procedure Get_Occurs
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Min_Occurs, Max_Occurs : out Occurrences)
   is
      Min_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.MinOccurs);
      Max_Occurs_Index : constant Integer :=
        Get_Index (Atts, URI => Empty_String, Local_Name => Handler.MaxOccurs);

      function Occurs_From_Value (Index  : Integer) return Occurrences;
      --  Return the value of maxOccurs from the attributes'value. This
      --  properly takes into account the "unbounded" case

      function Occurs_From_Value (Index  : Integer) return Occurrences is
         Value : constant Symbol := Get_Value (Atts, Index);
      begin
         if Value = Handler.Unbounded then
            return (Unbounded => True);
         else
            declare
               Val : constant Cst_Byte_Sequence_Access := Get (Value);
               Pos : Integer;
               C   : Unicode_Char;
            begin
               return (Unbounded => False, Value => Natural'Value (Val.all));
            exception
               when Constraint_Error =>
                  --  Either we have an integer too big to fit in Integer, or
                  --  we do not have an integer at all
                  Pos := Val'First;
                  while Pos <= Val'Last loop
                     Encoding.Read (Val.all, Pos, C);
                     if not Is_Digit (C) then
                        Validation_Error
                          (Handler, "Value for ""maxOccurs"" must"
                           & " be an integer or ""unbounded""");
                     end if;
                  end loop;

                  return (Unbounded => False, Value => Natural'Last);
            end;
         end if;
      end Occurs_From_Value;

   begin
      Min_Occurs := (False, 1);
      Max_Occurs := (False, 1);

      if Min_Occurs_Index /= -1 then
         Min_Occurs := Occurs_From_Value (Min_Occurs_Index);
         if Min_Occurs.Unbounded then
            Validation_Error
              (Handler, "minOccurs cannot be ""unbounded""");
         end if;
      end if;

      if Max_Occurs_Index /= -1 then
         Max_Occurs := Occurs_From_Value (Max_Occurs_Index);
      end if;

      if not Max_Occurs.Unbounded
        and then Max_Occurs.Value > Max_Max_Occurs
      then
         Validation_Error
           (Handler,
            "maxOccurs is too big, consider using ""unbounded""",
            Except => XML_Not_Implemented'Identity);
      end if;
   end Get_Occurs;

   ------------------
   -- Push_Context --
   ------------------

   procedure Push_Context
     (Handler : access Schema_Reader'Class; Ctx : Context)
   is
      Tmp : Context_Array_Access;
   begin
      if Handler.Contexts_Last = 0 then
         Handler.Contexts := new Context_Array (1 .. Default_Contexts);
      elsif Handler.Contexts_Last = Handler.Contexts'Last then
         Tmp := new Context_Array (1 .. Handler.Contexts'Last + 30);
         Tmp (Handler.Contexts'Range) := Handler.Contexts.all;
         Unchecked_Free (Handler.Contexts);
         Handler.Contexts := Tmp;
      end if;

      Handler.Contexts_Last := Handler.Contexts_Last + 1;
      Handler.Contexts (Handler.Contexts_Last) := Ctx;
   end Push_Context;

   ------------------
   -- Create_Group --
   ------------------

   procedure Create_Group
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Occurrences := (False, 1);
      Group   : Group_Descr;
      Name    : Qualified_Name;
      Details : Type_Details_Access;
   begin
      Group.Loc := Handler.Current_Location;

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Name then
               Group.Name := (NS    => Handler.Target_NS,
                              Local => Get_Value (Atts, J));
            elsif Name.Local = Handler.Ref then
               Group.Ref := Resolve_QName
                 (Handler, Get_Value (Atts, J),
                  Loc => Get_Location (Atts, J));
            end if;
         end if;
      end loop;

      case Handler.Contexts (Handler.Contexts_Last).Typ is
         when Context_Schema | Context_Redefine =>
            null;

         when Context_Sequence | Context_Choice | Context_Extension
            | Context_Restriction =>
            Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
            Details := new Type_Details'
              (Kind       => Type_Group,
               Min_Occurs => Min_Occurs,
               Max_Occurs => Max_Occurs,
               Loc        => Handler.Current_Location,
               In_Process => False,
               Next       => null,
               Group      => Group);
            Insert_In_Type (Handler, Details);

         when others =>
            Validation_Error
              (Handler,
               "Unsupported ""group"" in this context",
              Except => XML_Not_Implemented'Identity);
      end case;

      Push_Context (Handler,
                    (Typ   => Context_Group,
                     Group => Group));

   end Create_Group;

   ------------------
   -- Finish_Group --
   ------------------

   procedure Finish_Group (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine =>
            Set (Handler.Shared.Global_Groups, Ctx.Group.Name, Ctx.Group);
         when others =>
            null;
      end case;
   end Finish_Group;

   ----------------------------
   -- Create_Attribute_Group --
   ----------------------------

   procedure Create_Attribute_Group
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Group : AttrGroup_Descr;
      Name  : Qualified_Name;
   begin
      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Name then
               Group.Name := (NS    => Handler.Target_NS,
                              Local => Get_Value (Atts, J));
            elsif Name.Local = Handler.Ref then
               Group.Ref := Resolve_QName
                 (Handler, Get_Value (Atts, J),
                  Loc => Get_Location (Atts, J));
            end if;
         end if;
      end loop;

      Push_Context
        (Handler, (Typ        => Context_Attribute_Group,
                   Attr_Group => Group));
   end Create_Attribute_Group;

   ----------------------------
   -- Finish_Attribute_Group --
   ----------------------------

   procedure Finish_Attribute_Group (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
      Ctx2 : Context_Access;
      Index : Natural;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine =>
            Set
              (Handler.Shared.Global_AttrGroups,
               Ctx.Attr_Group.Name, Ctx.Attr_Group);
         when Context_Type_Def =>
            pragma Assert (Ctx.Attr_Group.Attributes = null);
            Append
              (Handler.Shared.Types.Table (Next.Type_Info).Attributes,
               (Kind      => Kind_Group,
                Loc       => Handler.Current_Location,
                Group_Ref => Ctx.Attr_Group.Ref));
         when Context_Extension =>
            pragma Assert (Ctx.Attr_Group.Attributes = null);
            Index := Handler.Contexts_Last - 1;
            while Index >= Handler.Contexts'First loop
               Ctx2 := Handler.Contexts (Index)'Access;
               if Ctx2.Typ = Context_Type_Def then
                  Append
                    (Handler.Shared.Types.Table (Ctx2.Type_Info).Attributes,
                     (Kind      => Kind_Group,
                      Loc       => Handler.Current_Location,
                      Group_Ref => Ctx.Attr_Group.Ref));
                  exit;
               end if;

               Index := Index - 1;
            end loop;
         when Context_Attribute_Group =>
            pragma Assert (Ctx.Attr_Group.Attributes = null);
            Append
              (Next.Attr_Group.Attributes,
               (Kind      => Kind_Group,
                Loc       => Handler.Current_Location,
                Group_Ref => Ctx.Attr_Group.Ref));

         when others =>
            Unchecked_Free (Ctx.Attr_Group.Attributes);
            Validation_Error
              (Handler,
               "Invalid context for attributeGroup: " & Next.Typ'Img,
               Except => XML_Not_Implemented'Identity);
      end case;
   end Finish_Attribute_Group;

   ------------
   -- Append --
   ------------

   procedure Append (List : in out Attr_Array_Access; Attr : Attr_Descr) is
      Tmp : Attr_Array_Access;
   begin
      if List = null then
         List := new Attr_Array'(1 => Attr,
                                 2 .. 10 => (Kind => Kind_Unset,
                                             Loc  => No_Location));

      elsif List (List'Last).Kind /= Kind_Unset then
         Tmp := new Attr_Array (1 .. List'Last + 10);
         Tmp (List'Range) := List.all;
         Tmp (List'Last + 1) := Attr;
         Tmp (List'Last + 2 .. Tmp'Last) :=
           (others => Attr_Descr'(Kind => Kind_Unset, Loc => No_Location));
         Unchecked_Free (List);
         List := Tmp;

      else
         for L in List'Range loop
            if List (L).Kind = Kind_Unset then
               List (L) := Attr;
               return;
            end if;
         end loop;
      end if;
   end Append;

   --------------------
   -- Create_Include --
   --------------------

   procedure Create_Include
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Schema_Location_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Schema_Location);
   begin
      Parse_Grammar
        (Handler,
         URI      => Handler.Target_NS,
         Xsd_File => Get_Value (Atts, Schema_Location_Index),
         Do_Create_NFA => False);  --  Will be performed later
   end Create_Include;

   ---------------------
   -- Create_Redefine --
   ---------------------

   procedure Create_Redefine
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Schema_Location);
   begin
      --  Disable for now.
      --  On the test./testschema -xsd boeingData/ipo4/ipo.xsd
      --    -xsd boeingData/ipo4/address.xsd
      --    -xsd boeingData/ipo4/itematt.xsd
      --    boeingData/ipo4/ipo_1.xml
      --  we redefine an extension whose base type comes from the redefined
      --  grammar, and whose name is the same. As a result, the extension and
      --  its base type end up being the same XML_Type, and thus we get
      --  infinite loops. We should really merge the models when the grammar is
      --  parsed.

      Validation_Error
        (Handler,
         "<redefine> not supported",
         Except => XML_Not_Implemented'Identity);
      Parse_Grammar
        (Handler,
         URI      => Handler.Target_NS,
         Do_Create_NFA => True,
         Xsd_File => Get_Value (Atts, Location_Index));

      Push_Context (Handler, (Typ => Context_Redefine));
   end Create_Redefine;

   -------------------
   -- Create_Import --
   -------------------

   procedure Create_Import
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Location_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Schema_Location);
      Namespace_Index : constant Integer :=
        Get_Index (Atts, Empty_String, Handler.Namespace);
   begin
      if Location_Index = -1 then
         if Namespace_Index = -1 then
            --  See 4.2.6.1: If that attribute is absent, then the import
            --  allows unqualified reference to components with no target
            --  namespace
            null;
         end if;

         Validation_Error
           (Handler,
            "Import with no schemaLocation is unsupported",
            Except => XML_Not_Implemented'Identity);

      else
         declare
            Location : constant Symbol := Get_Value (Atts, Location_Index);
         begin
            if Debug then
               Debug_Output ("Import: " & Get (Location).all);
               Debug_Output ("Adding new grammar to Handler.Created_Grammar");
            end if;

            --  The namespace attribute indicates that the XSD may contain
            --  qualified references to schema components in that namespace.
            --  (4.2.6.1). It does not give the default targetNamespace

            Parse_Grammar
              (Handler,
               URI      => Empty_String,
               Do_Create_NFA => True,
               Xsd_File => Location);
         end;
      end if;
   end Create_Import;

   --------------------------
   -- Create_Any_Attribute --
   --------------------------

   procedure Create_Any_Attribute
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Name  : Qualified_Name;
      Att   : Attr_Descr (Kind => Kind_Attribute);
   begin
      Att.Loc := Handler.Current_Location;
      Att.Attr.Any := (Namespaces       => Handler.Any_Namespace,
                       Target_NS        => Handler.Target_NS,
                       Process_Contents => Process_Strict);

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Namespace then
               Att.Attr.Any.Namespaces := Get_Value (Atts, J);
            elsif Name.Local = Handler.Process_Contents then
               Att.Attr.Any.Process_Contents :=
                 Process_Contents_From_Atts (Handler, Atts, J);
            end if;
         end if;
      end loop;

      Insert_Attribute (Handler, Handler.Contexts_Last, Att);
   end Create_Any_Attribute;

   ---------------------
   -- Create_Notation --
   ---------------------

   procedure Create_Notation
     (Handler : access Schema_Reader'Class; Atts : Sax_Attribute_List)
   is
      type Notation_Descr is record
         Name      : Symbol;
         System_Id : Symbol := Empty_String;
         Public_Id : Symbol := Empty_String;
      end record;

      Name     : Qualified_Name;
      Notation : Notation_Descr;

   begin
      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Name then
               Notation.Name := Get_Value (Atts, J);
            elsif Name.Local = Handler.Public then
               Notation.Public_Id := Get_Value (Atts, J);
            elsif Name.Local = Handler.System then
               Notation.System_Id := Get_Value (Atts, J);
            end if;
         end if;
      end loop;

      Add_Notation (Get_NFA (Handler.Grammar), Notation.Name);
      Notation_Decl
        (Sax_Reader'Class (Handler.all),
         Name      => Get (Notation.Name).all,
         System_Id => Get (Notation.System_Id).all,
         Public_Id => Get (Notation.Public_Id).all);
   end Create_Notation;

   --------------------
   -- Create_Element --
   --------------------

   procedure Create_Element
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Occurrences := (False, 1);
      Info    : Element_Descr;
      Name    : Qualified_Name;
      Details : Type_Details_Access;

   begin
      Info.Loc   := Handler.Current_Location;
      Info.Form  := Handler.Element_Form_Default;
      Info.Block := Handler.Target_Block_Default;

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Typ then
               Info.Typ := Resolve_QName
                 (Handler, Get_Value (Atts, J),
                  NS_If_Empty => Handler.Target_NS,
                  Loc         => Get_Location (Atts, J));
            elsif Name.Local = Handler.Name then
               Info.Name := (NS    => Handler.Target_NS,
                             Local => Get_Value (Atts, J));
            elsif Name.Local = Handler.Ref then
               Info.Ref := Resolve_QName
                 (Handler, Get_Value (Atts, J),
                  Loc => Get_Location (Atts, J));
            elsif Name.Local = Handler.Substitution_Group then
               Info.Substitution_Group :=
                 Resolve_QName
                   (Handler, Get_Value (Atts, J),
                    Loc => Get_Location (Atts, J));
            elsif Name.Local = Handler.Default then
               Info.Default := Get_Value (Atts, J);
            elsif Name.Local = Handler.Fixed then
               Info.Fixed := Get_Value (Atts, J);
            elsif Name.Local = Handler.S_Abstract then
               Info.Is_Abstract := Get_Value_As_Boolean (Atts, J, False);
            elsif Name.Local = Handler.Nillable then
               Info.Nillable := Get_Value_As_Boolean (Atts, J, False);
            elsif Name.Local = Handler.Form then
               Info.Form := Compute_Form (Atts, Handler, J);
            elsif Name.Local = Handler.Final then
               Info.Final := Compute_Final (Atts, Handler, J);
            elsif Name.Local = Handler.Block then
               Compute_Blocks (Atts, Handler, Info.Block, Info.Has_Block, J);
            end if;
         end if;
      end loop;

      if Info.Name /= No_Qualified_Name then
         if Info.Ref /= No_Qualified_Name
           and then Info.Ref.NS = No_Symbol
           and then Info.Name = Info.Ref
           and then not In_Redefine_Context (Handler.all)
         then
            Validation_Error
              (Handler, """ref"" attribute cannot be self-referencing");

         elsif Info.Ref /= No_Qualified_Name then
            Validation_Error
              (Handler, "Name and Ref cannot be both specified");
         end if;

      elsif Info.Ref = No_Qualified_Name then
         Validation_Error
           (Handler, "Either ""name"" or ""ref"" attribute must be present");

      else
         --  Section 3.3.2, validity constraints 3.3.3
         if Info.Typ /= No_Qualified_Name then
            Validation_Error
              (Handler,
               """type"" attribute cannot be specified along with ""ref""");
         end if;
      end if;

      if Info.Default /= No_Symbol and then Info.Fixed /= No_Symbol then
         Validation_Error
           (Handler, "Default and Fixed cannot be both specified");
      end if;

      if Info.Ref /= No_Qualified_Name then
         Info.Form := Qualified;
      end if;

      if Handler.Contexts (Handler.Contexts_Last).Typ /= Context_Schema then
         Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
         Details := new Type_Details'
           (Kind         => Type_Element,
            Min_Occurs   => Min_Occurs,
            Max_Occurs   => Max_Occurs,
            Loc          => Handler.Current_Location,
            In_Process   => False,
            Next         => null,
            Element      => Info);
         Insert_In_Type (Handler, Details);
      end if;

      Push_Context
        (Handler,
         (Typ          => Context_Element,
          Elem_Details => Details,
          Element      => Info));
   end Create_Element;

   --------------------
   -- Finish_Element --
   --------------------

   procedure Finish_Element (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
      Info : constant Element_Descr := Ctx.Element;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine =>
            Set (Handler.Shared.Global_Elements, Info.Name, Info);
         when others =>
            --  We might have added the type definition
            Ctx.Elem_Details.Element := Ctx.Element;
      end case;
   end Finish_Element;

   ------------------------
   -- Create_Simple_Type --
   ------------------------

   procedure Create_Simple_Type
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
   begin
      Prepare_Type (Handler, Atts, Is_Simple => True);

      if Ctx.Typ = Context_Simple_Restriction then
         Ctx.Simple.Base :=
           (Name  => No_Qualified_Name,
            Local => Handler.Contexts (Handler.Contexts_Last).Type_Info);
      end if;
   end Create_Simple_Type;

   ---------------------
   -- Add_Type_Member --
   ---------------------

   procedure Add_Type_Member
     (Handler : access Schema_Reader'Class;
      List    : in out Type_Member_Array;
      Member  : Type_Member;
      Loc     : Location)
   is
   begin
      for A in List'Range loop
         if List (A) = No_Type_Member then
            List (A) := Member;
            return;
         end if;
      end loop;

      Validation_Error
        (Handler,
         "Too many types in the union", Loc,
         XML_Not_Implemented'Identity);
   end Add_Type_Member;

   ------------------------
   -- Finish_Simple_Type --
   ------------------------

   procedure Finish_Simple_Type (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Schema | Context_Redefine => null;
         when Context_Element   => Next.Element.Local_Type   := Ctx.Type_Info;
         when Context_Attribute =>
            Next.Attribute.Attr.Local_Type := Ctx.Type_Info;
         when Context_List    =>
            Add_Type_Member (Handler, Next.List.List_Items,
              (Name => No_Qualified_Name,
               Local => Ctx.Type_Info),
              No_Location);
         when Context_Union   =>
            Add_Type_Member (Handler, Next.Union.Union_Items,
              (Name => No_Qualified_Name,
               Local => Ctx.Type_Info),
              No_Location);
         when Context_Restriction =>
            Next.Restriction.Restriction.Details :=
              Handler.Shared.Types.Table (Ctx.Type_Info).Details;
         when Context_Simple_Restriction =>
            --  The simpleType is in fact the base type of the restriction. The
            --  following was already done in Create_Simple_Type:
            --     Next.Simple.Base.Local := Ctx.Type_Info;
            null;
         when others          =>
            Validation_Error
              (Handler,
               "Unsupported: ""simpleType"" in this context",
               Except => XML_Not_Implemented'Identity);
      end case;
   end Finish_Simple_Type;

   --------------------
   -- Compute_Blocks --
   --------------------

   procedure Compute_Blocks
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Blocks  : out Block_Status;
      Is_Set  : out Boolean;
      Index   : Integer)
   is
      procedure On_Item (Str : Byte_Sequence);
      procedure On_Item (Str : Byte_Sequence) is
      begin
         if Str = "restriction" then
            Blocks (Block_Restriction) := True;
         elsif Str = "extension" then
            Blocks (Block_Extension) := True;
         elsif Str = "substitution" then
            Blocks (Block_Substitution) := True;
         elsif Str = "#all" then
            Blocks := (others => True);
         else
            Validation_Error
              (Handler, "Invalid value for block: """ & Str & """");
         end if;
      end On_Item;

      procedure For_Each is new For_Each_Item (On_Item);
   begin
      Is_Set := Index /= -1;
      Blocks := No_Block;

      if Index /= -1 then
         For_Each (Get (Get_Value (Atts, Index)).all);

         if Debug then
            Output_Action ("Set_Block (" & To_String (Blocks) & ")");
         end if;
      end if;
   end Compute_Blocks;

   ------------------
   -- Compute_Form --
   ------------------

   function Compute_Form
     (Atts      : Sax_Attribute_List;
      Handler   : access Schema_Reader'Class;
      Index     : Integer) return Form_Type is
   begin
      if Get_Value (Atts, Index) = Handler.Qualified then
         return Qualified;
      else
         return Unqualified;
      end if;
   end Compute_Form;

   -------------------
   -- Compute_Final --
   -------------------

   function Compute_Final
     (Atts    : Sax_Attribute_List;
      Handler : access Schema_Reader'Class;
      Index   : Integer) return Final_Status
   is
      Final : Final_Status;

      procedure On_Item (Str : Byte_Sequence);
      procedure On_Item (Str : Byte_Sequence) is
      begin
         if Str = "restriction" then
            Final (Final_Restriction) := True;
         elsif Str = "extension" then
            Final (Final_Extension)   := True;
         elsif Str = "#all" then
            Final := (others => True);
         elsif Str = "union" then
            Final (Final_Union)       := True;
         elsif Str = "list" then
            Final (Final_List)        := True;
         else
            Validation_Error
              (Handler, "Invalid value for final: """ & Str & """");
         end if;
      end On_Item;

      procedure For_Each is new For_Each_Item (On_Item);
   begin
      Final := (others => False);

      if Index /= -1 then
         For_Each (Get (Get_Value (Atts, Index)).all);

         if Debug then
            Output_Action ("Set_Final (" & To_String (Final) & ")");
         end if;
      end if;

      return Final;
   end Compute_Final;

   ------------------
   -- Prepare_Type --
   ------------------

   procedure Prepare_Type
     (Handler   : access Schema_Reader'Class;
      Atts      : Sax_Attribute_List;
      Is_Simple : Boolean)
   is
      Info   : Internal_Type_Descr (Is_Simple => Is_Simple);
      Is_Set : Boolean;
      Name   : Qualified_Name;
      Props  : Type_Descr;

   begin
      Info.Loc := Handler.Current_Location;
      Props.Block := Handler.Target_Block_Default;

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Mixed then
               Props.Mixed := Get_Value_As_Boolean (Atts, J, False);
            elsif Name.Local = Handler.Name then
               Props.Name :=
                 (NS    => Handler.Target_NS,
                  Local => Get_Value (Atts, J));
            elsif Name.Local = Handler.Block then
               Compute_Blocks (Atts, Handler, Props.Block, Is_Set, J);
            elsif Name.Local = Handler.Final then
               Props.Final := Compute_Final (Atts, Handler, J);
            elsif Name.Local = Handler.S_Abstract then
               Props.Is_Abstract := Get_Value_As_Boolean (Atts, J, False);
            end if;
         end if;
      end loop;

      --  block="substitution" does not apply to types, only to elements
      Props.Block (Block_Substitution) := False;

      Info.Properties := Props;

      Append (Handler.Shared.Types, Info);

      Push_Context
        (Handler,
         (Typ       => Context_Type_Def,
          Type_Info => Last (Handler.Shared.Types)));
   end Prepare_Type;

   -------------------------
   -- Create_Complex_Type --
   -------------------------

   procedure Create_Complex_Type
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List) is
   begin
      Prepare_Type (Handler, Atts, Is_Simple => False);
   end Create_Complex_Type;

   -------------------------
   -- Finish_Complex_Type --
   -------------------------

   procedure Finish_Complex_Type (Handler  : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Element => Next.Element.Local_Type := Ctx.Type_Info;
         when others          => null;
      end case;
   end Finish_Complex_Type;

   ------------------------
   -- Create_Restriction --
   ------------------------

   procedure Create_Restriction
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Restr      : Restriction_Descr;
      Details    : Type_Details_Access;
      Name       : Qualified_Name;
      In_Type    : constant Internal_Type_Index := Ctx.Type_Info;
   begin
      Restr.Loc := Handler.Current_Location;
      Restr.Base := (NS    => Handler.XML_Schema_URI,
                     Local => Handler.Any_Simple_Type);

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Base then
               Restr.Base := Resolve_QName
                 (Handler, Get_Value (Atts, J), Handler.Target_NS,
                  Get_Location (Atts, J));
            end if;
         end if;
      end loop;

      if Handler.Shared.Types.Table (In_Type).Is_Simple
        or else Handler.Shared.Types.Table (In_Type).Simple.Kind /=
        Simple_Type_None
      then
         if Restr.Base = (NS => Handler.XML_Schema_URI, Local => Handler.IDREF)
           or else Restr.Base =
             (NS => Handler.XML_Schema_URI, Local => Handler.IDREFS)
         then
            Validation_Error
              (Handler,
               "Unsupported type IDREF and IDREFS",
               Except => XML_Not_Implemented'Identity);
         end if;

         if not Handler.Shared.Types.Table (In_Type).Is_Simple then
            Details := new Type_Details'
              (Kind        => Type_Restriction,
               Min_Occurs  => (False, 1),
               Max_Occurs  => (False, 1),
               Loc         => Handler.Current_Location,
               In_Process  => False,
               Next        => null,
               Simple_Content_Restriction => True,
               Restriction => Restr);
            Insert_In_Type (Handler, Details);
         end if;

         Push_Context
           (Handler,
            (Typ         => Context_Simple_Restriction,
             Simple      => (Kind             => Simple_Type_Restriction,
                             In_Process       => False,
                             Facets           => No_Facets,
                             Base =>
                               (Name  => Restr.Base,
                                Local => No_Internal_Type_Index),
                             Loc         => Handler.Current_Location)));

      else
         Details := new Type_Details'
           (Kind        => Type_Restriction,
            Min_Occurs  => (False, 1),
            Max_Occurs  => (False, 1),
            Loc         => Handler.Current_Location,
            In_Process  => False,
            Next        => null,
            Simple_Content_Restriction => False,
            Restriction => Restr);
         Insert_In_Type (Handler, Details);
         Push_Context
           (Handler,
            (Typ         => Context_Restriction,
             Restriction => Details));
      end if;
   end Create_Restriction;

   ------------------------
   -- Finish_Restriction --
   ------------------------

   procedure Finish_Restriction (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      if Ctx.Typ = Context_Simple_Restriction then
         pragma Assert (Next.Typ = Context_Type_Def);
         Handler.Shared.Types.Table (Next.Type_Info).Simple := Ctx.Simple;
      end if;
   end Finish_Restriction;

   ------------------
   -- Create_Union --
   ------------------

   procedure Create_Union
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Name : Qualified_Name;

      procedure Add_Union (Str : Byte_Sequence);
      --  Add a unioned type to [Simple]

      procedure Add_Union (Str : Byte_Sequence) is
         Sym  : constant Symbol := Find_Symbol (Handler.all, Str);
         Ctx : constant Context_Access :=
           Handler.Contexts (Handler.Contexts_Last)'Access;
         Name : constant Qualified_Name :=
           Resolve_QName (Handler, Sym, Handler.Target_NS,
                          Loc => Ctx.Union.Loc);
      begin
         Add_Type_Member
           (Handler,
            Ctx.Union.Union_Items,
            (Name => Name, Local => No_Internal_Type_Index),
            Ctx.Union.Loc);
      end Add_Union;

      procedure For_Each_Union is new For_Each_Item (Add_Union);

   begin
      Push_Context
        (Handler,
         (Typ   => Context_Union,
          Union => (Kind             => Simple_Type_Union,
                    In_Process       => False,
                    Loc              => Handler.Current_Location,
                    Union_Items      => (others => No_Type_Member))));

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Member_Types then
               For_Each_Union (Get (Get_Value (Atts, J)).all);
            end if;
         end if;
      end loop;
   end Create_Union;

   ------------------
   -- Finish_Union --
   ------------------

   procedure Finish_Union (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      case Next.Typ is
         when Context_Type_Def =>
            Handler.Shared.Types.Table (Next.Type_Info).Simple := Ctx.Union;
         when others =>
            Validation_Error
              (Handler,
               "Unsupported: ""union"" in this context",
               Except => XML_Not_Implemented'Identity);
      end case;
   end Finish_Union;

   ----------------------
   -- Create_Extension --
   ----------------------

   procedure Create_Extension
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Ext   : Extension_Descr;
      Name  : Qualified_Name;
      Details : Type_Details_Access;
      In_Type : constant Internal_Type_Index := Ctx.Type_Info;
   begin
      Ext.Loc := Handler.Current_Location;

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Base then
               Ext.Base := Resolve_QName
                 (Handler, Get_Value (Atts, J), Handler.Target_NS,
                  Loc => Get_Location (Atts, J));
            end if;
         end if;
      end loop;

      if Ext.Base = No_Qualified_Name then
         Validation_Error
           (Handler, "Attribute ""base"" required for <extensionType>");
      end if;

      if Handler.Shared.Types.Table (In_Type).Is_Simple
        or else Handler.Shared.Types.Table (In_Type).Simple.Kind /=
           Simple_Type_None
      then
         if Debug then
            Debug_Output ("Create extension: in simpleContent or simpleType");
         end if;

         if not Handler.Shared.Types.Table (In_Type).Is_Simple then
            Details := new Type_Details'
              (Kind       => Type_Extension,
               Min_Occurs => (False, 1),
               Max_Occurs => (False, 1),
               Loc        => Handler.Current_Location,
               In_Process => False,
               Next       => null,
               Simple_Content => True,
               Extension  => Ext);
            Insert_In_Type (Handler, Details);
         end if;

         Push_Context
           (Handler,
            (Typ    => Context_Simple_Extension,
             Simple => (Kind             => Simple_Type_Extension,
                        In_Process       => False,
                        Base             => (Name  => Ext.Base,
                                             Local => No_Internal_Type_Index),
                        Facets           => No_Facets,
                        Loc              => Handler.Current_Location)));
      else
         Details := new Type_Details'
           (Kind       => Type_Extension,
            Min_Occurs => (False, 1),
            Max_Occurs => (False, 1),
            Loc        => Handler.Current_Location,
            In_Process => False,
            Next       => null,
            Simple_Content => False,
            Extension  => Ext);
         Insert_In_Type (Handler, Details);
         Push_Context
           (Handler,
            (Typ       => Context_Extension,
             Extension => Details));
      end if;
   end Create_Extension;

   ----------------------
   -- Finish_Extension --
   ----------------------

   procedure Finish_Extension (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
   begin
      if Ctx.Typ = Context_Simple_Extension then
         pragma Assert (Next.Typ = Context_Type_Def);  --  a simple type
         Handler.Shared.Types.Table (Next.Type_Info).Simple := Ctx.Simple;
      end if;
   end Finish_Extension;

   -----------------
   -- Create_List --
   -----------------

   procedure Create_List
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Name  : Qualified_Name;
   begin
      Push_Context
        (Handler,
         (Typ   => Context_List,
          List  => (Kind       => Simple_Type_List,
                    In_Process => False,
                    Loc        => Handler.Current_Location,
                    List_Items => (others => No_Type_Member))));

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Item_Type then
               Name := Resolve_QName
                 (Handler, Get_Value (Atts, J), Handler.Target_NS,
                  Loc => Get_Location (Atts, J));
               Add_Type_Member
                 (Handler,
                  Handler.Contexts (Handler.Contexts_Last).List.List_Items,
                  (Name => Name, Local => No_Internal_Type_Index),
                  Handler.Current_Location);
            end if;
         end if;
      end loop;
   end Create_List;

   -----------------
   -- Finish_List --
   -----------------

   procedure Finish_List (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 1)'Access;
      Next_Next : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last - 2)'Access;
   begin
      case Next.Typ is
         when Context_Type_Def =>
            if Next.Type_Info = No_Internal_Type_Index then
               --  within a <simpleType><restriction><simpleType><list>
               pragma Assert (Next_Next.Typ = Context_Simple_Restriction);
               Next_Next.Simple := Ctx.List;

            else
               --  within a <simpleType><list>
               pragma Assert (Next.Type_Info /= No_Internal_Type_Index);

               Handler.Shared.Types.Table (Next.Type_Info).Simple := Ctx.List;
            end if;
         when others =>
            Validation_Error
              (Handler,
               "Unsupported: ""list"" in this context",
               Except => XML_Not_Implemented'Identity);
      end case;
   end Finish_List;

   --------------------
   -- Insert_In_Type --
   --------------------

   procedure Insert_In_Type
     (Handler : access Schema_Reader'Class;
      Element : in out Type_Details_Access)
   is
      procedure Append (List : in out Type_Details_Access;
                        Elem : Type_Details_Access);
      procedure Append (List : in out Type_Details_Access;
                        Elem : Type_Details_Access)
      is
         Tmp  : Type_Details_Access;
      begin
         if List = null then
            List := Elem;
         else
            Tmp := List;
            while Tmp.Next /= null loop
               Tmp := Tmp.Next;
            end loop;

            Tmp.Next := Elem;
         end if;
      end Append;

      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;

   begin
      case Ctx.Typ is
         when Context_Type_Def =>
            if Handler.Shared.Types.Table (Ctx.Type_Info).Is_Simple then
               Free (Element);
               Validation_Error
                 (Handler, "Invalid element in simple type");
            end if;

            if Debug and then
              Handler.Shared.Types.Table (Ctx.Type_Info).Details /= null
            then
               Debug_Output ("Insert_In_Type, type already has details "
                             & " when inserting " & Element.Kind'Img);
            end if;

            pragma Assert
              (Handler.Shared.Types.Table (Ctx.Type_Info).Details = null);

            Handler.Shared.Types.Table (Ctx.Type_Info).Details := Element;

         when Context_Sequence =>
            Append (Ctx.Seq.First_In_Seq, Element);

         when Context_Choice =>
            Append (Ctx.Choice.First_In_Choice, Element);

         when Context_All =>
            Append (Ctx.All_Detail.First_In_All, Element);

         when Context_Group =>
            if Ctx.Group.Details /= null then
               Free (Element);
               Validation_Error
                 (Handler, "Invalid element in non group");
            end if;

            Ctx.Group.Details := Element;

         when Context_Extension =>
            if Ctx.Extension.Extension.Details /= null then
               Free (Element);
               Validation_Error
                 (Handler, "Invalid element in non-empty extension");
            end if;

            Ctx.Extension.Extension.Details := Element;

         when Context_Restriction =>
            if Ctx.Restriction.Restriction.Details /= null then
               Free (Element);
               Validation_Error
                 (Handler, "Invalid element in non-empty restriction");
            end if;
            Ctx.Restriction.Restriction.Details := Element;

         when Context_Simple_Restriction | Context_Simple_Extension =>
            Free (Element);

         when Context_Schema | Context_Attribute | Context_Element
            | Context_Union
            | Context_List | Context_Redefine | Context_Attribute_Group =>

            Free (Element);
            Validation_Error
              (Handler,
               "Unsupported: """ & Element.Kind'Img & """ in context "
               & Ctx.Typ'Img,
               Except => XML_Not_Implemented'Identity);
      end case;
   end Insert_In_Type;

   -------------------
   -- Create_Choice --
   -------------------

   procedure Create_Choice
     (Handler : access Schema_Reader'Class;
      Atts : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Occurrences := (False, 1);
      Choice  : Type_Details_Access;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Choice := new Type_Details'
        (Kind            => Type_Choice,
         Min_Occurs      => Min_Occurs,
         Max_Occurs      => Max_Occurs,
         Loc             => Handler.Current_Location,
         In_Process      => False,
         Next            => null,
         First_In_Choice => null);
      Insert_In_Type (Handler, Choice);
      Push_Context
        (Handler,
         (Typ    => Context_Choice,
          Choice => Choice));
   end Create_Choice;

   ---------------------
   -- Create_Sequence --
   ---------------------

   procedure Create_Sequence
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Occurrences := (False, 1);
      Seq  : Type_Details_Access;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Seq := new Type_Details'
        (Kind         => Type_Sequence,
         Min_Occurs   => Min_Occurs,
         Max_Occurs   => Max_Occurs,
         Loc          => Handler.Current_Location,
         In_Process   => False,
         Next         => null,
         First_In_Seq => null);
      Insert_In_Type (Handler, Seq);
      Push_Context
        (Handler,
         (Typ  => Context_Sequence,
          Seq  => Seq));
   end Create_Sequence;

   ----------------------
   -- Create_Attribute --
   ----------------------

   procedure Create_Attribute
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Name  : Qualified_Name;
      Att   : Attr_Descr (Kind => Kind_Attribute);
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
      Has_Form : Boolean := False;

   begin
      Att.Attr.Descr.Form := Handler.Attribute_Form_Default;
      Att.Loc := Handler.Current_Location;

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Name then
               Att.Attr.Descr.Name := (NS    => Handler.Target_NS,
                                       Local => Get_Value (Atts, J));
            elsif Name.Local = Handler.Typ then
               Att.Attr.Typ  := Resolve_QName
                 (Handler, Get_Value (Atts, J),
                  Loc => Get_Location (Atts, J));

               if Att.Attr.Typ =
                 (NS => Handler.XML_Schema_URI, Local => Handler.IDREF)
                 or else Att.Attr.Typ =
                   (NS => Handler.XML_Schema_URI, Local => Handler.IDREFS)
               then
                  Validation_Error
                    (Handler,
                     "Unsupported type IDREF and IDREFS",
                     Get_Location (Atts, J),
                     Except => XML_Not_Implemented'Identity);
               end if;

            elsif Name.Local = Handler.S_Use then
               if Get_Value (Atts, J) = Handler.Required then
                  Att.Attr.Descr.Use_Type := Required;
               elsif Get_Value (Atts, J) = Handler.Prohibited then
                  Att.Attr.Descr.Use_Type := Prohibited;
               else
                  Att.Attr.Descr.Use_Type := Optional;
               end if;

            elsif Name.Local = Handler.Fixed then
               Att.Attr.Descr.Fixed := Get_Value (Atts, J);
            elsif Name.Local = Handler.Ref then
               Att.Attr.Ref := Resolve_QName
                 (Handler, Get_Value (Atts, J), Handler.Target_NS,
                  Loc => Get_Location (Atts, J));
            elsif Name.Local = Handler.Form then
               Att.Attr.Descr.Form :=
                 Form_Type'Value (Get (Get_Value (Atts, J)).all);
               Has_Form := True;
            elsif Name.Local = Handler.Default then
               Att.Attr.Descr.Default := Get_Value (Atts, J);
            elsif Name.Local = Handler.Namespace_Target then
               Att.Attr.Descr.Target_NS := Get_Value (Atts, J);
            end if;
         end if;
      end loop;

      --  See section 3.2.3 for valid attributes combination

      if Att.Attr.Descr.Target_NS /= No_Symbol then
         if Att.Attr.Descr.Name /= No_Qualified_Name then
            Validation_Error
              (Handler,
               "name must be specified when targetNamespace is specified");
         end if;

         if Has_Form then
            Validation_Error
              (Handler,
               "Cannot specify ""form"" when targetNamespace is given");
         end if;

         Validation_Error
           (Handler,
            "targetNamespace not supported in attribute declaration",
            Except => XML_Not_Implemented'Identity);
      end if;

      if Has_Form and then Att.Attr.Ref /= No_Qualified_Name then
         Validation_Error
           (Handler,
            "Attributes ""form"" and ""ref"" cannot be both specified");
      end if;

      if Att.Attr.Typ /= No_Qualified_Name then
         if Att.Attr.Ref /= No_Qualified_Name then
            Validation_Error
              (Handler,
               "Attributes ""type"" and ""ref"" cannot be both specified");
         end if;
      end if;

      if Att.Attr.Descr.Fixed /= No_Symbol
        and then Att.Attr.Descr.Default /= No_Symbol
      then
         Validation_Error
           (Handler,
            "Attributes ""fixed"" and ""default"" cannot be both specified");
      end if;

      if Att.Attr.Descr.Default /= No_Symbol
        and then Att.Attr.Descr.Use_Type /= Optional
      then
         Validation_Error
           (Handler,
            "Use must be ""optional"" when a default value is specified");
      end if;

      if Get_XSD_Version (Handler.Grammar) = XSD_1_1
        and then Att.Attr.Descr.Fixed /= No_Symbol
        and then Att.Attr.Descr.Use_Type = Prohibited
      then
         Validation_Error
           (Handler,
            """prohibited"" is forbidden when"
            & " a fixed value is specified");
      end if;

      if Att.Attr.Descr.Name /= No_Qualified_Name then
         case Ctx.Typ is
            when Context_Attribute_Group | Context_Type_Def =>
               null;

            when others =>
               if Handler.Target_NS = Handler.XML_Instance_URI then
                  Validation_Error
                    (Handler,
                     "Invalid target namespace for attribute declaration: """
                     & Get (Handler.Target_NS).all & """");
               end if;
         end case;
      end if;

      Att.Attr.Descr.Is_Local := Att.Attr.Ref = No_Qualified_Name;

      Push_Context
        (Handler,
         (Typ       => Context_Attribute,
          Attribute => Att));
   end Create_Attribute;

   ----------------------
   -- Insert_Attribute --
   ----------------------

   procedure Insert_Attribute
     (Handler        : access Schema_Reader'Class;
      In_Context     : Natural;
      Attribute      : Attr_Descr)
   is
      Ctx   : Context renames Handler.Contexts (In_Context);
      Index : Natural;
      Ctx2  : Context_Access;
   begin
      case Ctx.Typ is
         when Context_Type_Def =>
            Append
              (Handler.Shared.Types.Table (Ctx.Type_Info).Attributes,
               Attribute);

         when Context_Schema | Context_Redefine =>
            Set
              (Handler.Shared.Global_Attributes,
               Attribute.Attr.Descr.Name, Attribute.Attr);

         when Context_Extension | Context_Restriction =>
            Index := Handler.Contexts_Last;
            while Index >= Handler.Contexts'First loop
               Ctx2 := Handler.Contexts (Index)'Access;
               if Ctx2.Typ = Context_Type_Def then
                  Append
                    (Handler.Shared.Types.Table (Ctx2.Type_Info).Attributes,
                     Attribute);
                  exit;
               end if;

               Index := Index - 1;
            end loop;

         when Context_Simple_Extension | Context_Simple_Restriction =>
            pragma Assert
              (Handler.Contexts (In_Context - 1).Typ = Context_Type_Def);
            pragma Assert  --  a <simpleType><extension> cannot have attributes
              (not Handler.Shared.Types.Table
                 (Handler.Contexts (In_Context - 1).Type_Info).Is_Simple);

            Append
              (Handler.Shared.Types.Table
                 (Handler.Contexts (In_Context - 1).Type_Info).Attributes,
               Attribute);

         when Context_Attribute_Group =>
            Append (Ctx.Attr_Group.Attributes, Attribute);

         when Context_Element | Context_Sequence | Context_Choice
            | Context_Attribute | Context_All
            | Context_Union | Context_List | Context_Group =>
            Validation_Error
              (Handler,
               "Unsupported: ""attribute"" in this context",
               Except => XML_Not_Implemented'Identity);
      end case;
   end Insert_Attribute;

   ----------------------
   -- Finish_Attribute --
   ----------------------

   procedure Finish_Attribute (Handler : access Schema_Reader'Class) is
      Ctx : constant Context_Access :=
        Handler.Contexts (Handler.Contexts_Last)'Access;
   begin
      Insert_Attribute (Handler, Handler.Contexts_Last - 1, Ctx.Attribute);
   end Finish_Attribute;

   -------------------
   -- Create_Schema --
   -------------------

   procedure Create_Schema
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Info   : Schema_Descr;
      Is_Set : Boolean := False;
      Name   : Qualified_Name;
   begin
      Info.Element_Form_Default   := Unqualified;
      Info.Attribute_Form_Default := Unqualified;

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.S_Element_Form_Default then
               Info.Element_Form_Default := Compute_Form (Atts, Handler, J);
            elsif Name.Local = Handler.S_Attribute_Form_Default then
               Info.Attribute_Form_Default := Compute_Form (Atts, Handler, J);
            elsif Name.Local = Handler.Block_Default then
               Compute_Blocks (Atts, Handler, Info.Block, Is_Set, J);
            elsif Name.Local = Handler.Namespace_Target then
               Info.Target_NS := Get_Value (Atts, J);
            end if;

         elsif Name.NS = Handler.XML_Instance_URI then
            if Name.Local = Handler.No_Namespace_Schema_Location then
               --  Already handled through Hook_Start_Element when validating
               --  the grammar itself, but needed if we do not validate the
               --  grammar
               Parse_Grammar
                 (Handler,
                  URI           => Empty_String,
                  Xsd_File      => Get_Value (Atts, J),
                  Do_Create_NFA => False);

            elsif Name.Local = Handler.Schema_Location then
               --  Already handled through Hook_Start_Element when validating
               --  the grammar itself
               Parse_Grammars
                 (Handler, Get_Value (Atts, J), Do_Create_NFA => False);
            end if;
         end if;
      end loop;

      if Info.Target_NS /= No_Symbol then
         if Debug then
            Output_Action ("Get_NS (Handler.Created_Grammar, """
                    & Get (Info.Target_NS).all & """, Handler.Target_NS)");
         end if;

         Handler.Target_NS := Info.Target_NS;
      end if;

      Handler.Element_Form_Default   := Info.Element_Form_Default;
      Handler.Attribute_Form_Default := Info.Attribute_Form_Default;

      if Is_Set then
         Handler.Target_Block_Default := Info.Block;
      end if;

      Push_Context (Handler, (Typ => Context_Schema));
   end Create_Schema;

   --------------------------------
   -- Process_Contents_From_Atts --
   --------------------------------

   function Process_Contents_From_Atts
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List;
      Index   : Integer) return Process_Contents_Type is
   begin
      if Get_Value (Atts, Index) = Handler.Lax then
         return Process_Lax;
      elsif Get_Value (Atts, Index) = Handler.Strict then
         return Process_Strict;
      else
         return Process_Skip;
      end if;
   end Process_Contents_From_Atts;

   ----------------
   -- Create_Any --
   ----------------

   procedure Create_Any
     (Handler : access Schema_Reader'Class;
      Atts    : Sax_Attribute_List)
   is
      Details : Type_Details_Access;
      Any     : Internal_Any_Descr;
      Name    : Qualified_Name;
      Min_Occurs, Max_Occurs : Occurrences := (False, 1);

   begin
      Any.Target_NS  := Handler.Target_NS;
      Any.Namespaces := Handler.Any_Namespace;

      for J in 1 .. Get_Length (Atts) loop
         Name := Get_Name (Atts, J);
         if Name.NS = Empty_String then
            if Name.Local = Handler.Namespace then
               Any.Namespaces := Get_Value (Atts, J);
            elsif Name.Local = Handler.Process_Contents then
               Any.Process_Contents :=
                 Process_Contents_From_Atts (Handler, Atts, J);
            end if;
         end if;
      end loop;

      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Details := new Type_Details'
        (Kind       => Type_Any,
         Min_Occurs => Min_Occurs,
         Max_Occurs => Max_Occurs,
         Loc        => Handler.Current_Location,
         In_Process => False,
         Next       => null,
         Any        => Any);
      Insert_In_Type (Handler, Details);
   end Create_Any;

   ----------------
   -- Create_All --
   ----------------

   procedure Create_All
     (Handler  : access Schema_Reader'Class;
      Atts     : Sax_Attribute_List)
   is
      Min_Occurs, Max_Occurs : Occurrences := (False, 1);
      Details : Type_Details_Access;
   begin
      Get_Occurs (Handler, Atts, Min_Occurs, Max_Occurs);
      Details := new Type_Details'
        (Kind         => Type_All,
         Min_Occurs   => Min_Occurs,
         Max_Occurs   => Max_Occurs,
         Loc          => Handler.Current_Location,
         In_Process   => False,
         Next         => null,
         First_In_All => null);
      Insert_In_Type (Handler, Details);
      Push_Context
        (Handler,
         (Typ        => Context_All,
          All_Detail => Details));
   end Create_All;

   -------------------
   -- Start_Element --
   -------------------

   overriding procedure Start_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax.Readers.Sax_Attribute_List)
   is
      H   : constant Schema_Reader_Access := Handler'Unchecked_Access;
      Ctx : Context_Access;
   begin
      if Debug then
         Output_Seen ("Start " & Get (Local_Name).all
                      & " at " & To_String (Handler.Current_Location));
      end if;

      --  Check the grammar
      Start_Element (Validating_Reader (Handler), NS, Local_Name, Atts);

      --  Process the element

      if Handler.Contexts = null then
         if Local_Name /= Handler.S_Schema then
            Validation_Error (H, "Root element must be <schema>");
         end if;

         Create_Schema (H, Atts);

      elsif Local_Name = Handler.Annotation then
         Handler.In_Annotation := True;

      elsif Local_Name = Handler.Notation then
         Create_Notation (H, Atts);

      elsif Local_Name = Handler.Element then
         Create_Element (H, Atts);

      elsif Local_Name = Handler.Complex_Type then
         Create_Complex_Type (H, Atts);

      elsif Local_Name = Handler.Simple_Type then
         Create_Simple_Type (H, Atts);

      elsif Local_Name = Handler.Restriction then
         Create_Restriction (H, Atts);

      elsif Local_Name = Handler.Extension then
         Create_Extension (H, Atts);

      elsif Local_Name = Handler.Any_Attribute then
         Create_Any_Attribute (H, Atts);

      elsif Local_Name = Handler.Pattern then
         Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
         pragma Assert (Ctx.Typ = Context_Simple_Restriction);
         pragma Assert (Ctx.Simple.Kind = Simple_Type_Restriction);
         --  Use the non-normalized value for <pattern>
         Add_Facet
           (Grammar    => Handler.Grammar,
            Facets     => Ctx.Simple.Facets,
            Facet_Name => Local_Name,
            Value      => Get_Non_Normalized_Value
              (Atts, Get_Index (Atts, Empty_String, Handler.Value)),
            Loc        => Handler.Current_Location);

      elsif Local_Name = Handler.Maxlength
        or else Local_Name = Handler.Minlength
        or else Local_Name = Handler.Length
        or else Local_Name = Handler.Enumeration
        or else Local_Name = Handler.Whitespace
        or else Local_Name = Handler.Total_Digits
        or else Local_Name = Handler.Fraction_Digits
        or else Local_Name = Handler.MaxInclusive
        or else Local_Name = Handler.MaxExclusive
        or else Local_Name = Handler.MinInclusive
        or else Local_Name = Handler.MinExclusive
      then
         Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
         pragma Assert (Ctx.Typ = Context_Simple_Restriction);
         pragma Assert (Ctx.Simple.Kind = Simple_Type_Restriction);
         Add_Facet
           (Grammar    => Handler.Grammar,
            Facets     => Ctx.Simple.Facets,
            Facet_Name => Local_Name,
            Value      => Get_Value
              (Atts, Get_Index (Atts, Empty_String, Handler.Value)),
            Loc        => Handler.Current_Location);

      elsif Local_Name = Handler.S_All then
         Create_All (H, Atts);

      elsif Local_Name = Handler.Sequence then
         Create_Sequence (H, Atts);

      elsif Local_Name = Handler.Choice then
         Create_Choice (H, Atts);

      elsif Local_Name = Handler.List then
         Create_List (H, Atts);

      elsif Local_Name = Handler.Union then
         Create_Union (H, Atts);

      elsif Local_Name = Handler.Attribute then
         Create_Attribute (H, Atts);

      elsif Local_Name = Handler.Group then
         Create_Group (H, Atts);

      elsif Local_Name = Handler.Simple_Content then
         Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
         pragma Assert (Ctx.Typ = Context_Type_Def);
         Handler.Shared.Types.Table (Ctx.Type_Info).Simple :=
           (Kind       => Simple_Type,
            In_Process => False,
            Loc        => Handler.Current_Location);
         Handler.Shared.Types.Table (Ctx.Type_Info).Properties.Mixed := True;

      elsif Local_Name = Handler.Complex_Content then
         Ctx := Handler.Contexts (Handler.Contexts_Last)'Access;
         pragma Assert (Ctx.Typ = Context_Type_Def);

         --  Do not reset Properties.Mixed here, since it might have been set
         --  to "true" on the parent <complexType> node.

      elsif Local_Name = Handler.Attribute_Group then
         Create_Attribute_Group (H, Atts);

      elsif Local_Name = Handler.Any then
         Create_Any (H, Atts);

      elsif Local_Name = Handler.Redefine then
         Validation_Error
           (Handler'Access,
            "Unsupported <redefined>",
            Except => XML_Not_Implemented'Identity);
         Create_Redefine (H, Atts);

      elsif Local_Name = Handler.Include then
         Create_Include (H, Atts);

      elsif Local_Name = Handler.Import then
         Create_Import (H, Atts);

      elsif Handler.In_Annotation then
         null;   --  ignore all tags

      elsif Handler.Feature_Ignore_Unsupported_XSD_Elements
        and then
          (Local_Name = Handler.Keyref
           or else Local_Name = Handler.Key
           or else Local_Name = Handler.Selector
           or else Local_Name = Handler.Unique
           or else Local_Name = Handler.Field)
      then
         Warning
           (Handler,
            Create (Message => "Unsupported element in the schema: "
                    & Get (Local_Name).all,
                    Loc     => Handler.Current_Location));

      else
         Validation_Error
           (Handler'Access,
            "Unsupported element in the schema: " & Get (Local_Name).all,
            Except => XML_Not_Implemented'Identity);
      end if;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   overriding procedure End_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol)
   is
      H       : constant Schema_Reader_Access := Handler'Unchecked_Access;
      Handled : Boolean := True;
   begin
      --  Check the grammar
      End_Element (Validating_Reader (Handler), NS, Local_Name);

      --  Process the tag
      if Local_Name = Handler.Element then
         Finish_Element (H);

      elsif Local_Name = Handler.S_Schema then
         null;

      elsif Local_Name = Handler.Complex_Type then
         Finish_Complex_Type (H);

      elsif Local_Name = Handler.Simple_Type then
         Finish_Simple_Type (H);

      elsif Local_Name = Handler.S_All then
         null;

      elsif Local_Name = Handler.Sequence then
         null;

      elsif Local_Name = Handler.Any_Attribute then
         Handled := False;

      elsif Local_Name = Handler.Choice then
         null;

      elsif Local_Name = Handler.Restriction then
         Finish_Restriction (H);

      elsif Local_Name = Handler.Extension then
         Finish_Extension (H);

      elsif Local_Name = Handler.Attribute then
         Finish_Attribute (H);

      elsif Local_Name = Handler.Union then
         Finish_Union (H);

      elsif Local_Name = Handler.List then
         Finish_List (H);

      elsif Local_Name = Handler.Maxlength
        or else Local_Name = Handler.Pattern
        or else Local_Name = Handler.Minlength
        or else Local_Name = Handler.Enumeration
        or else Local_Name = Handler.Whitespace
        or else Local_Name = Handler.Total_Digits
        or else Local_Name = Handler.Fraction_Digits
        or else Local_Name = Handler.MaxInclusive
        or else Local_Name = Handler.MaxExclusive
        or else Local_Name = Handler.MinInclusive
        or else Local_Name = Handler.MinExclusive
      then
         Handled := False;

      elsif Local_Name = Handler.Attribute_Group then
         Finish_Attribute_Group (H);

      elsif Local_Name = Handler.Redefine then
         null;

      elsif Local_Name = Handler.Group then
         Finish_Group (H);

      elsif Local_Name = Handler.Any
        or else Local_Name = Handler.Include
        or else Local_Name = Handler.Import
        or else Local_Name = Handler.Simple_Content
        or else Local_Name = Handler.Complex_Content
      then
         Handled := False;

      elsif Local_Name = Handler.Annotation then
         Handler.In_Annotation := False;
         Handled := False;

      else
         if Debug then
            Output_Action
              ("Close tag not handled yet: " & Get (Local_Name).all);
         end if;
         Handled := False;
      end if;

      --  Release the context
      if Handled then
         Handler.Contexts_Last := Handler.Contexts_Last - 1;
      end if;
   end End_Element;

   ----------------
   -- Characters --
   ----------------

   procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence)
   is
   begin
      Characters (Validating_Reader (Handler), Ch);
   end Characters;

   ----------
   -- Free --
   ----------

   procedure Free (Self : in out Type_Details_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Type_Details, Type_Details_Access);
      Next : Type_Details_Access;
   begin
      while Self /= null loop
         Next := Self.Next;

         case Self.Kind is
            when Type_Empty | Type_Element | Type_Any => null;
            when Type_Sequence    => Free (Self.First_In_Seq);
            when Type_Choice      => Free (Self.First_In_Choice);
            when Type_All         => Free (Self.First_In_All);
            when Type_Group       => Free (Self.Group.Details);
            when Type_Extension   =>
               Free (Self.Extension.Details);
            when Type_Restriction =>
               Free (Self.Restriction.Details);
         end case;

         Unchecked_Free (Self);
         Self := Next;
      end loop;
   end Free;

   -----------------
   -- Set_Feature --
   -----------------

   overriding procedure Set_Feature
     (Parser : in out Schema_Reader; Name : String; Value : Boolean) is
   begin
      if Name = Feature_Ignore_Unsupported_XSD_Elements then
         Parser.Feature_Ignore_Unsupported_XSD_Elements := Value;
      else
         Set_Feature (Validating_Reader (Parser), Name, Value);
      end if;
   end Set_Feature;

   -----------------
   -- Get_Feature --
   -----------------

   overriding function Get_Feature
     (Parser : Schema_Reader; Name : String) return Boolean is
   begin
      if Name = Feature_Ignore_Unsupported_XSD_Elements then
         return Parser.Feature_Ignore_Unsupported_XSD_Elements;
      else
         return Get_Feature (Validating_Reader (Parser), Name);
      end if;
   end Get_Feature;

end Schema.Schema_Readers;
