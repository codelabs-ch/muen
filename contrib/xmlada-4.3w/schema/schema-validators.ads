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

with Ada.Exceptions;
with GNAT.Dynamic_Tables;
with Interfaces;
with Unicode.CES;
with Sax.HTable;
with Sax.Locators;
with Sax.Pointers;
with Sax.Readers;            use Sax.Readers;
with Sax.State_Machines;
with Sax.Symbols;
with Sax.Utils;
with Schema.Simple_Types;

package Schema.Validators is

   XML_Schema_URI : constant Unicode.CES.Byte_Sequence :=
     "http://www.w3.org/2001/XMLSchema";
   XML_URI : constant Unicode.CES.Byte_Sequence :=
     "http://www.w3.org/XML/1998/namespace";
   XML_Instance_URI : constant Unicode.CES.Byte_Sequence :=
     "http://www.w3.org/2001/XMLSchema-instance";

   XML_Validation_Error : exception;
   --  Raised in case of error in the validation process. The exception message
   --  contains the error, but not its location

   type XSD_Versions is (XSD_1_0, XSD_1_1);
   --  The version of XSD the parser should support.
   --  The support for 1.1 is only partial at present.

   type XML_Grammar is private;
   --  A grammar can contain the definition for multiple namespaces (generally
   --  the standard XML Schema namespace for predefined types, and the
   --  namespace we are defining).
   --  A grammar is a smart pointer, and will take care of freeing memory
   --  automatically when no longer needed.

   procedure Set_XSD_Version
     (Grammar     : in out XML_Grammar;
      XSD_Version : XSD_Versions);
   function Get_XSD_Version (Grammar : XML_Grammar) return XSD_Versions;
   --  Set the version of XSD accepted by this grammar

   function Get_Symbol_Table
     (Grammar : XML_Grammar) return Sax.Utils.Symbol_Table;
   procedure Set_Symbol_Table
     (Grammar : XML_Grammar; Symbols : Sax.Utils.Symbol_Table);
   --  The symbol table used to create the grammar.
   --  Any parser using this grammmar must also use the same symbol table,
   --  otherwise no validation can succeed (this is ensured by special tests in
   --  Set_Grammar and Set_Symbol_Table).

   No_Grammar : constant XML_Grammar;
   --  No Grammar has been defined

   type Occurrences (Unbounded : Boolean := False) is record
      case Unbounded is
         when True =>
            null;
         when False =>
            Value : Natural;
      end case;
   end record;
   --  The number of occurrences

   type Form_Type is (Qualified, Unqualified);
   --  Whether locally declared elements need to be qualified or whether
   --  qualification is optional (the latter is the default). This does not
   --  apply to global elements, that always need to be qualified (or found in
   --  the default namespace).
   --  Note that elements defined in a <group> are considered local only if
   --  they do not use the R.Ref attribute, otherwise they are considered
   --  global and therefore the "form" does not apply to them.

   type Process_Contents_Type is (Process_Strict, Process_Lax, Process_Skip);
   --  When in an element that accepts any children (ur-type, or xsd:any), this
   --  type indicates that should be done to validate the children:
   --     Strict: the children must have a definition in the schema (as a
   --             global element)
   --     Lax:    if the children have a definition, it is used, otherwise they
   --             are just accepted as is.
   --     Skip:   even if the children have a definition, it is ignored, and
   --             the child is processed as a ur-type.

   --------------------
   -- State machines --
   --------------------
   --  The validators are implemented as state machines

   type Header_Num is new Interfaces.Integer_32 range 0 .. 1023;
   function Hash (Name : Qualified_Name) return Header_Num;
   function Hash (Name : Sax.Symbols.Symbol) return Header_Num;
   --  Suitable for instantiating hash tables

   type Any_Descr is record
      Process_Contents : Process_Contents_Type := Process_Strict;
      No_Namespaces    : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Namespaces       : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      --  The combined list of namespaces. This could include ##any and
      --  ##local, since there is no way to represent them otherwise, but will
      --  not include ##targetNamespace which must be resolved first.
      --  No_Namespaces is the list of namespaces we must not match, and
      --  replaces the use of ##other in the list of namespaces. (note that if
      --  a namespace matches [Namespaces], it will match even if it is in
      --  [No_Namespaces].
      --
      --  Combining <anyAttribute>:
      --  - when we have an extension, we must match any of the namespaces
      --    either from the base or from the extension.
      --  - when we have a restriction, we restrict the list of valid
      --    namespaces.
   end record;
   No_Any_Descr : constant Any_Descr := (others => <>);

   function Combine
     (Grammar             : XML_Grammar;
      Base_Any            : Any_Descr;
      Local_Process       : Process_Contents_Type;
      Local               : Sax.Symbols.Symbol;  --  includes ##other
      As_Restriction      : Boolean;
      Target_NS           : Sax.Symbols.Symbol) return Any_Descr;
   --  Combines [Base_Any] and [Local_Any] into a single one.
   --  [Base_Any] can be No_Any_Descr if we simply want to resolve
   --  ##targetNamespace and ##other in [Local_Any]

   function Match_Any
     (Any : Any_Descr; Name : Qualified_Name) return Boolean;
   --  Whether [Name] matches the namespaces in [Any]

   type Transition_Event is record
      Name    : Qualified_Name;
      Closing : Boolean := False;
   end record;
   --  The event to do a transition in the state machine.
   --  [Closing] is set to true if we are seeing the end tag for [Name]

   type Transition_Kind is (Transition_Symbol,
                            Transition_Any,
                            Transition_Close,
                            Transition_Symbol_From_All,
                            Transition_Close_From_All
                           );
   --  Transition_*_From_All is used to support the <all> construct without
   --  creating a set of states for all possible permuations of child elements
   --  (since otherwise the number of states explodes). Instead, an <all> node
   --  has one output transition per possible child elements (these transitions
   --  are Transition_Symbol_From_All). These transitions are disabled once
   --  they have been visited (since maxOccurs=1 for children of <all>) and
   --  cannot be visited again. The <all> state also has one output transition
   --  to the next state. This transition is also conditional: it will be on
   --  active on any symbol, provided that all children of <all> have been
   --  visited once or have minOccurs=0.
   --  This also requires temporary data associated with all active instances
   --  of the <all> state in the Matcher, to remember which children have been
   --  visited.

   type Visited_All_Children is mod 2 ** 32;
   --  A mask for the children of an <all> element that have been visited.
   --  Such children can be visited at most once. In the transition, we store
   --  the mask for all such children that must be visited (the optional ones
   --  have a 0 in the mask). In the Matcher, we also store this info in the
   --  <all> node itself to make sure that children are not visited more than
   --  once.

   type Transition_Descr (Kind : Transition_Kind := Transition_Symbol) is
      record
         case Kind is
            when Transition_Symbol | Transition_Symbol_From_All =>
               Name : Qualified_Name;
               Form : Form_Type := Qualified;

               --  For <all> nodes: the index of this transition in the mask
               --  (Visited_All_Children) for <all>. This is used to memorize
               --  which children have already been visited.

               All_Child_Index : Integer;

            when Transition_Close_From_All =>
               Mask : Visited_All_Children := 0;

            when Transition_Close =>
               null;

            when Transition_Any    =>
               Any : Any_Descr;
         end case;
      end record;

   type Named_Attribute_List is new Natural;
   Empty_Named_Attribute_List : constant Named_Attribute_List := 0;

   type Attributes_List is record
      Any   : Any_Descr := No_Any_Descr;
      Named : Named_Attribute_List := Empty_Named_Attribute_List;
   end record;
   No_Attributes : constant Attributes_List := (others => <>);
   --  All types are assumed to have a <anyAttribute> even if it never
   --  accepts anything. For an extension or restriction, it is merged with the
   --  base's <anyAttribute>

   type Block_Type is (Block_Restriction,
                       Block_Extension,
                       Block_Substitution);
   type Block_Status is array (Block_Type) of Boolean;
   pragma Pack (Block_Status);
   No_Block : constant Block_Status := (others => False);

   function To_String (Blocks : Block_Status) return String;
   --  Return a displayable version of [Blocks], for debugging purposes.

   type Final_Type is (Final_Restriction,
                       Final_Extension,
                       Final_Union,
                       Final_List);
   type Final_Status is array (Final_Type) of Boolean;
   pragma Pack (Final_Status);

   type Type_Index is new Natural;
   No_Type_Index : constant Type_Index := 0;
   --  Index into a global table that contains the [Type_Descr].
   --  Going through a table instead of storing directly a [Type_Descr] (for
   --  instance in NFA states) reduces memory usage, but more importantly
   --  means that we can modify the type even once the NFA has been created,
   --  and still impact all states that reference that type.

   type Attribute_Use_Type is (Prohibited, Optional, Required, Default);

   type Attribute_Descr is record
      Target_NS    : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Name         : Qualified_Name     := No_Qualified_Name;
      Simple_Type  : Schema.Simple_Types.Simple_Type_Index :=
        Schema.Simple_Types.No_Simple_Type_Index;
      Fixed        : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Default      : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Use_Type     : Attribute_Use_Type := Optional;
      Form         : Form_Type          := Qualified;
      Is_Local     : Boolean            := True;
      Next         : Named_Attribute_List := Empty_Named_Attribute_List;
   end record;
   pragma Pack (Attribute_Descr);
   No_Attribute_Descr : constant Attribute_Descr := (others => <>);

   function Image (Trans : Transition_Descr) return String;
   --  Needed for the instantiation of Sax.State_Machines

   type State_Data is record
      Simple   : Type_Index;
      Fixed    : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Default  : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Block    : Block_Status := No_Block;
      Nillable : Boolean := False;
   end record;
   No_State_Data : constant State_Data :=
     (No_Type_Index, Sax.Symbols.No_Symbol,
      Sax.Symbols.No_Symbol, No_Block, False);
   --  User data associated with each state. This mostly point to the
   --  corresponding type in the schema, but also includes overridding data
   --  from the corresponding element itself.

   package Schema_State_Machines is new Sax.State_Machines
      (Symbol              => Transition_Event,
       Transition_Symbol   => Transition_Descr,
       Image               => Image,
       State_User_Data     => State_Data,
       Default_Data        => No_State_Data,
       Default_State_Count => 200,       --  XSD metaschema takes 904 states
       Default_Transition_Count => 200); --  XSD metaschema takes 1096
   use Schema_State_Machines;

   type Type_Descr is record
      Name           : Qualified_Name := No_Qualified_Name;
      Attributes     : Attributes_List := No_Attributes;
      Block          : Block_Status := No_Block;
      Final          : Final_Status := (others => False);

      Restriction_Of : Type_Index := No_Type_Index;
      Extension_Of   : Type_Index := No_Type_Index;

      Simple_Content : Schema.Simple_Types.Simple_Type_Index :=
        Schema.Simple_Types.No_Simple_Type_Index;
      --  set if we have a simpleType or simpleContent

      Mixed          : Boolean := False;
      Is_Abstract    : Boolean := False;

      Complex_Content : Schema_State_Machines.State :=
        Schema_State_Machines.No_State;
      --  The start of the nested NFA for a complexType
   end record;
   type Type_Descr_Access is access all Type_Descr;
   pragma Pack (Type_Descr);
   No_Type_Descr : constant Type_Descr := (others => <>);

   function Image
     (Self : access NFA'Class;
      S    : Schema_State_Machines.State;
      Data : State_Data) return String;
   --  Needed for the instantiation of Pretty_Printers

   type Active_State_Data is record
      Visited : Visited_All_Children := 0;
   end record;
   No_Active_Data : constant Active_State_Data := (Visited => 0);

   function Match
     (Self       : access Abstract_NFA_Matcher'Class;
      From_State, To_State : State;
      Parent_Data : access Active_State_Data;
      Trans      : Transition_Descr;
      Sym        : Transition_Event) return Boolean;
   --  Whether [Sym] matches [Trans].
   --  Parent_Data is the execution data associated with the parent state in
   --  which From_State is nested. It is used to validate <all> nodes (which
   --  needs to check that all children are either optional or were visited).

   function Expected
     (Self       : Abstract_NFA_Matcher'Class;
      From_State, To_State : State;
      Parent_Data : access Active_State_Data;
      Trans      : Transition_Descr) return String;
   --  What to display in "expecting ..." for this transition.

   package Schema_State_Machines_PP
     is new Schema_State_Machines.Pretty_Printers (Image);
   package Schema_State_Machines_Matchers
     is new Schema_State_Machines.Matchers
       (Active_State_Data, No_Active_Data, Match, Expected);

   type Schema_NFA is new Schema_State_Machines.NFA with private;
   type Schema_NFA_Access is access all Schema_NFA'Class;
   type Schema_NFA_Matcher
     is new Schema_State_Machines_Matchers.NFA_Matcher with private;

   procedure Do_Match
     (Matcher         : in out Schema_NFA_Matcher;
      Sym             : Transition_Event;
      Success         : out Boolean;
      Through_Any     : out Boolean;
      Through_Process : out Process_Contents_Type);
   --  Process the next event through NFA, and report whether it matched
   --  through a <any>

   function Ur_Type
     (NFA              : access Schema_NFA'Class;
      Process_Contents : Process_Contents_Type)
      return Schema_State_Machines.Nested_NFA;
   --  Return the nested NFA for <ur-Type>

   type Reference_Kind is (Ref_Element,
                           Ref_Type,
                           Ref_Attribute,
                           Ref_Group,
                           Ref_AttrGroup);
   type Global_Reference (Kind : Reference_Kind := Ref_Element) is record
      Name : Qualified_Name;
      case Kind is
         when Ref_Element   => Element    : State;
         when Ref_Type      => Typ        : Type_Index;
         when Ref_Group     => Gr_Start, Gr_End : State;
         when Ref_Attribute | Ref_AttrGroup => Attributes : Attributes_List;
      end case;
   end record;
   No_Global_Reference : constant Global_Reference :=
     (Ref_Type, Name => No_Qualified_Name, Typ => No_Type_Index);
   --  The global elements in a grammar that can be referenced from another
   --  grammar (or from an XML file).

   type Reference_Name is record
      Name : Qualified_Name;
      Kind : Reference_Kind;
   end record;
   function Hash (Name : Reference_Name) return Interfaces.Unsigned_32;

   function Get_Key (Ref : Global_Reference) return Reference_Name;

   package Reference_HTables is new Sax.HTable
     (Element       => Global_Reference,
      Empty_Element => No_Global_Reference,
      Key           => Reference_Name,
      Get_Key       => Get_Key,
      Hash          => Hash,
      Equal         => "=");
   type Reference_HTable is access Reference_HTables.HTable;

   Reference_HTable_Size : constant := 1023;
   --  Size created for the references table

   function Get_NFA (Grammar : XML_Grammar) return Schema_NFA_Access;
   function Get_References (Grammar : XML_Grammar) return Reference_HTable;
   --  Returns the state machine and global references used to validate
   --  [Grammar]

   function Dump_Dot_NFA
     (Grammar            : XML_Grammar;
      Nested             : Nested_NFA := No_Nested) return String;
   --  Return a "dot" graph for the NFA, possibly restricted to a specific
   --  nested NFA.

   function Get_Simple_Type
     (NFA    : access Schema_NFA'Class;
      Simple : Schema.Simple_Types.Simple_Type_Index)
      return Schema.Simple_Types.Simple_Type_Descr;
   pragma Inline (Get_Simple_Type);
   --  Return the simple type corresponding to the index

   function Get_Type_Descr
     (NFA   : access Schema_NFA'Class;
      Index : Type_Index) return access Type_Descr;
   pragma Inline (Get_Type_Descr);
   --  Return the type description at that index

   ------------
   -- Parser --
   ------------
   --  See packages Schema.Readers and Schema.Schema_Readers for non-abstract
   --  implementation of those.

   type Abstract_Validation_Reader
     is abstract new Sax.Readers.Sax_Reader
   with record
      Error_Location : Sax.Locators.Location;
      Error_Msg      : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;

      Id_Table       : Schema.Simple_Types.Symbol_Htable_Access;
      --  Mapping of IDs to elements

      Grammar   : XML_Grammar := No_Grammar;

      All_NNI                : Sax.Symbols.Symbol; --  "allNNI"
      Annotated              : Sax.Symbols.Symbol; --  "annotated"
      Annotation             : Sax.Symbols.Symbol; --  "annotation"
      Any                    : Sax.Symbols.Symbol; --  "any"
      Any_Attribute          : Sax.Symbols.Symbol; --  "anyAttribute"
      Any_Namespace          : Sax.Symbols.Symbol;  --  "##any"
      Any_Simple_Type        : Sax.Symbols.Symbol; --  "anySimpleType"
      Anytype                : Sax.Symbols.Symbol;  --  "anyType"
      Appinfo                : Sax.Symbols.Symbol; --  "appinfo"
      Attr_Decls             : Sax.Symbols.Symbol; --  "attrDecls"
      Attribute              : Sax.Symbols.Symbol; --  "attribute"
      Attribute_Group        : Sax.Symbols.Symbol; --  "attributeGroup"
      Attribute_Group_Ref    : Sax.Symbols.Symbol; -- "attributeGroupRef"
      Base                   : Sax.Symbols.Symbol; --  "base"
      Block                  : Sax.Symbols.Symbol; --  "block"
      Block_Default          : Sax.Symbols.Symbol; --  "blockDefault"
      Block_Set              : Sax.Symbols.Symbol; --  "blockSet"
      Choice                 : Sax.Symbols.Symbol; --  "choice"
      Complex_Content        : Sax.Symbols.Symbol; --  "complexContent"
      Complex_Extension_Type : Sax.Symbols.Symbol; --  "complexExtensionType"
      Complex_Restriction_Type : Sax.Symbols.Symbol;
      Complex_Type           : Sax.Symbols.Symbol; --  "complexType"
      Complex_Type_Model     : Sax.Symbols.Symbol; -- "complexTypeModel"
      Def_Ref                : Sax.Symbols.Symbol; --  "defRef"
      Default                : Sax.Symbols.Symbol; --  "default"
      Derivation_Control     : Sax.Symbols.Symbol; -- "derivationControl"
      Derivation_Set         : Sax.Symbols.Symbol; --  "derivationSet"
      Documentation          : Sax.Symbols.Symbol; --  "documentation"
      Element                : Sax.Symbols.Symbol; --  "element"
      Enumeration            : Sax.Symbols.Symbol;  --  "enumeration"
      Explicit_Group         : Sax.Symbols.Symbol; --  "explicitGroup"
      Extension              : Sax.Symbols.Symbol; --  "extension"
      Extension_Type         : Sax.Symbols.Symbol; --  "extensionType"
      Facet                  : Sax.Symbols.Symbol; --  "facet"
      Field                  : Sax.Symbols.Symbol; -- "field"
      Final                  : Sax.Symbols.Symbol; --  "final"
      Final_Default          : Sax.Symbols.Symbol; --  "finalDefault"
      Fixed                  : Sax.Symbols.Symbol; --  "fixed"
      Form                   : Sax.Symbols.Symbol; --  "form"
      Form_Choice            : Sax.Symbols.Symbol; -- "formChoice
      Fraction_Digits        : Sax.Symbols.Symbol;
      Group                  : Sax.Symbols.Symbol; --  "group"
      Group_Def_Particle     : Sax.Symbols.Symbol; --  "groupDefParticle"
      Group_Ref              : Sax.Symbols.Symbol; --  "groupRef"
      Id                     : Sax.Symbols.Symbol; --  "id"
      IDREF                  : Sax.Symbols.Symbol; --  "IDREF"
      IDREFS                 : Sax.Symbols.Symbol; --  "IDREFS"
      Identity_Constraint    : Sax.Symbols.Symbol; --  "identityConstraint"
      Import                 : Sax.Symbols.Symbol; --  "import"
      Include                : Sax.Symbols.Symbol; --  "include"
      Item_Type              : Sax.Symbols.Symbol; --  "itemType"
      Key                    : Sax.Symbols.Symbol; --  "key"
      Keybase                : Sax.Symbols.Symbol; --  "keybase"
      Keyref                 : Sax.Symbols.Symbol; --  "keyref"
      Lang                   : Sax.Symbols.Symbol; --  "lang"
      Lax                    : Sax.Symbols.Symbol; --  "lax"
      Length                 : Sax.Symbols.Symbol;
      List                   : Sax.Symbols.Symbol; --  "list"
      Local                  : Sax.Symbols.Symbol;
      Local_Complex_Type     : Sax.Symbols.Symbol; --  "localComplexType"
      Local_Element          : Sax.Symbols.Symbol; --  "localElement"
      Local_Simple_Type      : Sax.Symbols.Symbol; --  "localSimpleType"
      MaxExclusive           : Sax.Symbols.Symbol;
      MaxInclusive           : Sax.Symbols.Symbol;
      MaxOccurs              : Sax.Symbols.Symbol;
      Max_Bound              : Sax.Symbols.Symbol; --  "maxBound"
      Maxlength              : Sax.Symbols.Symbol;  --  "maxLength"
      Member_Types           : Sax.Symbols.Symbol; --  "memberTypes"
      MinExclusive           : Sax.Symbols.Symbol;
      MinInclusive           : Sax.Symbols.Symbol;
      MinOccurs              : Sax.Symbols.Symbol;
      Min_Bound              : Sax.Symbols.Symbol; --  "minBound"
      Minlength              : Sax.Symbols.Symbol;  --  "minLength"
      Mixed                  : Sax.Symbols.Symbol; --  "mixed"
      NCName                 : Sax.Symbols.Symbol; --  "NCName"
      NMTOKEN                : Sax.Symbols.Symbol; --  "NMTOKEN"
      Name                   : Sax.Symbols.Symbol;
      Named_Attribute_Group  : Sax.Symbols.Symbol; --  "namedAttributeGroup"
      Named_Group            : Sax.Symbols.Symbol; --  "namedGroup"
      Namespace              : Sax.Symbols.Symbol;
      Namespace_List         : Sax.Symbols.Symbol; --  "namespaceList"
      Namespace_Target       : Sax.Symbols.Symbol; --  "targetNamespace"
      Nested_Particle        : Sax.Symbols.Symbol; --  "nestedParticle"
      Nil                    : Sax.Symbols.Symbol;
      Nillable               : Sax.Symbols.Symbol; --  "nillable"
      No_Namespace_Schema_Location : Sax.Symbols.Symbol;
      Non_Negative_Integer   : Sax.Symbols.Symbol; --  "nonNegativeInteger"
      Notation               : Sax.Symbols.Symbol; -- "notation"
      Num_Facet              : Sax.Symbols.Symbol; --  "numFacet"
      Occurs                 : Sax.Symbols.Symbol; -- "occurs"
      Open_Attrs             : Sax.Symbols.Symbol; --  "openAttrs"
      Optional               : Sax.Symbols.Symbol; --  "optional"
      Other_Namespace        : Sax.Symbols.Symbol;
      Particle               : Sax.Symbols.Symbol; --  "particle"
      Pattern                : Sax.Symbols.Symbol;
      Positive_Integer       : Sax.Symbols.Symbol;
      Precision_Decimal      : Sax.Symbols.Symbol;
      Process_Contents       : Sax.Symbols.Symbol; --  "processContents"
      Prohibited             : Sax.Symbols.Symbol; --  "prohibited"
      Public                 : Sax.Symbols.Symbol; --  "public"
      QName                  : Sax.Symbols.Symbol; --  "QName"
      Qualified              : Sax.Symbols.Symbol; --  "qualified"
      Real_Group             : Sax.Symbols.Symbol; -- "realGroup"
      Redefinable            : Sax.Symbols.Symbol; --  "redefinable"
      Redefine               : Sax.Symbols.Symbol; --  "redefine"
      Reduced_Derivation_Control : Sax.Symbols.Symbol;
      Ref                    : Sax.Symbols.Symbol;
      Refer                  : Sax.Symbols.Symbol; --  "refer"
      Required               : Sax.Symbols.Symbol; --  "required"
      Restriction            : Sax.Symbols.Symbol; --  "restriction"
      Restriction_Type       : Sax.Symbols.Symbol; --  "restrictionType"
      S_1                    : Sax.Symbols.Symbol; --  "1"
      S_Abstract             : Sax.Symbols.Symbol; --  "abstract"
      S_All                  : Sax.Symbols.Symbol; --  "all"
      S_Attribute_Form_Default : Sax.Symbols.Symbol; --  "attributeFormDefault"
      S_Boolean              : Sax.Symbols.Symbol; --  "boolean"
      S_Element_Form_Default : Sax.Symbols.Symbol; --  "elementFormDefault"
      S_False                : Sax.Symbols.Symbol; --  "false"
      S_Schema               : Sax.Symbols.Symbol; --  "schema"
      S_String               : Sax.Symbols.Symbol; --  "string"
      S_Use                  : Sax.Symbols.Symbol; --  "use"
      Schema_Location        : Sax.Symbols.Symbol;
      Schema_Top             : Sax.Symbols.Symbol; --  "schemaTop"
      Selector               : Sax.Symbols.Symbol; --  "selector"
      Sequence               : Sax.Symbols.Symbol; --  "sequence"
      Simple_Content         : Sax.Symbols.Symbol; --  "simpleContent"
      Simple_Derivation      : Sax.Symbols.Symbol; --  "simpleDerivation"
      Simple_Derivation_Set  : Sax.Symbols.Symbol; --  "simpleDerivationSet"
      Simple_Extension_Type  : Sax.Symbols.Symbol; --  "simpleExtensionType"
      Simple_Restriction_Model : Sax.Symbols.Symbol;
      Simple_Restriction_Type  : Sax.Symbols.Symbol;
      Simple_Type            : Sax.Symbols.Symbol; --  "simpleType"
      Source                 : Sax.Symbols.Symbol; --  "source"
      Strict                 : Sax.Symbols.Symbol; --  "strict"
      Substitution_Group     : Sax.Symbols.Symbol; --  "substitutionGroup"
      System                 : Sax.Symbols.Symbol; --  "system"
      Target_Namespace       : Sax.Symbols.Symbol; --  "##targetNamespace"
      Token                  : Sax.Symbols.Symbol; --  "token"
      Top_Level_Attribute    : Sax.Symbols.Symbol; --  "topLevelAttribute"
      Top_Level_Complex_Type : Sax.Symbols.Symbol; --  "topLevelComplexType"
      Top_Level_Element      : Sax.Symbols.Symbol; --  "topLevelElement"
      Top_Level_Simple_Type  : Sax.Symbols.Symbol; --  "topLevelSimpleType"
      Total_Digits           : Sax.Symbols.Symbol;
      Typ                    : Sax.Symbols.Symbol;
      Type_Def_Particle      : Sax.Symbols.Symbol; --  "typeDefParticle"
      UC_ID                  : Sax.Symbols.Symbol; --  "ID"
      URI_Reference          : Sax.Symbols.Symbol; --  "uriReference"
      Unbounded              : Sax.Symbols.Symbol;
      Union                  : Sax.Symbols.Symbol; --  "union"
      Unique                 : Sax.Symbols.Symbol; -- "unique"
      Unqualified            : Sax.Symbols.Symbol; --  "unqualified"
      Ur_Type                : Sax.Symbols.Symbol; --  "ur-Type"
      Value                  : Sax.Symbols.Symbol; --  "value"
      Version                : Sax.Symbols.Symbol; --  "version"
      Whitespace             : Sax.Symbols.Symbol;
      Wildcard               : Sax.Symbols.Symbol; --  "wildcard"
      XML_Instance_URI       : Sax.Symbols.Symbol;
      XML_Schema_URI         : Sax.Symbols.Symbol;
      XML_URI                : Sax.Symbols.Symbol; --  XML_URI
      XPath                  : Sax.Symbols.Symbol; --  "xpath"
      XPath_Expr_Approx      : Sax.Symbols.Symbol; --  "XPathExprApprox"
      XPath_Spec             : Sax.Symbols.Symbol; --  "XPathSpec"
      Xmlns                  : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
   end record;
   type Abstract_Validating_Reader_Access
     is access all Abstract_Validation_Reader'Class;

   procedure Free (Reader : in out Abstract_Validation_Reader);
   --  Free the memory used by Reader

   overriding procedure Initialize_Symbols
     (Parser : in out Abstract_Validation_Reader);
   --  See inherited documentation

   procedure Validation_Error
     (Reader  : access Abstract_Validation_Reader;
      Message : Unicode.CES.Byte_Sequence;
      Loc     : Sax.Locators.Location := Sax.Locators.No_Location;
      Except  : Ada.Exceptions.Exception_Id := XML_Validation_Error'Identity);
   --  Sets an error message, and raise XML_Validation_Error.
   --  If [Message] starts with "#", this indicates a non-implemented
   --  feature, and XML_Not_Implemented is raised instead.

   function Get_Error_Message
     (Reader : Abstract_Validation_Reader) return Unicode.CES.Byte_Sequence;
   --  Return the current error message

   procedure Check_Substitution_Group_OK
     (Handler : access Abstract_Validation_Reader'Class;
      New_Type, Old_Type : Type_Index;
      Loc     : Sax.Locators.Location;
      Element_Block : Block_Status);
   --  Verifies that [New_Type] is a valid substitution for [Old_Type],
   --  according to 3.3.6.3.
   --  If not, raises a [Validation_Error]

   -------------------------
   -- Attribute_Validator --
   -------------------------

   type Namespace_Kind is (Namespace_Other, Namespace_Any, Namespace_List);
   --  "Any":   any non-conflicting namespace
   --  "Other": any non-conflicting namespace other than targetNamespace
   --  Namespace_List can contain "##local", "##targetNamespace" or actual
   --  namespaces.

   ---------------------
   -- Type validators --
   ---------------------
   --  Such validators are build to validate specific parts of an XML
   --  document (a whole element).

   procedure Validate_Simple_Type
     (Reader        : access Abstract_Validation_Reader'Class;
      Simple_Type   : Schema.Simple_Types.Simple_Type_Index;
      Ch            : Unicode.CES.Byte_Sequence;
      Loc           : Sax.Locators.Location;
      Insert_Id     : Boolean := True);
   --  Validate [Ch] as a simpleType
   --  If [Insert_Id] is True, and the type is ID, it is inserted in a global
   --  htables. Thus calling this procedure twice with this parameter set to
   --  true will result in a "duplicate id" error.

   procedure Normalize_And_Validate
     (Parser  : access Abstract_Validation_Reader'Class;
      Simple  : Schema.Simple_Types.Simple_Type_Index;
      Fixed   : in out Sax.Symbols.Symbol;
      Loc     : Sax.Locators.Location);
   --  Normalize whitespaces in [Fixed] according to the simple type.

   function Equal
     (Reader        : access Abstract_Validation_Reader'Class;
      Simple_Type   : Schema.Simple_Types.Simple_Type_Index;
      Ch1           : Sax.Symbols.Symbol;
      Ch2           : Unicode.CES.Byte_Sequence) return Boolean;
   --  Checks whether [Ch1]=[Ch2] according to the type (possibly involving
   --  whitespace normalization)

   procedure Validate_Attributes
     (NFA       : access Schema_NFA'Class;
      Typ       : access Type_Descr;
      Reader    : access Abstract_Validation_Reader'Class;
      Atts      : in out Sax.Readers.Sax_Attribute_List;
      Is_Nil    : in out Integer);
   --  Check whether this list of attributes is valid for elements associated
   --  with this validator. By default, this simply check whether the list of
   --  attributes registered through Add_Attribute matches Atts.
   --
   --  Id_Table is used to ensure that two same Ids are not in the document. It
   --  is passed as an access type, so that in case of exception it is still
   --  properly set on exit.
   --
   --  [Is_Nil] is set to the index in [Atts] for the xsi:nil attribute, or
   --  -1 if not found.
   --
   --  Sets the type of the attributes (through Sax.Attributes.Set_Type) to Id
   --  if the corresponding attribute is an id.

   type Internal_Any_Descr is record
      Target_NS        : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Process_Contents : Process_Contents_Type := Process_Strict;
      Namespaces       : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
   end record;
   No_Internal_Any_Descr : constant Internal_Any_Descr := (others => <>);
   --  We need to temporarily store the target_NS, in case we are parsing
   --  multiple grammars before we generate the NFA

   procedure Add_Any_Attribute
     (Grammar        : XML_Grammar;
      List           : in out Attributes_List;
      Any            : Internal_Any_Descr;
      As_Restriction : Boolean);
   procedure Add_Attribute
     (Parser         : access Abstract_Validation_Reader'Class;
      List           : in out Attributes_List;
      Attribute      : Attribute_Descr;
      Ref            : Named_Attribute_List := Empty_Named_Attribute_List;
      Loc            : Sax.Locators.Location);
   procedure Add_Attributes
     (Parser         : access Abstract_Validation_Reader'Class;
      List           : in out Attributes_List;
      Attributes     : Attributes_List;
      As_Restriction : Boolean;
      Loc            : Sax.Locators.Location);
   --  Add a valid attribute to Validator.
   --  Is_Local should be true if the attribute is local, or False if this is
   --  a reference to a global attribute.
   --  The second version copies elements from [Attributes] into [List].
   --  [As_Restriction] is used when including a <anyAttribute>. Since there
   --  can be only one in the list, this is merged with any existing
   --  <anyAttribute>. [Target_NS] is also used in this context.
   --  [Ref], if specified, is the "refed" attribute. Its type is used, but
   --  the use type of [Attribute] is used, instead.

   --------------
   -- Grammars --
   --------------

   procedure Initialize_Grammar
     (Reader : in out Abstract_Validation_Reader'Class);
   --  Initialize the internal structure of the grammar.
   --  This adds the definition for all predefined types

   procedure Reset (Grammar : in out XML_Grammar);
   --  Partial reset of the grammar: all the namespace-specific grammars are
   --  deleted, except for the grammar used to validate the XSD files
   --  themselves. This is mostly convenient if you want to reuse a grammar
   --  to handle _lots_ of unrelated XSD files (if your application only uses
   --  a few of these, you can easily store them all in the same grammar, but
   --  if you have hundreds of them, it might be more memory-efficient to
   --  discard the namespaces you no longer use).
   --  Keeping the grammar for the XSD files provides a minor optimization,
   --  avoiding the need to recreate it the next time you parse a XSD file.
   --
   --  TASKING: you should not call this procedure while some parsers are still
   --  using the grammar.

   procedure Create_Global_Attribute
     (Parser    : access Abstract_Validation_Reader'Class;
      Attr      : Attribute_Descr;
      Loc       : Sax.Locators.Location);
   function Create_Simple_Type
     (NFA   : access Schema_NFA'Class;
      Descr : Schema.Simple_Types.Simple_Type_Descr)
      return Schema.Simple_Types.Simple_Type_Index;
   function Create_Type
     (NFA   : access Schema_NFA'Class;
      Descr : Type_Descr) return Type_Index;
   --  Register a global attribute or type.
   --  [Name] or [Descr.Name] can be [No_Qualified_Name], in which case a local
   --  type is created (ie not registered in the list of global elements).

   procedure Add_Facet
     (Grammar      : XML_Grammar;
      Facets       : in out Schema.Simple_Types.All_Facets;
      Facet_Name   : Sax.Symbols.Symbol;
      Value        : Sax.Symbols.Symbol;
      Loc          : Sax.Locators.Location);
   pragma Inline (Add_Facet);
   --  See doc in schema-simple_types, this is a proxy

   function URI_Was_Parsed
     (Grammar : XML_Grammar;
      URI     : Sax.Symbols.Symbol) return Boolean;
   --  Return True if the schema at URI was already parsed and included in
   --  Grammar. URI must be an absolute URI.

   procedure Set_Parsed_URI
     (Reader  : in out Abstract_Validation_Reader'Class;
      URI     : Sax.Symbols.Symbol);
   --  Indicate that the schema found at URI was fully parsed and integrated
   --  into Grammar. It can then be tested through URI_Was_Parsed.

   procedure Debug_Dump (Grammar : XML_Grammar);
   --  Dump the grammar to stdout. This is for debug only

   function To_QName (Name : Qualified_Name) return Unicode.CES.Byte_Sequence;
   --  Return the name as it should be displayed in error messages

   function Simple_Nested
     (NFA : access Schema_NFA'Class) return Schema_State_Machines.State;

   procedure Add_Notation
     (NFA : access Schema_NFA'Class; Name : Sax.Symbols.Symbol);
   --  Register a new NOTATION

private

   -------------------------
   -- Attribute_Validator --
   -------------------------

   package Attributes_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Attribute_Descr,
      Table_Index_Type     => Named_Attribute_List,
      Table_Low_Bound      => Empty_Named_Attribute_List + 1,
      Table_Initial        => 200,
      Table_Increment      => 200);

   package Types_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Type_Descr,
      Table_Index_Type     => Type_Index,
      Table_Low_Bound      => No_Type_Index + 1,
      Table_Initial        => 300,
      Table_Increment      => 100);

   --------------
   -- Grammars --
   --------------

   type String_List_Record;
   type String_List is access String_List_Record;
   type String_List_Record is record
      Str  : Sax.Symbols.Symbol;
      Next : String_List;
   end record;
   --  We will use Ada2005 containers when the compiler is more widely
   --  available

   procedure Free (List : in out String_List);
   --  Free the list and its contents

   type Schema_NFA is new Schema_State_Machines.NFA with record
      Simple_Types : Schema.Simple_Types.Simple_Type_Table;
      References   : Reference_HTable;
      Attributes   : Attributes_Tables.Instance;
      Enumerations : Schema.Simple_Types.Enumeration_Tables.Instance;
      Types        : Types_Tables.Instance;
      Notations    : Schema.Simple_Types.Symbol_Htable.HTable (101);
      --  List of all notations defined in the current XSD

      Ur_Type      : Schema_State_Machines.State;
      Ur_Type_Skip : Schema_State_Machines.State;

      Simple_Nested : Schema_State_Machines.State;
      --  A dummy nested NFA: this is used when xsi:type replaces a complex
      --  type with a simple type, so that we accept no children, but still
      --  accept the <close> tag. We will temporarily override the state
      --  data to match the simple type.

      Metaschema_NFA_Last          : NFA_Snapshot := No_NFA_Snapshot;
      Metaschema_Simple_Types_Last : Schema.Simple_Types.Simple_Type_Index;
      Metaschema_Attributes_Last   : Named_Attribute_List;
      Metaschema_Enumerations_Last : Schema.Simple_Types.Enumeration_Index;
      Metaschema_Types_Last        : Type_Index;
      --  Last state for the metaschema XSD (for Reset)
   end record;

   type Schema_NFA_Matcher
     is new Schema_State_Machines_Matchers.NFA_Matcher with
      record
         Matched_Through_Any     : Boolean := False;
         Matched_Process_Content : Process_Contents_Type;
      end record;

   type XML_Grammar_Record is new Sax.Pointers.Root_Encapsulated with record
      Symbols  : Sax.Utils.Symbol_Table;

      Parsed_Locations : String_List;
      --  List of schema locations that have already been parsed. This is used
      --  in particular to handle cases where a schema imports two others
      --  schemas, that in turn import a common one.

      XSD_Version : XSD_Versions := XSD_1_0;

      NFA          : Schema_NFA_Access;
      --  The state machine representing the grammar
      --  This includes the states for all namespaces
   end record;

   procedure Free (Grammar : in out XML_Grammar_Record);
   --  Free the memory occupied by the grammar

   package XML_Grammars is new Sax.Pointers.Smart_Pointers
     (XML_Grammar_Record);
   type XML_Grammar is new XML_Grammars.Pointer;
   No_Grammar : constant XML_Grammar :=
     XML_Grammar (XML_Grammars.Null_Pointer);

end Schema.Validators;
