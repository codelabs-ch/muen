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

--  This package provides a SAX Reader that parses an XML Schema file, and
--  creates the appropriate data structure

pragma Ada_05;

with Input_Sources;
with Sax.Locators;
with Sax.Readers;           use Sax.Readers;
with Sax.Symbols;
with Sax.Utils;
with Schema.Readers;
with Schema.Simple_Types;
with Schema.Validators;
with Unicode.CES;
with GNAT.Dynamic_Tables;
with GNAT.Dynamic_HTables;

package Schema.Schema_Readers is

   type Schema_Reader is new Schema.Readers.Validating_Reader with private;
   type Schema_Reader_Access is access all Schema_Reader'Class;
   --  An XML reader that parses an XML schema, and store the information in
   --  a grammar

   procedure Parse_Grammar
     (Handler  : access Schema.Readers.Validating_Reader'Class;
      URI      : Sax.Symbols.Symbol;
      Xsd_File : Sax.Symbols.Symbol;
      Do_Create_NFA : Boolean);
   --  Parse (if not done already) the specified [Xsd_File], and associate it
   --  with the given namespace [URI].
   --  [Handler] is used to convert [Xsd_File] to an absolute URI, and find
   --  the grammar.

   overriding procedure Set_Feature
     (Parser : in out Schema_Reader; Name : String; Value : Boolean);
   overriding function Get_Feature
     (Parser : Schema_Reader; Name : String) return Boolean;
   --  Add support for new features

   Feature_Ignore_Unsupported_XSD_Elements : constant String :=
     "http://www.adacore.com/schema/features/ignoreUnsupportedXSDElements";
   --  If this feature is true, then elements from an XSD file that are known
   --  to be unsupported by XML/Ada (for instance <key>, <keyref>,...) will
   --  result in a warning, rather than a fatal error.
   --  As a user, you are free to ignore these. XML/Ada will simply not provide
   --  validation for those elements.

private
   use Schema.Validators;

   type Internal_Type_Index is new Integer;
   No_Internal_Type_Index : constant Internal_Type_Index := -1;

   type Type_Kind is (Type_Empty, Type_Sequence, Type_Choice, Type_Element,
                      Type_Any, Type_Group, Type_Extension, Type_Restriction,
                      Type_All);

   type Type_Details;
   type Type_Details_Access is access all Type_Details;

   type Element_Descr is record
      Name               : Qualified_Name     := No_Qualified_Name;
      Typ                : Qualified_Name     := No_Qualified_Name;
      Local_Type         : Internal_Type_Index := No_Internal_Type_Index;
      Ref                : Qualified_Name     := No_Qualified_Name;
      Form               : Form_Type          := Unqualified;
      Default            : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Fixed              : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Substitution_Group : Qualified_Name     := No_Qualified_Name;
      Final              : Final_Status       := (others => False);
      Block              : Block_Status       := (others => False);
      Is_Abstract        : Boolean            := False;
      Nillable           : Boolean            := False;
      Has_Block          : Boolean            := False;
      Loc                : Sax.Locators.Location := Sax.Locators.No_Location;
      S                  : Schema_State_Machines.State :=
        Schema_State_Machines.No_State;
   end record;
   No_Element_Descr : constant Element_Descr := (others => <>);

   type Group_Descr is record
      Name           : Qualified_Name := No_Qualified_Name;
      Ref            : Qualified_Name := No_Qualified_Name;
      Details        : Type_Details_Access;
      Loc            : Sax.Locators.Location;
   end record;
   No_Group_Descr : constant Group_Descr := (others => <>);

   type Internal_Attribute_Descr is record
      Descr        : Attribute_Descr     := No_Attribute_Descr;
      Typ          : Qualified_Name      := No_Qualified_Name;
      Local_Type   : Internal_Type_Index := No_Internal_Type_Index;
      Ref          : Qualified_Name      := No_Qualified_Name;

      Any          : Internal_Any_Descr  := No_Internal_Any_Descr;
      --  For the handling of <anyAttribute>
   end record;
   No_Internal_Attribute : constant Internal_Attribute_Descr := (others => <>);

   type Attr_Descr_Kind is (Kind_Group, Kind_Attribute, Kind_Unset);
   type Attr_Descr (Kind : Attr_Descr_Kind := Kind_Unset) is record
      Loc : Sax.Locators.Location := Sax.Locators.No_Location;
      case Kind is
         when Kind_Unset     => null;
         when Kind_Group     => Group_Ref : Qualified_Name;
         when Kind_Attribute => Attr      : Internal_Attribute_Descr;
      end case;
   end record;
   type Attr_Array is array (Natural range <>) of Attr_Descr;
   type Attr_Array_Access is access all Attr_Array;

   type AttrGroup_Descr is record
      Name           : Qualified_Name := No_Qualified_Name;
      Ref            : Qualified_Name := No_Qualified_Name;
      Attributes     : Attr_Array_Access;
   end record;
   No_AttrGroup_Descr : constant AttrGroup_Descr := (others => <>);

   type Extension_Descr is record
      Base           : Qualified_Name := No_Qualified_Name;
      Details        : Type_Details_Access;
      Loc            : Sax.Locators.Location;
   end record;
   --  Attributes are set in the corresponding Internal_Type_Descr

   type Restriction_Descr is record
      Base           : Qualified_Name := No_Qualified_Name;
      Details        : Type_Details_Access;
      Loc            : Sax.Locators.Location;
   end record;
   --  Attributes are set in the corresponding Internal_Type_Descr

   type Type_Details (Kind : Type_Kind := Type_Empty) is record
      In_Process : Boolean := False;
      --  Set to true while we are creating the NFA for this details. This is
      --  used to prevent infinite recursion, for instance when an extension
      --  indirectly uses itself as a base.

      Loc    : Sax.Locators.Location;

      Min_Occurs, Max_Occurs : Occurrences;
      Next : Type_Details_Access;
      case Kind is
         when Type_Empty       => null;
         when Type_Sequence    => First_In_Seq    : Type_Details_Access;
         when Type_Choice      => First_In_Choice : Type_Details_Access;
         when Type_Element     => Element         : Element_Descr;
         when Type_Any         => Any             : Internal_Any_Descr;
         when Type_Group       => Group           : Group_Descr;
         when Type_Extension   =>
            Extension       : Extension_Descr;
            Simple_Content  : Boolean;
         when Type_Restriction =>
            Restriction                : Restriction_Descr;
            Simple_Content_Restriction : Boolean;
         when Type_All         => First_In_All    : Type_Details_Access;
      end case;
   end record;

   type Type_Member is record
      Name  : Qualified_Name      := No_Qualified_Name;
      Local : Internal_Type_Index := No_Internal_Type_Index;
   end record;
   No_Type_Member : constant Type_Member :=
     (No_Qualified_Name, No_Internal_Type_Index);
   --  Only one of the two fields is set. These are the possible members of a
   --  union or list.

   type Type_Member_Array is array (Natural range <>) of Type_Member;

   type Simple_Type_Kind is (Simple_Type_None,
                             Simple_Type,
                             Simple_Type_Restriction,
                             Simple_Type_Extension,
                             Simple_Type_Union,
                             Simple_Type_List);
   type Internal_Simple_Type_Descr (Kind : Simple_Type_Kind := Simple_Type)
   is record
      In_Process : Boolean := False;
      --  Used to prevent infinite recursion when for instance a union's member
      --  is derived from this union.

      Loc    : Sax.Locators.Location := Sax.Locators.No_Location;

      case Kind is
         when Simple_Type_None        => null;
         when Simple_Type             => null;
         when Simple_Type_Union       =>
            Union_Items      : Type_Member_Array
              (1 .. Schema.Simple_Types.Max_Types_In_Union) :=
              (others => No_Type_Member);
         when Simple_Type_List        =>
            List_Items      : Type_Member_Array
              (1 .. 1) := (others => No_Type_Member);
         when Simple_Type_Restriction | Simple_Type_Extension =>
            Base   : Type_Member;
            Facets : Schema.Simple_Types.All_Facets :=
              Schema.Simple_Types.No_Facets;
      end case;
   end record;
   No_Internal_Simple_Type_Descr : constant Internal_Simple_Type_Descr :=
     (Kind => Simple_Type_None, others => <>);
   subtype Union_Type_Descr is Internal_Simple_Type_Descr (Simple_Type_Union);
   subtype List_Type_Descr  is Internal_Simple_Type_Descr (Simple_Type_List);

   type Internal_Type_Descr (Is_Simple : Boolean := False) is record
      Properties : Type_Descr;   --  Properties of the type, read in XSD
      In_NFA     : Type_Index;   --  As created in the NFA
      Loc        : Sax.Locators.Location := Sax.Locators.No_Location;

      Simple  : Internal_Simple_Type_Descr :=
        No_Internal_Simple_Type_Descr;
      --  Either the type itself if we are defining a simpleType, or its
      --  simpleContent if we are definiting a complexType (in which case its
      --  kind might be [Simple_Type_None] to indicate it is a complex content

      case Is_Simple is
         when False =>
            Attributes     : Attr_Array_Access;
            --  Stores attributes from <complexType> or the internal
            --  <extension>

            Details        : Type_Details_Access;
         when True =>
            null;
      end case;
   end record;
   --  Temporary structure while parsing a XSD file. Only [Descr] will be
   --  stored in the NFA for reuse while validating (or while parsing other
   --  XSD).

   type Schema_Descr is record
      Target_NS              : Sax.Symbols.Symbol := Sax.Symbols.No_Symbol;
      Block                  : Block_Status       := No_Block;
      Element_Form_Default   : Form_Type          := Unqualified;
      Attribute_Form_Default : Form_Type          := Unqualified;
   end record;

   type Context_Type is (Context_Type_Def,
                         Context_Element,
                         Context_Sequence,
                         Context_Choice,
                         Context_Schema,
                         Context_Restriction,
                         Context_Simple_Restriction, --  simpleType
                         Context_Simple_Extension,   --  simpleType
                         Context_Extension,
                         Context_All,
                         Context_List,
                         Context_Union,
                         Context_Redefine,
                         Context_Group,
                         Context_Attribute_Group,
                         Context_Attribute);

   type Context (Typ : Context_Type := Context_Schema) is record
      case Typ is
         when Context_Type_Def        => Type_Info   : Internal_Type_Index;
         when Context_Element         =>
            Element      : Element_Descr;
            Elem_Details : Type_Details_Access;
         when Context_Sequence        => Seq         : Type_Details_Access;
         when Context_Choice          => Choice      : Type_Details_Access;
         when Context_All             => All_Detail  : Type_Details_Access;
         when Context_Attribute_Group => Attr_Group  : AttrGroup_Descr;
         when Context_Schema          => null;
         when Context_Redefine        => null;
         when Context_Group           => Group       : Group_Descr;
         when Context_Extension       => Extension  : Type_Details_Access;
         when Context_List            => List        : List_Type_Descr;
         when Context_Restriction     => Restriction : Type_Details_Access;
         when Context_Simple_Restriction | Context_Simple_Extension =>
            Simple   : Internal_Simple_Type_Descr;
         when Context_Union           => Union       : Union_Type_Descr;
         when Context_Attribute      => Attribute   : Attr_Descr;
      end case;
   end record;
   type Context_Access is access all Context;
   type Context_Array is array (Natural range <>) of aliased Context;
   type Context_Array_Access is access all Context_Array;

   package Type_Tables is new GNAT.Dynamic_Tables
     (Table_Component_Type => Internal_Type_Descr,
      Table_Index_Type     => Internal_Type_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => 200,
      Table_Increment      => 100);
   package Element_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Element_Descr,
      No_Element => No_Element_Descr,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");
   package Group_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Group_Descr,
      No_Element => No_Group_Descr,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");
   package AttrGroup_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => AttrGroup_Descr,
      No_Element => No_AttrGroup_Descr,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");
   package Attribute_HTables is new GNAT.Dynamic_HTables.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Internal_Attribute_Descr,
      No_Element => No_Internal_Attribute,
      Key        => Qualified_Name,
      Hash       => Hash,
      Equal      => "=");

   type XSD_Data is record
      Types             : Type_Tables.Instance;
      Global_Elements   : Element_HTables.Instance;
      Global_Groups     : Group_HTables.Instance;
      Global_AttrGroups : AttrGroup_HTables.Instance;
      Global_Attributes : Attribute_HTables.Instance;
   end record;
   type XSD_Data_Access is access all XSD_Data;
   --  Data modified while loading XSD, and needed while loading nested (input
   --  or redefine) XSD, until we can create the NFA

   type Schema_Reader is new Schema.Readers.Validating_Reader with record
      Attribute_Form_Default : Schema.Validators.Form_Type :=
        Schema.Validators.Unqualified;
      Element_Form_Default : Schema.Validators.Form_Type :=
        Schema.Validators.Unqualified;
      --  The value of elementFormDefault for the current file

      Feature_Ignore_Unsupported_XSD_Elements : Boolean := False;

      Target_NS            : Sax.Symbols.Symbol;
      Target_Block_Default : Block_Status := No_Block;
      --  The namespace for which we are currently parsing. This might be
      --  different from Get_Target_NS (Created_Grammar) when processing
      --  <import> for instance.

      In_Annotation   : Boolean := False;
      --  Whether we are processing an <annotation> node, in which case we
      --  need to ignore all children

      Contexts        : Context_Array_Access;
      Contexts_Last   : Natural := 0;

      Shared          : XSD_Data_Access;
   end record;

   overriding procedure Start_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax.Readers.Sax_Attribute_List);
   overriding procedure End_Element
     (Handler       : in out Schema_Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol);
   overriding procedure Characters
     (Handler : in out Schema_Reader; Ch : Unicode.CES.Byte_Sequence);
   overriding procedure Parse
     (Parser : in out Schema_Reader;
      Input  : in out Input_Sources.Input_Source'Class);
   --  See inherited documentation

end Schema.Schema_Readers;
