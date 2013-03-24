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

pragma Ada_05;

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Input_Sources.File;        use Input_Sources.File;
with Input_Sources.Strings;     use Input_Sources.Strings;
with Input_Sources;             use Input_Sources;
with Interfaces;                use Interfaces;
with Sax.Attributes;            use Sax.Attributes;
with Sax.Encodings;             use Sax.Encodings;
with Sax.Exceptions;            use Sax.Exceptions;
with Sax.Locators;              use Sax.Locators;
with Sax.Models;                use Sax.Models;
with Sax.Symbols;               use Sax.Symbols;
with Unchecked_Deallocation;
with Unicode.CES;               use Unicode.CES;
with Unicode.CES.Basic_8bit;    use Unicode.CES.Basic_8bit;
with Unicode.Names.Basic_Latin; use Unicode.Names.Basic_Latin;
with Unicode;                   use Unicode;

package body Sax.Readers is

   use Entity_Table, Attributes_Table, Notations_Table;
   use Symbol_Table_Pointers;

   Debug_Lexical  : constant Boolean := False;
   Debug_Input    : constant Boolean := False;
   Debug_Internal : constant Boolean := False;
   --  Set to True if you want to debug this package

   Initial_Buffer_Length : constant := 10000;
   --  Initial length of the internal buffer that stores CDATA, tag names,...

   --------------------
   -- Error messages --
   --------------------
   --  The comment indicates the section of the XML or Namespaces specification
   --  relevant for that error

   Error_Attlist_DefaultDecl       : constant String :=
     "Invalid default declaration for the attribute";  --  3.3.2
   Error_Attlist_Invalid_Enum      : constant String :=
     "Invalid character ',' in ATTLIST enumeration";  --  3.3.1
   Error_Attlist_Type              : constant String :=
     "Invalid type for attribute"; --  WF
   Error_Attribute_External_Entity : constant String :=
     "Attribute values cannot reference external entities";
   Error_Attribute_Is_Name : constant String :=
     "Attribute must contain Names: ";  --  NS 6 and 3.3.1
   Error_Attribute_Is_Ncname : constant String :=
     "Attribute must contain Names with no colon: ";  --  NS 6 and 3.3.1
   Error_Attribute_Is_Nmtoken : constant String :=
     "Attribute must contain Nmtokens: ";  --  2.3 and 3.3.1
   Error_Attribute_Less_Than : constant String :=
     "'<' not authorized in attribute values";  --  2.3
   Error_Attribute_Less_Than_Suggests : constant String :=  --  2.3
     "'<' not authorized in attribute values. Possible end of value at ";
   Error_Attribute_Ref_Unparsed_Entity : constant String :=
     "Attribute must reference an existing unparsed entity: ";
   Error_Cdata_End            : constant String :=
     "CDATA sections must end with ']]>'"; --  2.7
   Error_Cdata_Unterminated   : constant String :=
     "CDATA must be followed immediately by '['";
   Error_Charref_Toplevel     : constant String :=
     "Character references cannot appear at top-level";  --  2.1
   Error_Charref_Invalid_Char : constant String :=
     "Invalid character in character reference: "; -- 4.1
   Error_Comment_End          : constant String :=
     "Comments must end with '-->'";  --  2.5
   Error_Comment_Unterminated : constant String :=
     "Unterminated comment in stream";  --  WF
   Error_Comment_Dash_Dash    : constant String :=
     "'--' cannot appear in comments";  --  2.5
   Error_Conditional_Location : constant String :=  --  3.4
     "INCLUDE and IGNORE sections only allowed in the external DTD subset";
   Error_Conditional_Syntax : constant String :=
     "Conditional sections need '[' after INCLUDE or IGNORE";  --  3.4
   Error_Content_Model_Closing_Paren : constant String :=
     "Closing parenthesis must be followed by '*' in mixed content"; --  3.2.2
   Error_Content_Model_Empty_List : constant String :=
     "Invalid content model: list of choices cannot be empty";
   Error_Content_Model_Expect_Operator : constant String :=
     "Expecting operator in content model";
   Error_Content_Model_Invalid : constant String :=
     "Invalid content model";
   Error_Content_Model_Invalid_Multiplier : constant String :=
     "Invalid location for '+', '?' or '*' operators"; --  3.2.1
   Error_Content_Model_Invalid_Name : constant String :=
     "Invalid name in content model: ";
   Error_Content_Model_Invalid_Seq : constant String :=
     "Missing content particle in sequence"; --  3.2.1
   Error_Content_Model_Invalid_Start : constant String :=
     "Invalid content model, cannot start with #";
   Error_Content_Model_Mixing : constant String :=
     "Cannot mix ',' and '|' in content model";
   Error_Content_Model_Nested_Groups : constant String :=
     "Nested groups and occurrence operators not allowed in mixed content";
   --  3.3.2
   Error_Content_Model_Pcdata : constant String :=
     "#PCDATA can only be used with '|' connectors"; --  3.2.2
   Error_Content_Model_Pcdata_First : constant String :=
     "#PCDATA must be first in list";  --  3.2.2
   Error_Content_Model_Pcdata_Occurrence : constant String :=
     "Occurrence on #PCDATA must be '*'"; --  3.2.2
   Error_Entity_Definition  : constant String :=
     "Invalid definition for ENTITY";
   Error_Entity_Definition_Unterminated  : constant String :=
     "Expecting end of ENTITY definition";
   Error_Entity_Name        : constant String := "Invalid entity name"; --  4.1
   Error_Entity_Not_Standalone    : constant String :=
     "Entity declared in external subset, but document is standalone"; --  4.1
   Error_Entity_Self_Ref  : constant String :=
     "Entity cannot reference itself"; --  4.1
   Error_Entity_Toplevel  : constant String :=
     "Entity references cannot appear at top-level"; --  2.1
   Error_Entity_Undefined : constant String := "Undefined entity"; --  4.1
   Error_Entityref_Unterminated : constant String :=
     "Entity references must end with ';'." & ASCII.LF
       & "Did you want to use &amp;?"; --  4.1
   Error_Entity_Nested : constant String :=
     "Replacement text for entities must be properly nested"; --  3.2.1
   Error_Entity_Self_Contained : constant String :=
     "Entity values must be self-contained";  --  4.5 or 4.3.2
   Error_Expecting_Space : constant String :=
     "Expecting a space"; --  WF or 3.3
   Error_External_Entity_Not_Found : constant String :=
     "External entity not found: ";
   Error_Invalid_Char    : constant String :=
     "Invalid character code:";  --  2.2 or 4.1
   Error_Invalid_Declaration : constant String := "Invalid declaration";
   Error_Invalid_Encoding    : constant String := "Invalid character encoding";
   Error_Invalid_Content_Model : constant String := "Invalid content model";
   Error_Invalid_Language    : constant String :=
     "Invalid language specification";  --  2.12
   Error_Invalid_Name : constant String :=
     "Invalid name: "; --  3.1
   Error_Invalid_Notation_Decl : constant String :=
     "Invalid notation declaration";  --  WF
   Error_Invalid_Space         : constant String :=
     "Value of xml:space must be (default|preserve)";  --  2.10
   Error_Is_Name  : constant String := "Expecting a Name"; --  3.3.1
   Error_Is_Ncname  : constant String :=
     "Expecting a Name with no colon"; --  NS 6 and 3.3.1
   Error_Missing_Operand       : constant String :=
     "Missing operand before this operator";
   Error_Mixed_Contents        : constant String :=
     "Mixed contents cannot be used in a list or a sequence"; --  3.2.1
   Error_Ndata_ParamEntity : constant String := --  4.2
     "NDATA annotation not allowed for parameter entities";
   Error_Ndata_Space : constant String := --  4.2.2
     "Expecting space before NDATA declaration";
   Error_Ndata_String : constant String :=
     "Expecting string after NDATA";
   Error_ParamEntity_In_Attribute : constant String :=
     "Parameter entities cannot occur in attribute values";
   --  WF PE in internal subset
   Error_Notation_Undeclared : constant String :=
     "Notation must be declared: "; --  VC 4.2.2 or 3.3.1
   Error_Prefix_Not_Declared   : constant String :=
     "Prefix must be declared before its use: ";   --  WF
   Error_Public_String : constant String :=
     "Expecting a string after PUBLIC";
   Error_Public_Sysid : constant String :=
     "Expecting SystemID after PUBLIC";
   Error_Public_Sysid_Space : constant String :=
     "Require whitespace between public and system IDs"; --  4.2.2
   Error_Public_Invalid : constant String :=
     "Invalid PubID character: ";
   Error_System_String : constant String :=
     "Expecting a string after SYSTEM";
   Error_System_URI : constant String :=  --  4.2.2
     "SYSTEM identifiers may not contain URI fragments starting with #";
   Error_Unknown_Declaration   : constant String :=
     "Unknown declaration in DTD"; --  WF
   Error_Unexpected_Chars1     : constant String :=
     "Invalid characters '<!-' in stream"; --  WF
   Error_Unexpected_Chars2     : constant String :=
     "Unexpected characters between ']' and '>' in the DTD";  --  2.8
   Error_Unexpected_Chars3     : constant String :=
     "Text may not contain the litteral ']]>'"; --  2.4
   Error_Unterminated_String   : constant String :=
     "Unterminated string";  --  2.3
   Error_Unterminated_String_Suggests : constant String :=
     "Unterminated string, possible end at "; --  2.3

   ------------
   -- Tokens --
   ------------

   type Token_Type is
     (Double_String_Delimiter, --  "
      Single_String_Delimiter, --  '
      Comment,                 --  <!--...--> (Data is the comment)
      Start_Of_Tag,            --  <
      Start_Of_End_Tag,        --  </
      End_Of_Start_Tag,        --  />
      Start_Of_PI,             --  <?
      End_Of_PI,               --  ?>
      End_Of_Tag,              --  >
      Equal,                   --  =  (in tags)
      Colon,                   --  :  (in tags)
      Open_Paren,              --  (  (while parsing content model in ATTLIST)
      Internal_DTD_Start,      --  [  (while in DTD)
      Internal_DTD_End,        --  ]  (while in DTD)
      Include,                 --  <![INCLUDE[
      Ignore,                  --  <![IGNORE[
      Start_Conditional,       --  <![
      End_Conditional,         --  ]]>
      Space,                   --  Any number of spaces (Data is the spaces)
      Text,                    --  any text  (Data is the identifier)
      Name,                    --  same as text, but contains only valid
      --  name characters
      Char_Ref,                --  A character reference. Data is the character
      Cdata_Section,           --  <![CDATA
      Doctype_Start,           --  <!DOCTYPE
      System,                  --  SYSTEM  (while in DTD)
      Public,                  --  PUBLIC  (while in DTD)
      Ndata,                   --  NDATA   (while in DTD)
      Any,                     --  ANY (while in DTD)
      Empty,                   --  EMPTY (while in DTD)
      Notation,                --  NOTATION (while in DTD or ATTLIST)
      Entity_Def,              --  <!ENTITY (while in DTD)
      Element_Def,             --  <!ELEMENT (while in DTD)
      Attlist_Def,             --  <!ATTLIST (while in DTD)
      Id_Type,                 --  ID (while in ATTLIST)       Data is "ID"
      Idref,                   --  IDREF (while in ATTLIST)    Data is "IDREF"
      Idrefs,                  --  IDREFS (while in ATTLIST)   Data is "IDREFS"
      Cdata,                   --  CDATA (while in ATTLIST)    Data is "CDATA"
      Entity,                  --  ENTITY (while in ATTLIST)   Data is "ENTITY"
      Entities,                --  ENTITIES (while in ATTLIST) Data="ENTITIES"
      Nmtoken,                 --  NMTOKEN (while in ATTLIST)  Data="NMTOKEN"
      Nmtokens,                --  NMTOKENS (while in ATTLIST) Data="NMTOKENS"
      Required,                --  REQUIRED (while in ATTLIST) Data="#REQUIRED"
      Implied,                 --  IMPLIED (while in ATTLIST)  Data="#IMPLIED"
      Fixed,                   --  FIXED (while in ATTLIST)    Data="#FIXED"
      End_Of_Input             --  End of input was seen.
     );

   type Token is record
      Typ         : Token_Type;
      First, Last : Natural;  --   Indexes in the buffer
      Location    : Sax.Locators.Location;
      From_Entity : Boolean;  --   Whether the characters come from the
                              --   expansion of an entity.
   end record;

   Null_Token : constant Token := (End_Of_Input, 1, 0, No_Location, False);

   Default_State : constant Parser_State :=
     (Name => "Def",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => True,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => False,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Attr_Value_State : constant Parser_State :=
     (Name => "Att",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => True,
      Expand_Param_Entities => False,
      Expand_Entities => True,
      Report_Character_Ref => True,
      Expand_Character_Ref => False,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Non_Interpreted_String_State : constant Parser_State :=
     (Name => "Str",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => False,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   DTD_State : constant Parser_State :=
     (Name => "DTD",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => True,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => True,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   PI_State : constant Parser_State :=
     (Name => "PI ",
      Ignore_Special => True,
      Detect_End_Of_PI => True,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => False,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Entity_Def_State : constant Parser_State :=
     (Name => "Ent",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => True,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Element_Def_State : constant Parser_State :=
     (Name => "Ele",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => True,
      Handle_Strings => True,
      In_Tag => True,
      Report_Parenthesis => True,
      In_Attlist => False);
   Attribute_Def_State : constant Parser_State :=
     (Name => "AtD",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => True,
      Report_Parenthesis => True,
      In_Attlist => True);
   Attribute_Def_Name_State : constant Parser_State :=
     (Name => "ADN",
      Ignore_Special => False,
      Detect_End_Of_PI => False,
      Greater_Special => True,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => True,
      Report_Parenthesis => True,
      In_Attlist => False);
   Entity_Str_Def_State : constant Parser_State :=
     (Name => "EtS",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => True,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Attlist_Str_Def_State : constant Parser_State :=
     (Name => "AtS",
      Ignore_Special => True,
      Detect_End_Of_PI => False,
      Greater_Special => False,
      Less_Special => False,
      Expand_Param_Entities => False,
      Expand_Entities => True,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => True,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => False,
      Report_Parenthesis => False,
      In_Attlist => False);
   Tag_State : constant Parser_State :=
     (Name => "Tag",
      Ignore_Special => False,
      Greater_Special => True,
      Less_Special => False,
      Detect_End_Of_PI => False,
      Expand_Param_Entities => False,
      Expand_Entities => False,
      Report_Character_Ref => False,
      Expand_Character_Ref => True,
      In_DTD => False,
      Recognize_External => False,
      Handle_Strings => True,
      In_Tag => True,
      Report_Parenthesis => False,
      In_Attlist => False);

   --------------------------
   -- Internal subprograms --
   --------------------------

   procedure Unchecked_Free is new Unchecked_Deallocation
     (Input_Source'Class, Input_Source_Access);
   procedure Unchecked_Free is new Unchecked_Deallocation
     (Hook_Data'Class, Hook_Data_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Sax_Attribute_Array, Sax_Attribute_Array_Access);

   function Debug_Encode (C : Unicode_Char) return Byte_Sequence;
   --  Return an encoded string matching C (matching Sax.Encodins.Encoding)

   procedure Test_Valid_Char
     (Parser : in out Sax_Reader'Class; C : Unicode_Char; Loc : Token);
   --  Raise an error if C is not valid in XML. The error is reported at
   --  location Loc.

   function Is_Pubid_Char (C : Unicode_Char) return Boolean;
   --  Return True if C is a valid character for a Public ID (2.3 specs)

   procedure Test_Valid_Lang
     (Parser : in out Sax_Reader'Class; Lang : Byte_Sequence);
   --  Return True if Lang matches the rules for languages

   procedure Test_Valid_Space
     (Parser : in out Sax_Reader'Class; Space : Byte_Sequence);
   --  Return True if Space matches the rules for the xml:space attribute

   procedure Next_Char
     (Input   : in out Input_Source'Class;
      Parser  : in out Sax_Reader'Class);
   --  Return the next character, and increments the locators.
   --  If there are no more characters in the input streams, Parser is setup
   --  so that End_Of_Stream (Parser) returns True.

   procedure Lookup_Char
     (Input   : in out Input_Source'Class;
      Parser  : in out Sax_Reader'Class;
      Char    : out Unicode_Char);
   --  Lookup one character, but put it back in the input so that the next call
   --  to Next_Char will return it again. This does not change
   --  Parser.Last_Read.

   function End_Of_Stream (Parser : Sax_Reader'Class) return Boolean;
   pragma Inline (End_Of_Stream);
   --  Return True if there are no more characters in the parser.
   --  Note that this indicates that no more character remains to be read, and
   --  is different from checking Eof on the current input (since for instance
   --  a new input is open for an entity).

   procedure Add
     (Parser             : in out Sax_Reader'Class;
      Attr               : in out Sax_Attribute_Array_Access;
      Count              : in out Natural;
      If_Unique          : Boolean;
      Location           : Sax.Locators.Location;
      Local_Name, Prefix : Symbol;
      Value              : Symbol;
      Att_Type           : Attribute_Type := Cdata;
      Default_Decl       : Default_Declaration := Default);
   --  Add the attribute to the list of authorized attributes for this
   --  element, unless it is already there and If_Unique is True.
   --  Last is the last position set in Attr, so the new attribute is added
   --  just after it, and Last modified.

   function Create_Attribute_List
     (Attrs  : Sax_Attribute_List) return Sax.Attributes.Attributes;
   --  Create the list of attributes from Parser.Attributes.
   --  This function has the side effect of resetting
   --  Parser.Attributes_Count to 0, and freeing memory as appropriate

   procedure Put_In_Buffer
     (Parser : in out Sax_Reader'Class; Char : Unicode_Char);
   pragma Inline (Put_In_Buffer);

   procedure Put_In_Buffer
     (Parser : in out Sax_Reader'Class; Str : Byte_Sequence);
   pragma Inline (Put_In_Buffer);
   --  Put the last character read in the internal buffer

   procedure Next_Token
     (Input  : in out Input_Sources.Input_Source'Class;
      Parser : in out Sax_Reader'Class;
      Id     : out Token;
      Coalesce_Space : Boolean := False);
   --  Return the next identifier in the input stream.
   --  Locator is modified accordingly (line and column).
   --  If Coalesce_Space is True, then all the Name or Text tokens preceded or
   --  followed by Space tokens are grouped together and returned as a single
   --  Text token.
   --  Id.Typ is set to End_Of_Input if there are no more token to be read.

   procedure Next_Token_Skip_Spaces
     (Input  : in out Input_Sources.Input_Source'Class;
      Parser : in out Sax_Reader'Class;
      Id     : out Token;
      Must_Have : Boolean := False);
   --  Same as Next_Token, except it skips spaces. If Must_Have is True,
   --  then the first token read must be a space, or an error is raised
   --  Id.Typ is set to End_Of_Input if there are no more token to be read.

   procedure Next_NS_Token_Skip_Spaces
     (Input   : in out Input_Sources.Input_Source'Class;
      Parser  : in out Sax_Reader'Class;
      NS_Id   : out Token;
      Name_Id : out Token);
   --  Skip spaces, if any, then read a "ns:name" or "name" token.

   function Find_Symbol (Parser : Sax_Reader'Class; T : Token) return Symbol;
   function Find_Symbol
     (Parser : Sax_Reader'Class; First, Last : Token) return Symbol;
   --  Return the value of the symbol

   procedure Reset_Buffer
     (Parser : in out Sax_Reader'Class; Id : Token := Null_Token);
   --  Clears the internal buffer in Parser.
   --  If Id is not Null_Token, then only the characters starting from
   --  Id.First are removed

   procedure Set_State
     (Parser : in out Sax_Reader'Class; State : Parser_State);
   --  Set the current state for the parser

   function Get_State (Parser : Sax_Reader'Class) return Parser_State;
   --  Return the current state.

   procedure Close_Namespaces
     (Parser : in out Sax_Reader'Class; List : XML_NS);
   --  Close all namespaces in the list, and report appropriate SAX events

   procedure Check_Valid_Name_Or_NCname
     (Parser : in out Sax_Reader'Class;
      Name   : Token);
   --  Check that Name is a valid Name (if namespaces are not supported) or
   --  a NCname if namespaces are supported.

   procedure Check_Attribute_Value
     (Parser     : in out Sax_Reader'Class;
      Local_Name : Symbol;
      Typ        : Attribute_Type;
      Value      : Symbol;
      Error_Loc  : Token);
   --  Check Validity Constraints for a single attribute. Only call this
   --  subprogram for a validating parser

   procedure Syntactic_Parse
     (Parser : in out Sax_Reader'Class;
      Input  : in out Input_Sources.Input_Source'Class);
   --  Internal syntactical parser.

   procedure Find_NS
     (Parser  : in out Sax_Reader'Class;
      Prefix  : Token;
      NS      : out XML_NS;
      Include_Default_NS : Boolean := True);
   --  Internal version of Find_NS

   function Qname_From_Name
     (Parser : Sax_Reader'Class; Prefix, Local_Name : Token)
      return Byte_Sequence;
   function Qname_From_Name (Prefix, Local_Name : Symbol) return Byte_Sequence;
   --  Create the qualified name from the namespace URI and the local name.

   procedure Add_Namespace
     (Parser       : in out Sax_Reader'Class;
      Node         : Element_Access;
      Prefix       : Symbol;
      URI          : Symbol;
      Report_Event : Boolean := True);
   --  Same as above, with strings

   procedure Add_Namespace_No_Event
     (Parser : in out Sax_Reader'Class;
      Prefix, URI : Symbol);
   --  Create a new default namespace in the parser

   procedure Free (Parser : in out Sax_Reader'Class);
   --  Free the memory allocated for the parser, including the namespaces,
   --  entities,...

   procedure Free (Elem : in out Element_Access);
   --  Free the memory of Elem (and its contents). Note that this doesn't free
   --  the parent of Elem).
   --  On Exit, Elem is set to its parent.

   procedure Parse_Element_Model
     (Input   : in out Input_Sources.Input_Source'Class;
      Parser  : in out Sax_Reader'Class;
      Result  : out Element_Model_Ptr;
      Attlist : Boolean := False;
      Open_Was_Read : Boolean);
   --  Parse the following characters in the stream so as to create an
   --  element or attribute contents model, ie the tree matching an
   --  expression like "(foo|bar)+".
   --  Nmtokens should be true if the names in the model should follow the
   --  Nmtoken rule in XML specifications rather than the Name rule.
   --  If Open_Was_Read, then the opening parenthesis is considered to have
   --  been read already and is automatically inserted into the stack.
   --  Attlist should be set to true if this is the model in <!ELEMENT>

   procedure Fatal_Error
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Loc    : Sax.Locators.Location := No_Location);
   procedure Fatal_Error
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Loc    : Token);
   --  Raises a fatal error.
   --  The error is reported at location Id (or the current parser location
   --  if Id is Null_Token).
   --  The user application should not return from this call. Thus, a
   --  Program_Error is raised if it does return.

   procedure Error
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Loc    : Sax.Locators.Location);
   procedure Error
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Id     : Token);
   --  Same as Fatal_Error, but reports an error instead

   procedure Warning
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Id     : Token := Null_Token);
   --  Same as Fatal_Error, but reports a warning instead

   function Location
     (Parser : Sax_Reader'Class;
      Loc    : Sax.Locators.Location) return Byte_Sequence;
   --  Return the location of the start of Id as a string.

   function Resolve_URI
     (Parser    : Sax_Reader'Class;
      System_Id : Symbol;
      URI       : Symbol) return Symbol;
   --  Return a fully resolved URI, based on the system identifier set for
   --  Machine, and URI.
   --  [System_Id] should be the result of [System_Id (Parser)] at the time the
   --  URI was found.

   function System_Id (Parser : Sax_Reader'Class) return Symbol;
   function Public_Id (Parser : Sax_Reader'Class) return Symbol;
   pragma Inline (System_Id, Public_Id);
   --  Return the current system id that we are parsing

   procedure Close_Inputs
     (Parser : in out Sax_Reader'Class;
      Inputs : in out Entity_Input_Source_Access);
   --  Close the inputs that have been completely read. This should be
   --  called every time one starts an entity, so that calls to
   --  Start_Entity/End_Entity are properly nested, and error messages
   --  point to the right entity.

   procedure Debug_Print (Parser : Sax_Reader'Class; Id : Token);
   --  Print the contents of Id

   -----------------
   -- Find_Symbol --
   -----------------

   function Find_Symbol
     (Parser : Sax_Reader'Class; Str : Byte_Sequence) return Symbol is
   begin
      return Find (Get (Parser.Symbols), Str);
   end Find_Symbol;

   -----------------
   -- Find_Symbol --
   -----------------

   function Find_Symbol (Parser : Sax_Reader'Class; T : Token) return Symbol is
   begin
      return Find (Get (Parser.Symbols), Parser.Buffer (T.First .. T.Last));
   end Find_Symbol;

   -----------------
   -- Find_Symbol --
   -----------------

   function Find_Symbol
     (Parser : Sax_Reader'Class; First, Last : Token) return Symbol is
   begin
      return Find (Get (Parser.Symbols),
                   Parser.Buffer (First.First .. Last.Last));
   end Find_Symbol;

   -------------------
   -- End_Of_Stream --
   -------------------

   function End_Of_Stream (Parser : Sax_Reader'Class) return Boolean is
   begin
      return not Parser.Last_Read_Is_Valid
        and Parser.Last_Read = 16#FFFF#;
   end End_Of_Stream;

   ------------------
   -- Debug_Encode --
   ------------------

   function Debug_Encode (C : Unicode_Char) return Byte_Sequence is
      Buffer : Byte_Sequence (1 .. 20);
      Index  : Natural := Buffer'First - 1;
   begin
      Encoding.Encode (C, Buffer, Index);
      return Buffer (Buffer'First .. Index);
   end Debug_Encode;

   ---------------
   -- System_Id --
   ---------------

   function System_Id (Parser : Sax_Reader'Class) return Symbol is
   begin
      if Parser.Inputs = null then
         return Parser.System_Id;
      else
         return Parser.Inputs.System_Id;
      end if;
   end System_Id;

   ---------------
   -- Public_Id --
   ---------------

   function Public_Id (Parser : Sax_Reader'Class) return Symbol is
   begin
      if Parser.Inputs = null then
         return Parser.Public_Id;
      else
         return Parser.Inputs.Public_Id;
      end if;
   end Public_Id;

   ----------
   -- Free --
   ----------

   procedure Free (Elem : in out Element_Access) is
      procedure Free_Element is new Unchecked_Deallocation
        (Element, Element_Access);
      Tmp : constant Element_Access := Elem.Parent;
   begin
      Free (Elem.Namespaces);
      Free_Element (Elem);
      Elem := Tmp;
   end Free;

   ---------------------------
   -- Create_Attribute_List --
   ---------------------------

   function Create_Attribute_List
     (Attrs  : Sax_Attribute_List) return Sax.Attributes.Attributes
   is
      Attributes : Sax.Attributes.Attributes;
   begin
      for J in 1 .. Attrs.Count loop
         Add_Attribute
           (Attr       => Attributes,
            URI        => Get (Get_URI (Attrs.List (J).NS)).all,
            Local_Name => Get (Attrs.List (J).Local_Name).all,
            Qname      =>
              Qname_From_Name
                (Prefix     => Attrs.List (J).Prefix,
                 Local_Name => Attrs.List (J).Local_Name),
            Att_Type   => Attrs.List (J).Att_Type,
            Content    => Unknown_Model, --  not needed anyway
            Value      => Get (Attrs.List (J).Value).all,
            Default_Decl => Attrs.List (J).Default_Decl);
      end loop;

      return Attributes;

   exception
      when others =>
         Clear (Attributes);
         raise;
   end Create_Attribute_List;

   -----------------
   -- Resolve_URI --
   -----------------

   function Resolve_URI
     (Parser    : Sax_Reader'Class;
      System_Id : Symbol;
      URI       : Symbol) return Symbol
   is
      C : Unicode_Char;
      URI_Str : constant Cst_Byte_Sequence_Access := Get (URI);
      URI_Index : Positive := URI_Str'First;
   begin
      pragma Assert (URI /= No_Symbol);

      if URI = Empty_String then
         return System_Id;
      end if;

      --  ??? Only resolve paths for now
      Encoding.Read (URI_Str.all, URI_Index, C);
      if C = Slash then
         return URI;
      else
         declare
            System_Str : constant Cst_Byte_Sequence_Access := Get (System_Id);
            Index : Natural := System_Str'First;
            Basename_Start : Natural := System_Str'First;
         begin
            while Index <= System_Str'Last loop
               Encoding.Read (System_Str.all, Index, C);
               if C = Slash or else C = Backslash then
                  Basename_Start := Index;
               end if;
            end loop;
            return Find_Symbol
              (Parser,
               System_Str (System_Str'First .. Basename_Start - 1)
               & URI_Str.all);
         end;
      end if;
   end Resolve_URI;

   --------------
   -- Location --
   --------------

   function Location (Parser : Sax_Reader'Class; Loc : Sax.Locators.Location)
      return Byte_Sequence
   is
      Line : constant Byte_Sequence := Natural'Image (Loc.Line);
      Col : constant Byte_Sequence := Natural'Image (Loc.Column);
   begin
      if Parser.Close_Inputs = null then
         if Use_Basename_In_Error_Messages (Parser) then
            return Base_Name (Get (Get_Public_Id (Parser.Locator)).all) & ':'
              & Line (Line'First + 1 .. Line'Last)
              & ':' & Col (Col'First + 1 .. Col'Last);
         else
            return Get (Get_Public_Id (Parser.Locator)).all & ':'
              & Line (Line'First + 1 .. Line'Last)
              & ':' & Col (Col'First + 1 .. Col'Last);
         end if;
      else
         if Use_Basename_In_Error_Messages (Parser) then
            return Base_Name (Get_Public_Id (Parser.Close_Inputs.Input.all))
              & ':' & Line (Line'First + 1 .. Line'Last)
              & ':' & Col (Col'First + 1 .. Col'Last);
         else
            return Get_Public_Id (Parser.Close_Inputs.Input.all) & ':'
              & Line (Line'First + 1 .. Line'Last)
              & ':' & Col (Col'First + 1 .. Col'Last);
         end if;
      end if;
   end Location;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Parser  : in out Sax_Reader'Class;
      Msg     : String;
      Loc     : Sax.Locators.Location := No_Location)
   is
      Id2  : Sax.Locators.Location := Loc;
   begin
      if Id2 = No_Location then
         Id2 := Parser.Current_Location;
      end if;
      Parser.Buffer_Length := 0;

      --  So that when calling Close_Inputs, we do generate an End_Entity
      Parser.State.Ignore_Special := True;

      begin
         --  Must be called before End_Document, as per the SAX standard
         Fatal_Error
           (Parser, Create (Location (Parser, Id2) & ": " & Msg, Id2));
         End_Document (Parser);
      exception
         when E : others =>
            begin
               End_Document (Parser);
            exception
               when others => null;
            end;

            --  Priority is given to the Fatal_Error, whatever
            --  End_Document raises
            Reraise_Occurrence (E);
      end;

      raise Program_Error;
   end Fatal_Error;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Loc    : Token) is
   begin
      Fatal_Error (Parser, Msg, Loc.Location);
   end Fatal_Error;

   -----------
   -- Error --
   -----------

   procedure Error
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Loc    : Sax.Locators.Location)
   is
      Id2  : Sax.Locators.Location := Loc;
   begin
      if Id2 = No_Location then
         Id2 := Parser.Current_Location;
      end if;
      Error (Parser, Create (Location (Parser, Id2) & ": " & Msg, Id2));
   end Error;

   procedure Error
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Id     : Token) is
   begin
      Error (Parser, Msg, Id.Location);
   end Error;

   -----------
   -- Error --
   -----------

   procedure Error (Parser  : in out Sax_Reader'Class; Msg : String) is
   begin
      Error (Parser, Msg, No_Location);
   end Error;

   -------------
   -- Warning --
   -------------

   procedure Warning
     (Parser : in out Sax_Reader'Class;
      Msg    : String;
      Id     : Token := Null_Token)
   is
      Id2 : Sax.Locators.Location := Id.Location;
   begin
      if Id2 = No_Location then
         Id2 := Parser.Current_Location;
      end if;

      Warning (Parser, Create (Location (Parser, Id2) & ": " & Msg, Id2));
   end Warning;

   -----------------
   -- Lookup_Char --
   -----------------

   procedure Lookup_Char
     (Input   : in out Input_Source'Class;
      Parser  : in out Sax_Reader'Class;
      Char    : out Unicode_Char)
   is
   begin
      if Parser.Inputs /= null then
         if Eof (Parser.Inputs.Input.all) then
            if Debug_Input then
               Put_Line ("++Input Lookup_Char: <at end of stream>");
            end if;
            Char := Unicode_Char'Last;
         else
            Input_Sources.Next_Char (Parser.Inputs.Input.all, Char);
         end if;
      else
         if Eof (Input) then
            if Debug_Input then
               Put_Line ("++Input Lookup_Char 2: <at end of stream>");
            end if;
            Char := Unicode_Char'Last;
         else
            Input_Sources.Next_Char (Input, Char);
         end if;
      end if;

      if Debug_Input then
         Put_Line ("++Input Lookup_Char: " & Unicode_Char'Image (Char));
      end if;

      Parser.Lookup_Char := Char;
   end Lookup_Char;

   ---------------
   -- Next_Char --
   ---------------

   procedure Next_Char
     (Input   : in out Input_Source'Class;
      Parser  : in out Sax_Reader'Class)
   is
      procedure Internal (Stream : in out Input_Source'Class);
      pragma Inline (Internal);

      --------------
      -- Internal --
      --------------

      procedure Internal (Stream : in out Input_Source'Class) is
         C : Unicode_Char;
      begin
         if Parser.Lookup_Char /= Unicode_Char'Last then
            C := Parser.Lookup_Char;
            Parser.Lookup_Char := Unicode_Char'Last;
         else
            Next_Char (Stream, C);
         end if;

         --  XML specs say that #xD#xA must be converted to one single #xA.
         --  A single #xD must be converted to one single #xA

         if C = Carriage_Return then
            Parser.Previous_Char_Was_CR := True;

            --  When expanding an internal entity, do not normalize the
            --  character (which has already been normalized when creating the
            --  entity, and therefore comes from a &#13; character ref
            if Parser.Inputs = null
              or else Parser.Inputs.External
            then
               Parser.Last_Read := Line_Feed;
            else
               Parser.Last_Read := Carriage_Return;
            end if;

         elsif C = Line_Feed and then Parser.Previous_Char_Was_CR then
            Parser.Previous_Char_Was_CR := False;

            --  When expanding an internal entity, do not strip the CRLF
            --  sequences: since they have already been stripped when the
            --  entity was created, the sequences that remain were created
            --  through character references &#13;&#10; and should therefore
            --  be kept as is.
            if Parser.Inputs = null
              or else Parser.Inputs.External
            then
               Next_Char (Stream, Parser);
            end if;

         else
            Parser.Last_Read := C;

            if Parser.Feature_Test_Valid_Chars then
               Test_Valid_Char (Parser, Parser.Last_Read, Null_Token);
            end if;
         end if;
      end Internal;

      Input_A : Entity_Input_Source_Access;

   begin
      --  First thing is to take into account location changes due to the
      --  previous character.
      if Parser.Last_Read_Is_Valid then
         if Parser.Last_Read = Line_Feed
           and then not Parser.Previous_Char_Was_CR
         then
            Set_Column_Number (Parser.Locator, 0);
            Increase_Line_Number (Parser.Locator);
         end if;

      elsif Parser.Inputs /= null then
         Set_Location (Parser.Locator, Parser.Inputs.Save_Loc);

         if Parser.Inputs.External then
            Parser.In_External_Entity := False;
            --  ??? Should test whether we are still in an external entity.
            --  However, this is only used for the <?xml?> PI, and at this
            --  point we have already read and discarded it, so it doesn't
            --  really matter.
         end if;

         --  Insert the closed input at the end of the Close_Input list, so
         --  that the next call to Next_Token properly closes the entity.
         --  This can not be done here, otherwise End_Entity is called too
         --  early, and the error messages do not point to the right entity.
         if Parser.Close_Inputs = null then
            Parser.Close_Inputs := Parser.Inputs;
         else
            Input_A := Parser.Close_Inputs;
            while Input_A.Next /= null loop
               Input_A := Input_A.Next;
            end loop;
            Input_A.Next := Parser.Inputs;
         end if;

         Input_A := Parser.Inputs;
         Parser.Inputs := Parser.Inputs.Next;
         Input_A.Next := null;
      end if;

      --  Read the text of the entity if there is any

      if Parser.Inputs /= null then
         if Parser.Inputs.Input = null
           or else Eof (Parser.Inputs.Input.all)
         then
            if Debug_Input then
               Put_Line ("++Input END OF INPUT");
            end if;

            Parser.Last_Read := Unicode_Char'Val (16#00#);
            Parser.Last_Read_Is_Valid := False;
            return;
         end if;

         Parser.Last_Read_Is_Valid := True;
         Increase_Column_Number (Parser.Locator);
         Internal (Parser.Inputs.Input.all);

      --  Else read from the initial input stream
      elsif Eof (Input) then
         if Debug_Input then
            Put_Line
              ("++Input " & To_String (Parser.Locator) & " END_OF_INPUT");
         end if;
         Parser.Last_Read := 16#FFFF#;
         Parser.Last_Read_Is_Valid := False;

      else
         Parser.Last_Read_Is_Valid := True;
         Increase_Column_Number (Parser.Locator);
         Internal (Input);
      end if;

      if Debug_Input and then Parser.Last_Read_Is_Valid then
         Put ("++Input " & To_String (Parser.Locator)
              & "(" & Unicode_Char'Image (Parser.Last_Read) & ")= ");
         if Parser.Last_Read /= Line_Feed then
            Put_Line (Debug_Encode (Parser.Last_Read));
         else
            Put_Line ("Line_Feed");
         end if;
      end if;

   exception
      when Unicode.CES.Invalid_Encoding =>
         Fatal_Error (Parser, Error_Invalid_Encoding);
   end Next_Char;

   -------------------
   -- Put_In_Buffer --
   -------------------

   procedure Put_In_Buffer
     (Parser : in out Sax_Reader'Class; Char : Unicode_Char)
   is
      W : constant Natural := Encoding.Width (Char);
      Tmp : Byte_Sequence_Access;
   begin
      --  Loop until we have enough memory to store the string
      while Parser.Buffer_Length + W > Parser.Buffer'Last loop
         Tmp := Parser.Buffer;
         Parser.Buffer := new Byte_Sequence
           (1 .. Tmp'Length * 2);
         Parser.Buffer (1 .. Tmp'Length) := Tmp.all;
         Free (Tmp);
      end loop;

      Encoding.Encode (Char, Parser.Buffer.all, Parser.Buffer_Length);
   end Put_In_Buffer;

   -------------------
   -- Put_In_Buffer --
   -------------------

   procedure Put_In_Buffer
     (Parser : in out Sax_Reader'Class; Str : Byte_Sequence)
   is
      Tmp : Byte_Sequence_Access;
   begin
      --  Loop until we have enough memory to store the string
      while Parser.Buffer_Length + Str'Length > Parser.Buffer'Last loop
         Tmp := Parser.Buffer;
         Parser.Buffer := new Byte_Sequence (1 .. Tmp'Length * 2);
         Parser.Buffer (1 .. Tmp'Length) := Tmp.all;
         Free (Tmp);
      end loop;

      Parser.Buffer
        (Parser.Buffer_Length + 1 .. Parser.Buffer_Length + Str'Length) := Str;
      Parser.Buffer_Length := Parser.Buffer_Length + Str'Length;
   end Put_In_Buffer;

   ---------------------
   -- Test_Valid_Lang --
   ---------------------

   procedure Test_Valid_Lang
     (Parser : in out Sax_Reader'Class; Lang : Byte_Sequence) is
   begin
      --  XML Errata 41: An empty xml:lang attribute is valid
      if Lang /= "" and then not Is_Valid_Language_Name (Lang) then
         Error (Parser, Error_Invalid_Language);
      end if;
   end Test_Valid_Lang;

   ----------------------
   -- Test_Valid_Space --
   ----------------------

   procedure Test_Valid_Space
     (Parser : in out Sax_Reader'Class; Space : Byte_Sequence) is
   begin
      if Space /= Default_Sequence
        and then Space /= Preserve_Sequence
      then
         Error (Parser, Error_Invalid_Space);
      end if;
   end Test_Valid_Space;

   -------------------
   -- Is_Pubid_Char --
   -------------------

   function Is_Pubid_Char (C : Unicode_Char) return Boolean is
   begin
      return C = Unicode.Names.Basic_Latin.Space
        or else C = Line_Feed
        or else C in Latin_Small_Letter_A .. Latin_Small_Letter_Z
        or else C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z
        or else C in Digit_Zero .. Digit_Nine
        or else C = Hyphen_Minus
        or else C = Apostrophe
        or else C = Opening_Parenthesis
        or else C = Closing_Parenthesis
        or else C = Plus_Sign
        or else C = Comma
        or else C = Dot
        or else C = Slash
        or else C = Unicode.Names.Basic_Latin.Colon
        or else C = Equals_Sign
        or else C = Question_Mark
        or else C = Semicolon
        or else C = Exclamation_Mark
        or else C = Star
        or else C = Number_Sign
        or else C = Commercial_At
        or else C = Dollar_Sign
        or else C = Spacing_Underscore
        or else C = Percent_Sign;
   end Is_Pubid_Char;

   ---------------------
   -- Test_Valid_Char --
   ---------------------

   procedure Test_Valid_Char
     (Parser : in out Sax_Reader'Class; C : Unicode_Char; Loc : Token)
   is
      Id : Sax.Locators.Location;
   begin
      if not (C = 16#9#
              or else C = 16#A#
              or else C = 16#D#
              or else C in Unicode.Names.Basic_Latin.Space .. 16#D7FF#
              or else C in 16#E000# .. 16#FFFD#
              or else C in 16#10000# .. 16#10FFFF#)
      then
         if Loc /= Null_Token then
            Id := Loc.Location;
         else
            Id := No_Location;
            Id.Line := Get_Line_Number (Parser.Locator);
            Id.Column := Get_Column_Number (Parser.Locator);
         end if;
         Fatal_Error (Parser, Error_Invalid_Char & Unicode_Char'Image (C), Id);
      end if;
   end Test_Valid_Char;

   -------------
   -- Find_NS --
   -------------

   procedure Find_NS
     (Parser  : in out Sax_Reader'Class;
      Prefix  : Token;
      NS      : out XML_NS;
      Include_Default_NS : Boolean := True) is
   begin
      Find_NS
        (Parser,
         Find_Symbol (Parser, Parser.Buffer (Prefix.First .. Prefix.Last)),
         NS, Include_Default_NS);
      if NS = No_XML_NS then
         Fatal_Error
           (Parser, Error_Prefix_Not_Declared &
            Parser.Buffer (Prefix.First .. Prefix.Last));
      end if;
   end Find_NS;

   -------------
   -- Find_NS --
   -------------

   procedure Find_NS
     (Parser             : Sax_Reader'Class;
      Prefix             : Sax.Symbols.Symbol;
      NS                 : out XML_NS;
      Include_Default_NS : Boolean := True)
   is
      E : Element_Access := Parser.Current_Node;
   begin
      loop
         if E = null then
            NS := Find_NS_In_List
              (Parser.Default_Namespaces, Prefix, Include_Default_NS, False);
         else
            NS := Find_NS_In_List
              (E.Namespaces, Prefix, Include_Default_NS, True);
         end if;

         exit when NS /= No_XML_NS or else E = null;
         E := E.Parent;
      end loop;
   end Find_NS;

   ----------------------
   -- Find_NS_From_URI --
   ----------------------

   procedure Find_NS_From_URI
     (Parser             : in out Sax_Reader'Class;
      URI                : Symbol;
      NS                 : out XML_NS)
   is
      E      : Element_Access := Parser.Current_Node;
   begin
      loop
         --  Search in the default namespaces
         if E = null then
            NS := Find_NS_From_URI_In_List (Parser.Default_Namespaces, URI);
         else
            NS := Find_NS_From_URI_In_List (E.Namespaces, URI);
         end if;

         exit when NS /= No_XML_NS or else E = null;
         E := E.Parent;
      end loop;
   end Find_NS_From_URI;

   ---------------------
   -- Qname_From_Name --
   ---------------------

   function Qname_From_Name
     (Parser : Sax_Reader'Class; Prefix, Local_Name : Token)
      return Byte_Sequence is
   begin
      if Prefix = Null_Token then
         return Parser.Buffer (Local_Name.First .. Local_Name.Last);
      else
         return Parser.Buffer (Prefix.First .. Prefix.Last)
           & Colon_Sequence
           & Parser.Buffer (Local_Name.First .. Local_Name.Last);
      end if;
   end Qname_From_Name;

   ---------------------
   -- Qname_From_Name --
   ---------------------

   function Qname_From_Name
     (Prefix, Local_Name : Symbol) return Byte_Sequence is
   begin
      if Prefix = No_Symbol or else Prefix = Empty_String then
         return Get (Local_Name).all;
      else
         return Get (Prefix).all & Colon_Sequence & Get (Local_Name).all;
      end if;
   end Qname_From_Name;

   -----------------------
   -- Prefix_From_Qname --
   -----------------------

   function Prefix_From_Qname (Qname : Byte_Sequence) return Byte_Sequence is
      Index    : Natural := Qname'First;
      C        : Unicode_Char;
      Previous : Natural;
   begin
      while Index <= Qname'Last loop
         Previous := Index;
         Encoding.Read (Qname, Index, C);
         if C = Unicode.Names.Basic_Latin.Colon then
            return Qname (Qname'First .. Previous - 1);
         end if;
      end loop;
      return "";
   end Prefix_From_Qname;

   ----------------------------
   -- Add_Namespace_No_Event --
   ----------------------------

   procedure Add_Namespace_No_Event
     (Parser : in out Sax_Reader'Class;
      Prefix, URI : Symbol) is
   begin
      Add_Namespace (Parser, null, Prefix, URI, Report_Event => False);
   end Add_Namespace_No_Event;

   -------------------
   -- Add_Namespace --
   -------------------

   procedure Add_Namespace
     (Parser       : in out Sax_Reader'Class;
      Node         : Element_Access;
      Prefix       : Symbol;
      URI          : Symbol;
      Report_Event : Boolean := True)
   is
      Same_As : XML_NS := No_XML_NS;
   begin
      --  Was there a previous definition of this namespace ?
      Find_NS_From_URI (Parser, URI, Same_As);

      if Node = null then
         Add_NS_To_List (Parser.Default_Namespaces, Same_As, Prefix, URI);
      else
         Add_NS_To_List (Node.Namespaces, Same_As, Prefix, URI);
      end if;

      if Report_Event then
         Start_Prefix_Mapping (Parser, Prefix => Prefix, URI => URI);
      end if;
   end Add_Namespace;

   ------------------
   -- Close_Inputs --
   ------------------

   procedure Close_Inputs
     (Parser : in out Sax_Reader'Class;
      Inputs : in out Entity_Input_Source_Access)
   is
      procedure Free is new Unchecked_Deallocation
        (Entity_Input_Source, Entity_Input_Source_Access);
      Input_A : Entity_Input_Source_Access;
   begin
      while Inputs /= null loop
         --  ??? Could use Input_Sources.Locator.Free
         if Inputs.Input /= null then
            Close (Inputs.Input.all);
            Unchecked_Free (Inputs.Input);
         end if;

         --  not in string context
         if not Parser.State.Ignore_Special then
            End_Entity (Parser, Inputs.Name);
         end if;

         Input_A := Inputs;
         Inputs := Inputs.Next;
         Free (Input_A);
      end loop;
   end Close_Inputs;

   -----------------
   -- Debug_Print --
   -----------------

   procedure Debug_Print (Parser : Sax_Reader'Class; Id : Token) is
   begin
      Put ("++Lex (" & Parser.State.Name & ") at "
           & To_String (Parser.Locator)
           & " (" & Token_Type'Image (Id.Typ) & ") at "
           & To_String (Id.Location));
      if Parser.State.Ignore_Special then
         Put (" (in string)");
      end if;

      if Id.Typ = Space then
         declare
            J : Natural := Id.First;
            C : Unicode_Char;
         begin
            Put (" --");
            while J <= Id.Last loop
               Encoding.Read (Parser.Buffer.all, J, C);
               Put (Unicode_Char'Image (C));
            end loop;
            Put ("--");
         end;

      elsif Id.Last >= Id.First then
         Put (" --" & Parser.Buffer (Id.First .. Id.Last) & "--");
      end if;

      Put_Line
        (" buffer="
         & Parser.Buffer (Parser.Buffer'First .. Parser.Buffer_Length)
         & "--");
   end Debug_Print;

   ----------------
   -- Next_Token --
   ----------------

   procedure Next_Token
     (Input          : in out Input_Source'Class;
      Parser         : in out Sax_Reader'Class;
      Id             : out Token;
      Coalesce_Space : Boolean := False)
   is
      function Looking_At (Str : Byte_Sequence) return Boolean;
      --  True if the next characters read (including the current one) in the
      --  stream match Str. Characters read are stored in the buffer

      procedure Handle_Comments;
      --  <!- has been seen in the buffer, check if this is a comment and
      --  handle it appropriately. The first character after '<!-' has
      --  already been read on calling this subprogram.
      --  Raise an error message when the end of the input stream is seen.

      procedure Handle_Character_Ref;
      --  '&#' has been seen in the buffer, check if this is a character
      --  entity reference and handle it appropriately

      procedure Handle_Less_Than_Sign;
      --  Handle '<', '<!', '<!--', '<![',... sequences

      procedure Handle_Entity_Ref;
      --  '&' has been read (as well as the following character). Skips till
      --  the end of the entity, ie ';'. Saves the name of the entity in the
      --  buffer.
      --  Parser.Last_Read is left to ';', but it is not put in the buffer.

      ----------------
      -- Looking_At --
      ----------------

      function Looking_At (Str : Byte_Sequence) return Boolean is
         C : Unicode_Char;
         Index : Natural := Str'First;
      begin
         while Index <= Str'Last loop
            Encoding.Read (Str, Index, C);

            if C /= Parser.Last_Read
              or else not Parser.Last_Read_Is_Valid
            then
               return False;
            end if;
            Put_In_Buffer (Parser, Parser.Last_Read);
            Next_Char (Input, Parser);
         end loop;
         return True;
      end Looking_At;

      ---------------------
      -- Handle_Comments --
      ---------------------

      procedure Handle_Comments is
      begin
         if not Eof (Input) then
            Next_Char (Input, Parser);
            if Parser.Last_Read = Hyphen_Minus then
               Id.Typ := Comment;  --  In case we reach the eof in the loop
               --  Note that if the file ends exactly with '<!--', we get
               --  an empty text. But at least we will detect the error.
               --  It also fails if we have a non-terminated comment and the
               --  last character in the file is '-'. Doesn't seem worth
               --  paying the cost for some extra tests to handle this.
               loop
                  Next_Char (Input, Parser);
                  if End_Of_Stream (Parser) then
                     Fatal_Error (Parser, Error_Comment_End, Id);
                     Id.Typ := End_Of_Input;
                     return;

                  elsif Parser.Last_Read = Hyphen_Minus then
                     Next_Char (Input, Parser);
                     if End_Of_Stream (Parser) then
                        Fatal_Error (Parser, Error_Comment_Unterminated);
                        Id.Typ := End_Of_Input;
                        return;

                     elsif Parser.Last_Read = Hyphen_Minus then
                        if Parser.Last_Read_Is_Valid then
                           Next_Char (Input, Parser);
                           if Parser.Last_Read = Greater_Than_Sign then
                              exit;
                           end if;
                        end if;
                        Parser.Buffer_Length := Id.First - 1;
                        Id.Location.Line := Get_Line_Number (Parser.Locator);
                        Id.Location.Column :=
                          Get_Column_Number (Parser.Locator) - 2;
                        --  2 = 2 * Hyphen_Minus
                        Fatal_Error (Parser, Error_Comment_Dash_Dash, Id);
                     else
                        Put_In_Buffer (Parser, Hyphen_Minus);
                        Put_In_Buffer (Parser, Parser.Last_Read);
                     end if;
                  else
                     Put_In_Buffer (Parser, Parser.Last_Read);
                  end if;
               end loop;

               if Parser.Feature_Validation
                 and then System_Id (Parser) /= Id.Location.System_Id
               then
                  Error (Parser, Error_Entity_Self_Contained, Id);
               end if;

               Next_Char (Input, Parser);
               return;
            end if;
         end if;
         Fatal_Error (Parser, Error_Unexpected_Chars1);
         Id.Typ := End_Of_Input;
      end Handle_Comments;

      --------------------------
      -- Handle_Character_Ref --
      --------------------------

      procedure Handle_Character_Ref is
         Val : Unicode_Char := 0;
      begin
         if Parser.State.Expand_Character_Ref then
            Id.Typ := Text;
         else
            Id.Typ := Char_Ref;
         end if;

         if Parser.Current_Node = null
           and then Parser.State.Name = Default_State.Name
         then
            Fatal_Error (Parser, Error_Charref_Toplevel, Id);
         end if;

         Next_Char (Input, Parser);
         if Parser.Last_Read = Latin_Small_Letter_X then
            Next_Char (Input, Parser);

            while Parser.Last_Read_Is_Valid
              and then Parser.Last_Read /= Semicolon
            loop
               if Parser.Last_Read in Digit_Zero .. Digit_Nine then
                  Val := Val * 16 + Parser.Last_Read - Digit_Zero;

               elsif Parser.Last_Read in
                 Latin_Capital_Letter_A .. Latin_Capital_Letter_F
               then
                  Val := Val * 16 + Parser.Last_Read - Latin_Capital_Letter_A
                    + 10;

               elsif Parser.Last_Read in
                 Latin_Small_Letter_A .. Latin_Small_Letter_F
               then
                  Val := Val * 16 + Parser.Last_Read - Latin_Small_Letter_A
                    + 10;

               else
                  Id.Location.Line := Get_Line_Number (Parser.Locator);
                  Id.Location.Column := Get_Column_Number (Parser.Locator);
                  Fatal_Error
                    (Parser, Error_Charref_Invalid_Char
                     & Debug_Encode (Parser.Last_Read), Id);
               end if;
               Next_Char (Input, Parser);
            end loop;
         else
            while Parser.Last_Read_Is_Valid
              and then Parser.Last_Read /= Semicolon
            loop
               if Parser.Last_Read in Digit_Zero .. Digit_Nine then
                  Val := Val * 10 + Parser.Last_Read - Digit_Zero;
               else
                  Id.Location.Line := Get_Line_Number (Parser.Locator);
                  Id.Location.Column := Get_Column_Number (Parser.Locator);
                  Fatal_Error
                    (Parser, Error_Charref_Invalid_Char
                     & Debug_Encode (Parser.Last_Read), Id);
               end if;
               Next_Char (Input, Parser);
            end loop;
         end if;

         if Parser.Feature_Test_Valid_Chars then
            Test_Valid_Char (Parser, Val, Id);
         end if;
         Put_In_Buffer (Parser, Val);
         Next_Char (Input, Parser);
         Id.From_Entity := True;
      end Handle_Character_Ref;

      ---------------------------
      -- Handle_Less_Than_Sign --
      ---------------------------

      procedure Handle_Less_Than_Sign is
         Num_Closing_Bracket : Natural;
         Id2 : Token;
      begin
         Id.Typ := Start_Of_Tag;
         Next_Char (Input, Parser);
         case Parser.Last_Read is
            when Slash =>
               Id.Typ := Start_Of_End_Tag;
               Next_Char (Input, Parser);

            when Exclamation_Mark =>
               Next_Char (Input, Parser);
               if Parser.Last_Read = Hyphen_Minus then
                  Handle_Comments;

               elsif Looking_At (Doctype_Sequence) then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Doctype_Start;

               elsif Parser.Last_Read = Opening_Square_Bracket then
                  Next_Char (Input, Parser);

                  if Parser.Last_Read = Latin_Capital_Letter_C then

                     if not Looking_At (Cdata_Sequence) then
                        Fatal_Error (Parser, Error_Invalid_Declaration, Id);
                     end if;

                     if Parser.Last_Read /= Opening_Square_Bracket then
                        Fatal_Error (Parser, Error_Cdata_Unterminated, Id);
                     end if;

                     Reset_Buffer (Parser, Id);
                     Id.Typ := Cdata_Section;
                     Num_Closing_Bracket := 1;
                     loop
                        Next_Char (Input, Parser);

                        if End_Of_Stream (Parser) then
                           Id.Typ := End_Of_Input;
                           Fatal_Error (Parser, Error_Cdata_End, Id);
                           return;

                        elsif Parser.Last_Read_Is_Valid then
                           Put_In_Buffer (Parser, Parser.Last_Read);

                           if Parser.Last_Read = Closing_Square_Bracket then
                              Num_Closing_Bracket := Num_Closing_Bracket + 1;

                           elsif Parser.Last_Read = Greater_Than_Sign
                             and then Num_Closing_Bracket >= 2
                           then
                              Parser.Buffer_Length := Parser.Buffer_Length
                                - 2 * Encoding.Width (Closing_Square_Bracket)
                                - Encoding.Width (Greater_Than_Sign);
                              exit;

                           else
                              Num_Closing_Bracket := 0;
                           end if;
                        end if;
                     end loop;

                     if Id.Location.System_Id /= System_Id (Parser) then
                        Fatal_Error (Parser, Error_Entity_Self_Contained, Id);
                     end if;

                     if not Eof (Input) then
                        Next_Char (Input, Parser);
                     else
                        Parser.Last_Read := 16#FFFF#;
                     end if;

                  else
                     while Is_White_Space (Parser.Last_Read) loop
                        Next_Char (Input, Parser);
                     end loop;

                     if Parser.Last_Read = Latin_Capital_Letter_I
                       or else Parser.Last_Read = Percent_Sign
                     then
                        --  Skip spaces: if we are expending a parameter
                        --  entity, it must start with spaces (4.4.8)
                        Next_Token_Skip_Spaces (Input, Parser, Id2);
                        if Parser.Buffer (Id2.First .. Id2.Last) =
                          Include_Sequence
                        then
                           Reset_Buffer (Parser, Id2);
                           Id.Typ := Include;
                        elsif Parser.Buffer (Id2.First .. Id2.Last) =
                          Ignore_Sequence
                        then
                           Reset_Buffer (Parser, Id2);
                           Id.Typ := Ignore;
                        else
                           Fatal_Error (Parser, Error_Invalid_Declaration, Id);
                        end if;

                        if not Parser.State.In_DTD
                          or else not Parser.In_External_Entity
                        then
                           Fatal_Error
                             (Parser, Error_Conditional_Location, Id);
                        end if;

                        Next_Token_Skip_Spaces (Input, Parser, Id2);
                        if Id2.Typ /= Internal_DTD_Start then
                           Fatal_Error (Parser, Error_Conditional_Syntax, Id2);
                        end if;

                     elsif Parser.State.In_DTD then
                        Id.Typ := Start_Conditional;
                     else
                        Fatal_Error (Parser, Error_Unexpected_Chars1, Id);
                     end if;
                  end if;

               elsif not Parser.State.In_DTD then
                  Fatal_Error (Parser, Error_Unexpected_Chars1, Id);

               elsif Looking_At (Attlist_Sequence)
               --  Since parameter entities are expanded with spaces, we can
               --  have one following ATTLIST immediately
                 and then (Is_White_Space (Parser.Last_Read)
                           or else Parser.Last_Read = Percent_Sign)
               then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Attlist_Def;

               elsif Parser.Last_Read = Latin_Capital_Letter_E then
                  Next_Char (Input, Parser);
                  if Looking_At (Ntity_Sequence) then
                     Reset_Buffer (Parser, Id);
                     Id.Typ := Entity_Def;

                  elsif Looking_At (Element_Sequence) then
                     Reset_Buffer (Parser, Id);
                     Id.Typ := Element_Def;

                  else
                     Fatal_Error (Parser, Error_Unknown_Declaration);
                  end if;

               elsif Looking_At (Notation_Sequence)
               --  Since parameter entities are expanded with spaces, we can
               --  have one following NOTATION immediately
                 and then (Is_White_Space (Parser.Last_Read)
                           or else Parser.Last_Read = Percent_Sign)
               then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Notation;

               else
                  Put_In_Buffer (Parser, Less_Than_Sign);
                  Put_In_Buffer (Parser, Exclamation_Mark);
                  Id.Typ := Text;
               end if;

            when Question_Mark =>
               Id.Typ := Start_Of_PI;
               Next_Char (Input, Parser);

            when others => null;
         end case;
      end Handle_Less_Than_Sign;

      -----------------------
      -- Handle_Entity_Ref --
      -----------------------

      procedure Handle_Entity_Ref is
      begin
         if not Parser.Last_Read_Is_Valid
           or else Is_Valid_Name_Startchar
             (Parser.Last_Read, Parser.XML_Version)
         then
            while Parser.Last_Read_Is_Valid
              and then Parser.Last_Read /= Semicolon
              and then Is_Valid_Name_Char
                (Parser.Last_Read, Parser.XML_Version)
            loop
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            end loop;

            if not Parser.Last_Read_Is_Valid
              or else System_Id (Parser) /= Id.Location.System_Id
            then
               Fatal_Error (Parser, Error_Entity_Self_Contained, Id);
            end if;

            if Parser.Last_Read /= Semicolon then
               Fatal_Error (Parser, Error_Entityref_Unterminated, Id);
            end if;

            Id.From_Entity := True;

         else
            Fatal_Error (Parser, Error_Entity_Name, Id);
         end if;
      end Handle_Entity_Ref;

      type Entity_Ref is (None, Entity, Param_Entity);
      Is_Entity_Ref : Entity_Ref := None;
      Old_System_Id : Symbol;
   begin
      if not Parser.Last_Read_Is_Valid then
         Next_Char (Input, Parser);
      end if;

      Id.First := Parser.Buffer_Length + 1;
      Id.Last := Parser.Buffer_Length;
      Id.Typ := End_Of_Input;
      Id.Location.System_Id := System_Id (Parser);
      Id.Location.Public_Id := Public_Id (Parser);
      Id.Location.Line      := Get_Line_Number (Parser.Locator);
      Id.Location.Column    := Get_Column_Number (Parser.Locator);
      Id.From_Entity := False;

      Close_Inputs (Parser, Parser.Close_Inputs);

      if Eof (Input) and then Parser.Last_Read = 16#FFFF# then
         Id.Location.Column := Id.Location.Column + 1;
         return;
      end if;

      if Is_White_Space (Parser.Last_Read) then
         Id.Typ := Space;
         loop
            Put_In_Buffer (Parser, Parser.Last_Read);
            Next_Char (Input, Parser);
            exit when not Is_White_Space (Parser.Last_Read);
         end loop;

      --  If we are ignoring special characters
      elsif Id.Typ = End_Of_Input
        and then (Parser.Ignore_State_Special
                  or else Parser.State.Ignore_Special)
        and then not Parser.State.Detect_End_Of_PI
      then
         Id.Typ := Text;
         Parser.Ignore_State_Special := True;
         while Parser.Last_Read_Is_Valid loop
            exit when Parser.Last_Read = Ampersand
              and then (Parser.State.Expand_Entities
                        or else Parser.State.Expand_Character_Ref);
            exit when Parser.Last_Read = Percent_Sign
              and then Parser.State.Expand_Param_Entities;
            exit when (Parser.Last_Read = Apostrophe
                       or else Parser.Last_Read = Quotation_Mark)
              and then Parser.State.Handle_Strings
              and then (Parser.Inputs = null
                        or else Parser.Inputs.Handle_Strings);
            exit when Parser.Last_Read = Less_Than_Sign
              and then Parser.State.Less_Special;
            Put_In_Buffer (Parser, Parser.Last_Read);
            Next_Char (Input, Parser);
         end loop;
      end if;

      --  If we haven't found a non-empty token yet
      if Id.Typ = End_Of_Input
        or else Id.First > Parser.Buffer_Length
      then
         case Parser.Last_Read is
            when Less_Than_Sign =>
               if Parser.State.Less_Special then
                  Id.Typ := Start_Of_Tag;
                  Next_Char (Input, Parser);
               elsif Parser.State.Detect_End_Of_PI then
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
                  Next_Char (Input, Parser);
               else
                  Handle_Less_Than_Sign;
               end if;

            when Question_Mark =>
               if Eof (Input) then
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               else
                  Next_Char (Input, Parser);
                  if Parser.Last_Read = Greater_Than_Sign then
                     Id.Typ := End_Of_PI;
                     Next_Char (Input, Parser);
                  elsif Parser.Last_Read = Question_Mark then
                     Put_In_Buffer (Parser, Question_Mark);
                     Id.Typ := Text;
                  else
                     Put_In_Buffer (Parser, Question_Mark);
                     Id.Typ := Text;
                  end if;
               end if;

            when Greater_Than_Sign =>
               if Parser.State.Greater_Special then
                  Id.Typ := End_Of_Tag;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Equals_Sign =>
               if Parser.State.In_Tag then
                  Id.Typ := Equal;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Unicode.Names.Basic_Latin.Colon =>
               if Parser.State.In_Tag then
                  if Parser.Feature_Namespace then
                     Id.Typ := Colon;
                  else
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Id.Typ := Name;
                  end if;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Ampersand =>
               Id.Typ := Text; --  So that eof would at least report an error
               if Eof (Input)
                 and then Parser.State.Expand_Entities
               then
                  Fatal_Error (Parser, Error_Entityref_Unterminated, Id);
               end if;

               Next_Char (Input, Parser);
               if Parser.Last_Read = Number_Sign
                 and then (Parser.State.Expand_Character_Ref
                           or Parser.State.Report_Character_Ref)
               then
                  Handle_Character_Ref;
                  if System_Id (Parser) /= Id.Location.System_Id then
                     Fatal_Error (Parser, Error_Entity_Self_Contained, Id);
                  end if;

               elsif Parser.Last_Read /= Number_Sign
                 and then Parser.State.Expand_Entities
               then
                  Handle_Entity_Ref;
                  Is_Entity_Ref := Entity;

               elsif Parser.Last_Read /= Number_Sign
                 and then Parser.State.Ignore_Special   --  string context
                 and then not Parser.State.Detect_End_Of_PI  --  not in PI
               then
                  --  Inside a string (entity value), we still need to check
                  --  that the '&' marks the beginning of an entity reference.
                  Put_In_Buffer (Parser, Ampersand);
                  Handle_Entity_Ref;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);

               else
                  Put_In_Buffer (Parser, Ampersand);
               end if;

            when Percent_Sign =>
               Put_In_Buffer (Parser, Parser.Last_Read);
               Id.Typ := Text;

               Next_Char (Input, Parser);
               if Parser.State.Expand_Param_Entities then
                  while Parser.Last_Read /= Semicolon
                    and then Is_Valid_Name_Char
                      (Parser.Last_Read, Parser.XML_Version)
                  loop
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Next_Char (Input, Parser);
                  end loop;

                  if Parser.Last_Read /= Semicolon then
                     Fatal_Error (Parser, Error_Entityref_Unterminated);
                  end if;
                  Is_Entity_Ref := Param_Entity;
               end if;

            when Quotation_Mark =>
               if Parser.State.Handle_Strings then
                  Id.Typ := Double_String_Delimiter;
                  Next_Char (Input, Parser);
               else
                  Id.Typ := Text;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);
               end if;

            when Apostrophe =>
               if Parser.State.Handle_Strings then
                  Id.Typ := Single_String_Delimiter;
                  Next_Char (Input, Parser);
               else
                  Id.Typ := Text;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);
               end if;

            when Opening_Square_Bracket =>
               if Parser.State.In_DTD then
                  Id.Typ := Internal_DTD_Start;
               else
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Id.Typ := Text;
               end if;
               Next_Char (Input, Parser);

            when Closing_Square_Bracket =>
               if Parser.State.In_DTD
                 and then not Parser.In_External_Entity
               then
                  Id.Typ := Internal_DTD_End;
                  loop
                     Next_Char (Input, Parser);
                     exit when Parser.Last_Read = Greater_Than_Sign;

                     if Parser.Last_Read_Is_Valid
                       and then not Is_White_Space (Parser.Last_Read)
                     then
                        Fatal_Error (Parser, Error_Unexpected_Chars2, Id);
                     end if;
                  end loop;
                  Next_Char (Input, Parser);

               --  In string context ?
               elsif Parser.State.Ignore_Special then
                  Id.Typ := Text;
                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);

               else
                  declare
                     Num_Bracket : Natural := 1;
                  begin
                     Id.Typ := Text;

                     loop
                        Put_In_Buffer (Parser, Parser.Last_Read);
                        Next_Char (Input, Parser);

                        if Parser.Last_Read = Closing_Square_Bracket then
                           Num_Bracket := Num_Bracket + 1;

                        elsif Num_Bracket >= 2
                          and Parser.Last_Read = Greater_Than_Sign
                        then
                           if Parser.State.In_DTD
                             and then Parser.In_External_Entity
                           then
                              Id.Typ := End_Conditional;
                              Reset_Buffer (Parser, Id);
                              Next_Char (Input, Parser);
                              exit;
                           else
                              Id.Location.Column :=
                                Id.Location.Column + Num_Bracket - 2;
                              Fatal_Error
                                (Parser, Error_Unexpected_Chars3, Id);
                           end if;
                        else
                           exit;
                        end if;
                     end loop;
                  end;
               end if;

            when Slash =>
               Id.Typ := Text;
               Next_Char (Input, Parser);
               if Parser.State.Greater_Special
                 and then Parser.Last_Read = Greater_Than_Sign
               then
                  Id.Typ := End_Of_Start_Tag;
                  Next_Char (Input, Parser);
               else
                  Put_In_Buffer (Parser, Slash);
               end if;

            when others =>
               if Parser.State.Recognize_External then

                  if Parser.Last_Read = Latin_Capital_Letter_A then
                     if Looking_At (Any_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Any;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_E then
                     if Looking_At (Empty_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Empty;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_N then
                     if Looking_At (Ndata_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Ndata;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_P then
                     if Looking_At (Public_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := Public;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_S then
                     if Looking_At (System_Sequence) then
                        Reset_Buffer (Parser, Id);
                        Id.Typ := System;
                     else
                        Id.Typ := Name;
                     end if;
                  end if;
               end if;

               if Parser.State.Report_Parenthesis
                 and then Parser.Last_Read = Opening_Parenthesis
               then
                  Reset_Buffer (Parser, Id);
                  Id.Typ := Open_Paren;
                  Next_Char (Input, Parser);
                  return;
               end if;

               if Parser.State.In_Attlist then
                  if Parser.Last_Read = Latin_Capital_Letter_C then
                     if Looking_At (Cdata_Sequence) then
                        Id.Typ := Cdata;
                     else
                        Id.Typ := Name;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_E
                    and then Looking_At (Entit_Sequence)
                  then
                     if Looking_At (Ies_Sequence) then
                        Id.Typ := Entities;
                     elsif Parser.Last_Read = Latin_Capital_Letter_Y then
                        Id.Typ := Entity;
                        Put_In_Buffer (Parser, Parser.Last_Read);
                        Next_Char (Input, Parser);
                     else
                        Fatal_Error (Parser, Error_Attlist_Type);
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_I
                    and then Looking_At (Id_Sequence)
                  then
                     if Looking_At (Ref_Sequence) then
                        if Parser.Last_Read = Latin_Capital_Letter_S then
                           Id.Typ := Idrefs;
                           Put_In_Buffer (Parser, Parser.Last_Read);
                           Next_Char (Input, Parser);
                        else
                           Id.Typ := Idref;
                        end if;
                     else
                        Id.Typ := Id_Type;
                     end if;

                  elsif Parser.Last_Read = Latin_Capital_Letter_N then
                     Next_Char (Input, Parser);
                     if Looking_At (Mtoken_Sequence) then
                        if Parser.Last_Read = Latin_Capital_Letter_S then
                           Id.Typ := Nmtokens;
                           Next_Char (Input, Parser);
                        else
                           Id.Typ := Nmtoken;
                        end if;
                     elsif Looking_At (Otation_Sequence) then
                        Id.Typ := Notation;
                     else
                        Fatal_Error (Parser, Error_Attlist_Type);
                     end if;

                  elsif Parser.Last_Read = Number_Sign then
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Next_Char (Input, Parser);
                     if Looking_At (Implied_Sequence) then
                        Id.Typ := Implied;
                     elsif Looking_At (Required_Sequence) then
                        Id.Typ := Required;
                     elsif Looking_At (Fixed_Sequence) then
                        Id.Typ := Fixed;
                     else
                        Fatal_Error (Parser, Error_Attlist_DefaultDecl);
                     end if;
                  end if;
               end if;
         end case;

         --  try to coalesce as many things as possible into a single
         --  text event
         if Id.Typ = End_Of_Input then
            if Is_Valid_Name_Startchar (Parser.Last_Read, Parser.XML_Version)
              or else Parser.Last_Read = Spacing_Underscore
            then
               Id.Typ := Name;
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            else
               Id.Typ := Text;
            end if;
         end if;

         if Id.Typ = Name and then not Coalesce_Space then
            while
              (Parser.Last_Read /= Unicode.Names.Basic_Latin.Colon
               or else not Parser.Feature_Namespace)
              and then
                Is_Valid_NCname_Char (Parser.Last_Read, Parser.XML_Version)
            loop
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            end loop;

         elsif Is_Entity_Ref = None
           and then (Id.Typ = Text
                     or else (Coalesce_Space and then Id.Typ = Name))
         then
            if not Parser.Last_Read_Is_Valid then
               Next_Char (Input, Parser);

            else
               loop
                  if Is_White_Space (Parser.Last_Read) then
                     exit when not Coalesce_Space;

                  else
                     case Parser.Last_Read is
                     when Greater_Than_Sign =>
                        exit when Parser.State.Greater_Special;

                     when Less_Than_Sign             --  Start of new tag
                        | Ampersand                  --  for Entities
                        | Closing_Square_Bracket     --  for CData   ]]>
                        | Quotation_Mark             --  for attributes a="..."
                        | Apostrophe                 --  for attributes a='...'
                        | Equals_Sign =>             --  for attributes
                        exit;

                     when Slash =>                   --  For <NODE/>
                        declare
                           C : Unicode_Char;
                        begin
                           Lookup_Char (Input, Parser, C);
                           exit when C = Greater_Than_Sign
                             or else Id.Typ = Name;
                        end;

                     when Percent_Sign =>
                        exit when Parser.State.Expand_Param_Entities;

                     when Question_Mark =>
                        exit when Parser.State.Detect_End_Of_PI;

                     when others =>
                        null;
                     end case;
                  end if;

                  Put_In_Buffer (Parser, Parser.Last_Read);
                  Next_Char (Input, Parser);
                  exit when not Parser.Last_Read_Is_Valid;
               end loop;
            end if;
         end if;

         Parser.Ignore_State_Special := False;
      end if;

      if Coalesce_Space and then Id.Typ = Space then
         --  First character is necessarily not a space, so we'll change the
         --  type of the token to text
         declare
            Save_Length : constant Natural := Parser.Buffer_Length;
         begin
            while Parser.Last_Read_Is_Valid
              and then (not Parser.State.Greater_Special
                   or else Parser.Last_Read /= Greater_Than_Sign)
              and then Parser.Last_Read /= Less_Than_Sign
              and then Parser.Last_Read /= Ampersand
              and then (not Parser.State.Expand_Param_Entities
                        or else Parser.Last_Read /= Percent_Sign)
              and then Parser.Last_Read /= Equals_Sign
              and then Parser.Last_Read /= Quotation_Mark
              and then Parser.Last_Read /= Closing_Square_Bracket
              and then Parser.Last_Read /= Apostrophe
              and then Parser.Last_Read /= Slash
              and then (Parser.Last_Read /= Question_Mark
                        or else not Parser.State.Detect_End_Of_PI)
            loop
               Put_In_Buffer (Parser, Parser.Last_Read);
               Next_Char (Input, Parser);
            end loop;

            --  Special case for ']': since the parser needs to detect whether
            --  this is the beginning of ']]>', this will be done in the next
            --  call to Next_Token. However, we shouldn't report the spaces as
            --  Ignorable_Whitespace in this case.

            if Parser.Last_Read = Closing_Square_Bracket
              or else Parser.Buffer_Length /= Save_Length
            then
               Id.Typ := Text;
            end if;
         end;
      end if;

      Id.Last := Parser.Buffer_Length;

      if Debug_Lexical then
         Debug_Print (Parser, Id);
      end if;

      --  Internal entities should be processes inline

      if Is_Entity_Ref /= None then
         declare
            N : constant Symbol := Find_Symbol (Parser, Id);
            V : constant Entity_Entry_Access := Get (Parser.Entities, N);
         begin
            Reset_Buffer (Parser, Id);
            if N = Parser.Lt_Sequence then
               Put_In_Buffer (Parser, Less_Than_Sign);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Parser.Gt_Sequence then
               Put_In_Buffer (Parser, Greater_Than_Sign);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Parser.Amp_Sequence then
               Put_In_Buffer (Parser, Ampersand);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Parser.Apos_Sequence then
               Put_In_Buffer (Parser, Apostrophe);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif N = Parser.Quot_Sequence then
               Put_In_Buffer (Parser, Quotation_Mark);
               Id.Typ := Text;
               Id.Last := Parser.Buffer_Length;
               Next_Char (Input, Parser);

            elsif V = null then
               declare
                  Sym : constant Cst_Byte_Sequence_Access := Get (N);
               begin
                  Skipped_Entity (Parser, N);
                  if N = Parser.Symbol_Ampersand
                    or else N = Parser.Symbol_Percent
                  then
                     Fatal_Error (Parser, Error_Entity_Name & " '"
                                  & Sym.all & "'", Id);

                  elsif Sym (Sym'First) = '%' then
                     Error (Parser, Error_Entity_Undefined & " '"
                            & Sym.all & "'", Id);

                  elsif not Parser.In_External_Entity then
                     --  WF Entity Declared
                     Fatal_Error
                       (Parser, Error_Entity_Undefined & " '"
                        & Sym.all & ''', Id);

                  else
                     --  if Parser.Feature_Validation then
                     --  VC Entity Declared
                     Error
                       (Parser, Error_Entity_Undefined & " '"
                        & Sym.all & ''', Id);
                  end if;
               end;

               Id.Typ := Text;
               Id.Last := Id.First - 1;
               Next_Char (Input, Parser);

            else
               if Parser.Standalone_Document
                 and then V.External_Declaration
               then
                  --  4.1 WF Entity Declared
                  Fatal_Error
                    (Parser, Error_Entity_Not_Standalone, Id);
               end if;

               if Is_Entity_Ref = Entity
                 and then Parser.Current_Node = null
                 and then not Parser.State.In_DTD
               then
                  Fatal_Error (Parser, Error_Entity_Toplevel, Id);

               --  Else if we are in the internal subset of the DTD, and in
               --  a context other than a declaration
               elsif Is_Entity_Ref = Param_Entity
                 and then not Parser.In_External_Entity
                 and then Parser.State.Name /= DTD_State.Name
               then
                  Fatal_Error (Parser, Error_ParamEntity_In_Attribute, Id);
               end if;

               Close_Inputs (Parser, Parser.Close_Inputs);

               --  not in string context
               if not Parser.State.Ignore_Special then
                  Start_Entity (Parser, N);
               end if;

               if V.Already_Read then
                  Fatal_Error (Parser, Error_Entity_Self_Ref, Id);
               end if;

               V.Already_Read := True;

               Parser.Element_Id := Parser.Element_Id + 1;

               if Debug_Internal then
                  Put_Line ("Expanding entity " & Get (N).all & " External="
                            & V.External'Img
                            & " Value=" & Get (V.Value).all);
               end if;

               Old_System_Id := Get_System_Id (Parser.Locator);

               Parser.Inputs := new Entity_Input_Source'
                 (External       => V.External,
                  Name           => N,
                  Input          => null,
                  Save_Loc       => Get_Location (Parser.Locator),
                  System_Id      => Find_Symbol
                    (Parser, Get (System_Id (Parser)).all & '#' & Get (N).all),
                  Public_Id      => Find_Symbol
                    (Parser, Get (Public_Id (Parser)).all & '#' & Get (N).all),
                  Handle_Strings => not Parser.State.Ignore_Special,
                  Next           => Parser.Inputs);

               if V.External then
                  if Parser.State.Name = Attlist_Str_Def_State.Name
                    or else Parser.State.Name = Attr_Value_State.Name
                  then
                     Fatal_Error (Parser, Error_Attribute_External_Entity, Id);
                  end if;

                  declare
                     URI : constant Symbol :=
                       Resolve_URI (Parser, Old_System_Id, V.Value);
                  begin
                     Parser.Inputs.Input := Resolve_Entity
                       (Parser,
                        Public_Id => Get (V.Public).all,
                        System_Id => Get (URI).all);

                     --  If either there is no entity resolver or if the
                     --  standard algorithm should be used

                     if Parser.Inputs.Input = null then
                        Parser.Inputs.Input := new File_Input;
                        Open (Get (URI).all,
                              File_Input (Parser.Inputs.Input.all));
                        Set_Public_Id
                          (Parser.Inputs.Input.all, Get (V.Value).all);
                        Set_System_Id (Parser.Inputs.Input.all, Get (URI).all);
                     end if;

                     Parser.Inputs.Name := Find_Symbol
                       (Parser, Get_System_Id (Parser.Inputs.Input.all));

                     Set_System_Id (Parser.Locator, URI);
                     Set_Public_Id (Parser.Locator, V.Value);

                  exception
                     when Name_Error =>
                        Error
                          (Parser, Error_External_Entity_Not_Found
                           & Get (URI).all, Id);
                        Unchecked_Free (Parser.Inputs.Input);
                     when E : Mismatching_BOM =>
                        Error (Parser, Exception_Message (E));
                        Unchecked_Free (Parser.Inputs.Input);
                  end;

                  Parser.In_External_Entity := True;
               else
                  Parser.Inputs.Input := new String_Input;

                  --  4.4.8: Expansion of parameter entities must include
                  --  a leading and trailing space, unless we are within an
                  --  entity value.
                  if Is_Entity_Ref = Param_Entity
                    and then not Parser.State.Ignore_Special
                  then
                     Open (' ' & Get (V.Value).all & ' ',
                           Encoding,
                           String_Input (Parser.Inputs.Input.all));
                  else
                     Open (Get (V.Value).all, Encoding,
                           String_Input (Parser.Inputs.Input.all));
                  end if;
                  Set_Public_Id
                    (Parser.Locator,
                     Find_Symbol (Parser, "entity " & Get (N).all));
                  Set_Public_Id
                    (Parser.Inputs.Input.all,
                     Get (Get_Public_Id (Parser.Locator)).all);
               end if;

               if Parser.Inputs.Input = null then
                  Skipped_Entity (Parser, V.Name);
                  Next_Char (Input, Parser);
                  Next_Token (Input, Parser, Id);

               else
                  Set_Line_Number (Parser.Locator, 1);
                  Set_Column_Number
                    (Parser.Locator,
                     Prolog_Size (Parser.Inputs.Input.all));

                  Next_Char (Input, Parser);
                  Next_Token (Input, Parser, Id);

                  V.Already_Read := False;
               end if;
            end if;
         end;
      end if;
   end Next_Token;

   ----------------------------
   -- Next_Token_Skip_Spaces --
   ----------------------------

   procedure Next_Token_Skip_Spaces
     (Input  : in out Input_Sources.Input_Source'Class;
      Parser : in out Sax_Reader'Class;
      Id     : out Token;
      Must_Have : Boolean := False) is
   begin
      Next_Token (Input, Parser, Id);
      if Must_Have and then Id.Typ /= Space then
         Fatal_Error (Parser, Error_Expecting_Space, Id);
      end if;
      while Id.Typ = Space loop
         Reset_Buffer (Parser, Id);
         Next_Token (Input, Parser, Id);
      end loop;
   end Next_Token_Skip_Spaces;

   -------------------------------
   -- Next_NS_Token_Skip_Spaces --
   -------------------------------

   procedure Next_NS_Token_Skip_Spaces
     (Input   : in out Input_Sources.Input_Source'Class;
      Parser  : in out Sax_Reader'Class;
      NS_Id   : out Token;
      Name_Id : out Token)
   is
      Id : Token;
      Saved_In_Tag : constant Boolean := Parser.State.In_Tag;
   begin
      NS_Id := Null_Token;
      Next_Token (Input, Parser, Id);
      while Id.Typ = Space loop
         Reset_Buffer (Parser, Id);
         Next_Token (Input, Parser, Id);
      end loop;
      Name_Id := Id;

      if Name_Id.Typ = Colon then
         --  An empty namespace, used in the XML testsuite ?
         NS_Id := Null_Token;
         Reset_Buffer (Parser, Id);
         Next_Token (Input, Parser, Name_Id);

      elsif Name_Id.Typ = Name then
         if Parser.Last_Read_Is_Valid
           and then Parser.Last_Read = Unicode.Names.Basic_Latin.Colon
           and then Parser.Feature_Namespace
         then
            Parser.State.In_Tag := True;  --  Get COLON on its own
            Next_Token (Input, Parser, Id);
            Parser.State.In_Tag := Saved_In_Tag;

            NS_Id := Name_Id;
            Reset_Buffer (Parser, Id);
            Next_Token (Input, Parser, Name_Id);
         end if;
      end if;
   end Next_NS_Token_Skip_Spaces;

   ------------------
   -- Reset_Buffer --
   ------------------

   procedure Reset_Buffer
     (Parser : in out Sax_Reader'Class; Id : Token := Null_Token) is
   begin
      Parser.Buffer_Length := Id.First - 1;
   end Reset_Buffer;

   ---------------
   -- Set_State --
   ---------------

   procedure Set_State
     (Parser : in out Sax_Reader'Class; State : Parser_State) is
   begin
      Parser.State := State;
   end Set_State;

   ---------------
   -- Get_State --
   ---------------

   function Get_State (Parser : Sax_Reader'Class) return Parser_State is
   begin
      return Parser.State;
   end Get_State;

   -------------------------
   -- Parse_Element_Model --
   -------------------------

   procedure Parse_Element_Model
     (Input   : in out Input_Source'Class;
      Parser  : in out Sax_Reader'Class;
      Result  : out Element_Model_Ptr;
      Attlist : Boolean := False;
      Open_Was_Read : Boolean)
   is
      --  ??? Would be nice to get rid of this hard-coded limitation in stacks
      Stack_Size : constant Natural := 1024;
      Operand_Stack : Element_Model_Array (1 .. Stack_Size);
      Operand_Index : Natural := Operand_Stack'First;
      Operator_Stack : array (1 .. Stack_Size) of Unicode_Char;
      Operator_Index : Natural := Operator_Stack'First;
      Expect_Operator : Boolean := not Open_Was_Read;

      procedure Parse_Element_Model_From_Entity (Name : Symbol);
      --  Parse the element model defined in the entity Name, and leave the
      --  contents on the stacks.

      procedure Parse
        (Input         : in out Input_Source'Class;
         Result        : out Element_Model_Ptr;
         Open_Was_Read : Boolean;
         Is_Recursive_Call : Boolean);
      --  Parse the content model read in Input
      --  Is_Recursive_Call should be true when called from itself or from
      --  Parse_Element_Model_From_Entity.

      -------------------------------------
      -- Parse_Element_Model_From_Entity --
      -------------------------------------

      procedure Parse_Element_Model_From_Entity (Name : Symbol) is
         Loc : Sax.Locators.Location;
         Last : constant Unicode_Char := Parser.Last_Read;
         Input_S : String_Input;
         Val : constant Entity_Entry_Access := Get (Parser.Entities, Name);
         M : Element_Model_Ptr;
      begin
         if Val = null then
            Fatal_Error
              (Parser,
               Error_Entity_Undefined & ' ' & Get (Name).all);

         elsif Val.Value = Empty_String then
            return;

         else
            Loc := Get_Location (Parser.Locator);
            Set_Line_Number (Parser.Locator, 1);
            Set_Column_Number (Parser.Locator, 1);
            Set_Public_Id
              (Parser.Locator,
               Find_Symbol (Parser, "entity " & Get (Name).all));

            Open (Get (Val.Value).all, Encoding, Input_S);
            Next_Char (Input_S, Parser);
            Parse (Input_S, M, False, True);
            --  Parse_Element_Model (Input_S, Parser, M, Attlist, False);
            Close (Input_S);

            Set_Location (Parser.Locator, Loc);
            Parser.Last_Read := Last;
         end if;
      end Parse_Element_Model_From_Entity;

      -----------
      -- Parse --
      -----------

      procedure Parse
        (Input : in out Input_Source'Class;
         Result : out Element_Model_Ptr;
         Open_Was_Read : Boolean;
         Is_Recursive_Call : Boolean)
      is
         Num_Items : Positive;
         Current_Item, Current_Operand : Natural;
         Start_Sub : Natural := Parser.Buffer_Length + 1;
         M : Element_Model_Ptr;
         Found : Boolean;
         Start_Id : constant Symbol := System_Id (Parser);
         Start_Token : Token;
         Test_Multiplier : Boolean;
         Can_Be_Mixed : Boolean;
         Num_Parenthesis : Integer := 0;
         Already_Displayed_Self_Contained_Error : Boolean := False;

      begin
         Start_Token                 := Null_Token;
         Start_Token.Location.Line   := Get_Line_Number (Parser.Locator);
         Start_Token.Location.Column := Get_Column_Number (Parser.Locator);

         if Open_Was_Read then
            Start_Token.Location.Column := Start_Token.Location.Column - 1;
         end if;

         while Is_White_Space (Parser.Last_Read) loop
            Next_Char (Input, Parser);
         end loop;

         loop
            if End_Of_Stream (Parser) then
               if not Is_Recursive_Call then
                  for J in Operand_Stack'First .. Operand_Index - 1 loop
                     Free (Operand_Stack (J));
                  end loop;

               elsif Num_Parenthesis /= 0 then
                  Fatal_Error (Parser, Error_Entity_Nested, Start_Token);

               elsif Parser.Buffer_Length >= Start_Sub then
                  Operand_Stack (Operand_Index) :=
                    new Element_Model (Element_Ref);
                  Operand_Stack (Operand_Index).Name := Find_Symbol
                    (Parser,
                     Parser.Buffer (Start_Sub .. Parser.Buffer_Length));
                  Operand_Index := Operand_Index + 1;
                  Parser.Buffer_Length := Start_Sub - 1;
               end if;

               exit;
            end if;

            if Parser.Feature_Validation
              and then (not Parser.Last_Read_Is_Valid
                        or else System_Id (Parser) /= Start_Id)
              and then not Already_Displayed_Self_Contained_Error
            then
               Already_Displayed_Self_Contained_Error := True;
               Error (Parser, Error_Entity_Self_Contained, Start_Token);
            end if;

            Test_Multiplier := False;

            --  Process the operator
            case Parser.Last_Read is
               when Opening_Parenthesis =>
                  Operator_Stack (Operator_Index) := Parser.Last_Read;
                  Operator_Index := Operator_Index + 1;
                  Expect_Operator := False;
                  Next_Char (Input, Parser);
                  Num_Parenthesis := Num_Parenthesis + 1;

               when Closing_Parenthesis =>
                  Num_Parenthesis := Num_Parenthesis - 1;
                  Num_Items := 1;
                  Current_Item := Operator_Index - 1;
                  Current_Operand := Operand_Index - 1;
                  Can_Be_Mixed :=  Current_Operand >= Operand_Stack'First
                    and then
                    (Operand_Stack (Current_Operand).Content = Character_Data
                     or else Operand_Stack (Current_Operand).Content
                     = Element_Ref);

                  if Current_Operand >= Operand_Stack'First
                    and then Is_Mixed (Operand_Stack (Current_Operand))
                  then
                     Fatal_Error (Parser, Error_Mixed_Contents);
                  end if;

                  while Current_Item >= Operator_Stack'First
                    and then
                      Operator_Stack (Current_Item) /= Opening_Parenthesis
                  loop
                     if Operator_Stack (Current_Item) /= Comma
                       and then Operator_Stack (Current_Item) /= Vertical_Line
                     then
                        Fatal_Error
                          (Parser, Error_Invalid_Content_Model, Start_Token);
                     end if;

                     if Current_Operand = 0 then
                        Fatal_Error
                          (Parser, Error_Missing_Operand, Start_Token);
                     end if;

                     Current_Operand := Current_Operand - 1;

                     if Current_Operand < Operand_Stack'First then
                        Fatal_Error
                          (Parser, Error_Invalid_Content_Model, Start_Token);
                     end if;

                     if Operand_Stack (Current_Operand).Content
                       /= Character_Data and then
                       Operand_Stack (Current_Operand).Content /= Element_Ref
                     then
                        Can_Be_Mixed := False;
                     end if;

                     if Is_Mixed (Operand_Stack (Current_Operand)) then
                        Fatal_Error (Parser, Error_Mixed_Contents);
                     end if;

                     Num_Items := Num_Items + 1;
                     Current_Item := Current_Item - 1;
                  end loop;

                  if Current_Item < Operator_Stack'First then
                     Fatal_Error
                       (Parser, Error_Invalid_Content_Model, Start_Token);
                  end if;

                  if Current_Operand < Operand_Stack'First then
                     Fatal_Error
                       (Parser, Error_Content_Model_Empty_List, Start_Token);
                  end if;

                  if Operator_Stack (Operator_Index - 1) = Comma then
                     M := new Element_Model (Sequence);
                  else
                     if not Can_Be_Mixed
                       and then Operand_Stack (Current_Operand).Content
                       = Character_Data
                     then
                        Fatal_Error
                          (Parser, Error_Content_Model_Nested_Groups);
                     end if;

                     M := new Element_Model (Any_Of);
                  end if;
                  M.List := new Element_Model_Array (1 .. Num_Items);
                  for J in Current_Operand .. Operand_Index - 1 loop
                     M.List (J - Current_Operand + 1) := Operand_Stack (J);
                  end loop;
                  Operand_Index := Current_Operand + 1;
                  Operand_Stack (Current_Operand) := M;
                  Operator_Index := Current_Item;
                  Expect_Operator := False;
                  Test_Multiplier := True;
                  Next_Char (Input, Parser);

                  if not End_Of_Stream (Parser)
                    and then Current_Operand >= Operand_Stack'First
                    and then Is_Mixed (Operand_Stack (Current_Operand))
                    and then Operand_Stack (Current_Operand).List'Length >= 2
                    and then Parser.Last_Read /= Star
                  then
                     Fatal_Error
                       (Parser, Error_Content_Model_Closing_Paren);
                  end if;

               when Comma | Vertical_Line =>
                  if Attlist and then Parser.Last_Read = Comma then
                     Fatal_Error (Parser, Error_Attlist_Invalid_Enum);
                  end if;

                  if Parser.Last_Read = Comma
                    and then Operand_Index - 1 < Operand_Stack'First
                  then
                     Fatal_Error (Parser, Error_Content_Model_Invalid_Seq);
                  end if;

                  if Parser.Last_Read = Comma
                    and then Operator_Stack (Operator_Index - 1)
                    = Opening_Parenthesis
                    and then Operand_Stack (Operand_Index - 1).Content
                    = Character_Data
                  then
                     Fatal_Error (Parser, Error_Content_Model_Pcdata);
                  end if;

                  if Operator_Index = Operator_Stack'First
                    or else
                    (Operator_Stack (Operator_Index - 1) /= Parser.Last_Read
                     and then
                     Operator_Stack (Operator_Index - 1) /=
                       Opening_Parenthesis)
                  then
                     Fatal_Error (Parser, Error_Content_Model_Mixing);
                  end if;
                  Operator_Stack (Operator_Index) := Parser.Last_Read;
                  Operator_Index := Operator_Index + 1;
                  Expect_Operator := False;
                  Next_Char (Input, Parser);

               when Star | Question_Mark | Plus_Sign =>
                  Fatal_Error
                    (Parser, Error_Content_Model_Invalid_Multiplier,
                     Start_Token);

               when Number_Sign =>
                  if Expect_Operator then
                     Fatal_Error
                       (Parser, Error_Content_Model_Invalid_Start,
                        Start_Token);
                  end if;
                  Expect_Operator := True;

                  --  #PCDATA can only be the first element of a choice list
                  --  ??? Note that in that case the Choice model can only be a
                  --  list of names, not a parenthesis expression.
                  Start_Sub := Parser.Buffer_Length + 1;

                  Next_Char (Input, Parser);
                  Found := (Parser.Last_Read = Latin_Capital_Letter_P);
                  if Found then
                     Next_Char (Input, Parser);
                     Found := (Parser.Last_Read = Latin_Capital_Letter_C);
                     if Found then
                        Next_Char (Input, Parser);
                        Found := (Parser.Last_Read = Latin_Capital_Letter_D);
                        if Found then
                           Next_Char (Input, Parser);
                           Found := Parser.Last_Read = Latin_Capital_Letter_A;
                           if Found then
                              Next_Char (Input, Parser);
                              Found :=
                                (Parser.Last_Read = Latin_Capital_Letter_T);
                              if Found then
                                 Next_Char (Input, Parser);
                                 Found :=
                                   (Parser.Last_Read = Latin_Capital_Letter_A);
                              end if;
                           end if;
                        end if;
                     end if;
                  end if;

                  if not Found then
                     Fatal_Error
                       (Parser, Error_Content_Model_Invalid_Seq, Start_Token);
                  end if;

                  if Operator_Stack (Operator_Index - 1)
                    /= Opening_Parenthesis
                  then
                     Fatal_Error (Parser, Error_Content_Model_Pcdata_First);
                  end if;

                  Operand_Stack (Operand_Index) :=
                    new Element_Model (Character_Data);
                  Operand_Index := Operand_Index + 1;
                  Parser.Buffer_Length := Start_Sub - 1;
                  Next_Char (Input, Parser);

               when Percent_Sign =>
                  if not Parser.In_External_Entity
                    and then Parser.State.Name /= DTD_State.Name
                  then
                     Fatal_Error (Parser, Error_ParamEntity_In_Attribute);
                  end if;

                  Start_Sub := Parser.Buffer_Length + 1;

                  while Parser.Last_Read_Is_Valid
                    and then Parser.Last_Read /= Semicolon
                  loop
                     Put_In_Buffer (Parser, Parser.Last_Read);
                     Next_Char (Input, Parser);
                  end loop;

                  Parse_Element_Model_From_Entity
                    (Find_Symbol
                       (Parser,
                        Parser.Buffer (Start_Sub .. Parser.Buffer_Length)));
                  Parser.Buffer_Length := Start_Sub - 1;
                  Next_Char (Input, Parser);

               when others =>
                  if Parser.Last_Read_Is_Valid then
                     if Expect_Operator then
                        Fatal_Error
                          (Parser, Error_Content_Model_Expect_Operator);
                     end if;
                     Expect_Operator := True;

                     --  ??? Should test Is_Nmtoken
                     Start_Sub := Parser.Buffer_Length + 1;

                     while Parser.Last_Read = Unicode.Names.Basic_Latin.Colon
                       or else Is_Valid_Name_Char
                         (Parser.Last_Read, Parser.XML_Version)
                     loop
                        Put_In_Buffer (Parser, Parser.Last_Read);
                        Next_Char (Input, Parser);
                     end loop;

                     if Start_Sub > Parser.Buffer_Length then
                        Error (Parser, Error_Content_Model_Invalid_Name
                               & Debug_Encode (Parser.Last_Read),
                               Start_Token);
                     end if;

                     Operand_Stack (Operand_Index) :=
                       new Element_Model (Element_Ref);
                     Operand_Stack (Operand_Index).Name := Find_Symbol
                       (Parser,
                        Parser.Buffer (Start_Sub .. Parser.Buffer_Length));
                     Operand_Index := Operand_Index + 1;
                     Parser.Buffer_Length := Start_Sub - 1;
                     Test_Multiplier := True;

                  else
                     --  Could happen with improper entity nesting
                     Next_Char (Input, Parser);
                  end if;

            end case;

            if Test_Multiplier then
               case Parser.Last_Read is
                  when Star =>
                     if Operand_Index = Operand_Stack'First then
                        Fatal_Error
                          (Parser, Error_Content_Model_Invalid_Multiplier);
                     end if;
                     Operand_Stack (Operand_Index - 1) := new Element_Model'
                       (Repeat, 0, Positive'Last,
                        Operand_Stack (Operand_Index - 1));
                     Expect_Operator := True;
                     Next_Char (Input, Parser);

                  when Plus_Sign =>
                     if Operand_Index = Operand_Stack'First then
                        Fatal_Error
                          (Parser, Error_Content_Model_Invalid_Multiplier);
                     end if;
                     if Is_Mixed (Operand_Stack (Operand_Index - 1)) then
                        Fatal_Error
                          (Parser, Error_Content_Model_Pcdata_Occurrence);
                     end if;

                     Operand_Stack (Operand_Index - 1) := new Element_Model'
                       (Repeat, 1,
                        Positive'Last, Operand_Stack (Operand_Index - 1));
                     Expect_Operator := True;
                     Next_Char (Input, Parser);

                  when Question_Mark =>
                     if Operand_Index = Operand_Stack'First then
                        Fatal_Error
                          (Parser, Error_Content_Model_Invalid_Multiplier);
                     end if;
                     if Is_Mixed (Operand_Stack (Operand_Index - 1)) then
                        Fatal_Error
                          (Parser, Error_Content_Model_Pcdata_Occurrence);
                     end if;
                     Operand_Stack (Operand_Index - 1) := new Element_Model'
                       (Repeat, 0, 1, Operand_Stack (Operand_Index - 1));
                     Expect_Operator := True;
                     Next_Char (Input, Parser);

                  when others => null;
               end case;
            end if;

            exit when Operator_Index = Operator_Stack'First
              and then Operand_Index = Operand_Stack'First + 1;

            while Is_White_Space (Parser.Last_Read) loop
               Next_Char (Input, Parser);
            end loop;
         end loop;

         if not Is_Recursive_Call then
            if Operator_Index /= Operator_Stack'First
              or else Operand_Index /= Operand_Stack'First + 1
            then
               Error
                 (Parser, Error_Content_Model_Invalid, Start_Token);
            end if;

            Result := Operand_Stack (Operand_Stack'First);

         elsif Num_Parenthesis /= 0 then
            Error (Parser, Error_Entity_Nested, Start_Token);
         end if;

      exception
         when others =>
            if not Is_Recursive_Call then
               for J in Operand_Stack'First .. Operand_Index - 1 loop
                  Free (Operand_Stack (J));
               end loop;
            end if;
            raise;
      end Parse;

   begin
      if Open_Was_Read then
         --  Insert the opening parenthesis into the operators stack
         Operator_Stack (Operator_Stack'First) := Opening_Parenthesis;
         Operator_Index := Operator_Index + 1;
      end if;

      Parse (Input, Result, Open_Was_Read, False);
   end Parse_Element_Model;

   --------------------------------
   -- Check_Valid_Name_Or_NCname --
   --------------------------------

   procedure Check_Valid_Name_Or_NCname
     (Parser : in out Sax_Reader'Class;
      Name   : Token)
   is
   begin
      if Parser.Feature_Namespace then
         if not Is_Valid_NCname
           (Parser.Buffer (Name.First .. Name.Last), Parser.XML_Version)
         then
            Fatal_Error (Parser, Error_Is_Ncname, Name);
         end if;
      else
         if not Is_Valid_Name
           (Parser.Buffer (Name.First .. Name.Last), Parser.XML_Version)
         then
            Fatal_Error (Parser, Error_Is_Name, Name);
         end if;
      end if;
   end Check_Valid_Name_Or_NCname;

   ---------------------------
   -- Check_Attribute_Value --
   ---------------------------

   procedure Check_Attribute_Value
     (Parser     : in out Sax_Reader'Class;
      Local_Name : Symbol;
      Typ        : Attribute_Type;
      Value      : Symbol;
      Error_Loc  : Token)
   is
      Ent : Entity_Entry_Access;
      Val : constant Cst_Byte_Sequence_Access := Get (Value);
   begin
      case Typ is
         when Id | Idref =>
            if Parser.Feature_Namespace then
               if not Is_Valid_NCname (Val.all, Parser.XML_Version) then
                  --  Always a non-fatal error, since we are dealing with
                  --  namespaces
                  Error (Parser, Error_Attribute_Is_Ncname
                         & Get (Local_Name).all, Error_Loc);
               end if;
            else
               if not Is_Valid_Name (Val.all, Parser.XML_Version) then
                  Error (Parser, Error_Attribute_Is_Name
                         & Get (Local_Name).all, Error_Loc);
               end if;
            end if;

         when Idrefs =>
            if Parser.Feature_Namespace then
               if not Is_Valid_NCnames (Val.all, Parser.XML_Version) then
                  Error (Parser, Error_Attribute_Is_Ncname
                         & Get (Local_Name).all, Error_Loc);
               end if;
            else
               if not Is_Valid_Names (Val.all, Parser.XML_Version) then
                  Error (Parser, Error_Attribute_Is_Name
                         & Get (Local_Name).all, Error_Loc);
               end if;
            end if;

         when Nmtoken =>
            if not Is_Valid_Nmtoken (Val.all, Parser.XML_Version) then
               Error (Parser, Error_Attribute_Is_Nmtoken
                      & Get (Local_Name).all, Error_Loc);
            end if;

         when Nmtokens =>
            if not Is_Valid_Nmtokens (Val.all, Parser.XML_Version) then
               Error (Parser, Error_Attribute_Is_Nmtoken
                      & Get (Local_Name).all, Error_Loc);
            end if;

         when Entity =>
            if not Is_Valid_Name (Val.all, Parser.XML_Version) then
               Error (Parser, Error_Attribute_Is_Name
                      & Get (Local_Name).all, Error_Loc);
            end if;

            Ent := Get (Parser.Entities, Value);
            if Ent = null or else not Ent.Unparsed then
               Error (Parser, Error_Attribute_Ref_Unparsed_Entity
                      & Get (Local_Name).all, Error_Loc);
            end if;

         when Entities =>
            declare
               Index : Integer := Val'First;
               Last, Previous  : Integer;
               C     : Unicode_Char;
            begin
               Last := Index;
               while Last <= Val'Last loop
                  Previous := Last;
                  Encoding.Read (Val.all, Last, C);
                  if C = Unicode.Names.Basic_Latin.Space
                    or else Last > Val'Last
                  then
                     if not Is_Valid_Name (Val (Index .. Previous),
                                           Parser.XML_Version)
                     then
                        Error (Parser, Error_Attribute_Is_Name
                               & Get (Local_Name).all,
                               Error_Loc);
                     end if;

                     Ent := Get
                       (Parser.Entities,
                        Find_Symbol (Parser, Val (Index .. Previous)));
                     if Ent = null or else not Ent.Unparsed then
                        Error (Parser, Error_Attribute_Ref_Unparsed_Entity
                               & Get (Local_Name).all,
                               Error_Loc);
                     end if;
                     Index := Last;
                  end if;
               end loop;
            end;

         when others =>
            null;
      end case;
   end Check_Attribute_Value;

   ---------
   -- Add --
   ---------

   procedure Add
     (Parser             : in out Sax_Reader'Class;
      Attr               : in out Sax_Attribute_Array_Access;
      Count              : in out Natural;
      If_Unique          : Boolean;
      Location           : Sax.Locators.Location;
      Local_Name, Prefix : Symbol;
      Value              : Symbol;
      Att_Type           : Attribute_Type := Cdata;
      Default_Decl       : Default_Declaration := Default)
   is
      pragma Unreferenced (Parser);
      Tmp : Sax_Attribute_Array_Access;
   begin
      if If_Unique then
         for A in 1 .. Count loop
            if Attr (A).Local_Name = Local_Name
              and then Attr (A).Prefix = Prefix
            then
               return;
            end if;
         end loop;
      end if;

      if Attr = null or else Count = Attr'Last then
         Tmp := Attr;
         if Tmp /= null then
            Attr := new Sax_Attribute_Array (Tmp'First .. Tmp'Last + 1);
            Attr (Tmp'Range) := Tmp.all;
            Unchecked_Free (Tmp);
         else
            Attr  := new Sax_Attribute_Array (1 .. 1);
            Count := 0;
         end if;
      end if;

      --  The URI cannot be resolved at this point, since it will
      --  depend on the contents of the document at the place where
      --  the attribute is used.

      Count := Count + 1;
      Attr (Count) := Sax_Attribute'
        (Prefix       => Prefix,
         Local_Name   => Local_Name,
         Value        => Value,
         Non_Normalized_Value => Value,
         Att_Type     => Att_Type,
         NS           => No_XML_NS,
         Default_Decl => Default_Decl,
         Location     => Location);
   end Add;

   ---------------------
   -- Syntactic_Parse --
   ---------------------

   procedure Syntactic_Parse
     (Parser : in out Sax_Reader'Class;
      Input  : in out Input_Sources.Input_Source'Class)
   is
      Id  : Token := Null_Token;

      procedure Parse_Start_Tag;
      --  Process an element start and its attributes   <!name name="value"..>

      procedure Parse_Attributes
        (Elem_NS_Id, Elem_Name_Id : Token; Id : in out Token);
      --  Process the list of attributes in a start tag, and store them in
      --  Parser.Attributes.
      --  Id should have been initialized to the first token in the attributes
      --  list, and will be left on the first token after it.
      --  Return the list of attributes for this element
      --  On exit, NS_Count is set to the number of references to Elem_NS_Id
      --  among the attributes. The count for other XML_NS that the one of the
      --  element is directly increment in the corresponding XML_NS, but for
      --  the element we want to keep it virgin until we have called the
      --  validation hook.

      procedure Resolve_Attribute_Namespaces;
      --  For each attributes defined in Parser.Attributes, set its URI for
      --  the namespace

      procedure Check_And_Define_Namespace
        (Prefix, URI : Symbol; Location : Sax.Locators.Location);
      --  An attribute defining a namespace was found. Check that the values
      --  are valid, and register the new namespace. If Prefix is Null_Token,
      --  the default namespace is defined

      function Get_String (Str : Token) return String;
      function Get_String (First, Last : Token) return String;
      pragma Inline (Get_String);
      --  Return the string pointed to by the token

      procedure Add_Default_Attributes (DTD_Attr : Sax_Attribute_Array_Access);
      --  Add all DEFAULT attributes declared in the DTD into the attributes of
      --  the current element, if they weren't overriden by the user

      procedure Parse_End_Tag;
      --  Process an element end   </name>

      procedure Parse_Doctype;
      --  Process the DTD declaration

      procedure Parse_Doctype_Contents;
      --  Process the DTD's contents

      procedure Parse_Entity_Def (Id : in out Token);
      --  Parse an <!ENTITY declaration

      procedure Parse_Element_Def (Id : in out Token);
      --  Parse an <!ELEMENT declaration

      procedure Parse_Notation_Def (Id : in out Token);
      --  Parse an <!NOTATION declaration

      procedure Parse_Attlist_Def (Id : in out Token);
      --  Parse an <!ATTLIST declaration

      procedure Parse_PI (Id : in out Token);
      --  Parse a <?...?> processing instruction

      procedure End_Element;
      --  End the current element. Its namespace prefix and local_name are
      --  given in the parameters.

      procedure Get_String
        (Id : in out Token;
         State : Parser_State;
         Str_Start, Str_End : out Token;
         Normalize         : Boolean := False;
         Collapse_Spaces   : Boolean := False);
      --  Get all the character till the end of the string. Id should contain
      --  the initial quote that starts the string.
      --  On exit, Str_Start is set to the first token of the string, and
      --  Str_End to the last token.
      --  If Normalize is True, then all space characters are converted to
      --  ' '.
      --  If Collapse_Spaces is True, then all duplicate spaces sequences are
      --  collapsed into a single space character. Leading and trailing spaces
      --  are also removed.

      procedure Get_Name_NS (Id : in out Token; NS_Id, Name_Id : out Token);
      --  Read the next tokens so as to match either a single name or
      --  a "ns:name" name.
      --  Id should initially point to the candidate token for the name, and
      --  will be left on the token following that name.
      --  An error is raised if we can't even match a Name.

      procedure Get_External
        (Id : in out Token;
         System_Start, System_End, Public_Start, Public_End : out Token;
         Allow_Publicid : Boolean := False);
      --  Parse a PUBLIC or SYSTEM definition and its arguments.
      --  Id should initially point to the keyword itself, and will be set to
      --  the first identifier following the full definition
      --  If Allow_Publicid is True, then PUBLIC might be followed by a single
      --  string, as in rule [83] of the XML specifications.

      procedure Check_Standalone_Value (Id : in out Token);
      procedure Check_Encoding_Value (Id : in out Token);
      procedure Check_Version_Value (Id : in out Token);
      --  Check the arguments for the <?xml?> processing instruction.
      --  Each of this procedures gets the arguments from Next_Token, up to,
      --  and including, the following space or End_Of_PI character.
      --  They raise errors appropriately

      procedure Check_Model;
      --  Check that the last element inserted matches the model. This
      --  procedure should not be called for the root element.

      ----------------
      -- Get_String --
      ----------------

      procedure Get_String
        (Id : in out Token;
         State : Parser_State;
         Str_Start, Str_End : out Token;
         Normalize : Boolean := False;
         Collapse_Spaces : Boolean := False)
      is
         T : constant Token := Id;
         Saved_State : constant Parser_State := Get_State (Parser);
         Possible_End : Token := Null_Token;
         C : Unicode_Char;
         Index : Natural;
         Last_Space : Natural := 0;
         Had_Space : Boolean := Collapse_Spaces; --  Avoid leading spaces

      begin
         if Debug_Internal then
            Put_Line ("Get_String Normalize="
                      & Boolean'Image (Normalize)
                      & " Collapse_Spaces="
                      & Boolean'Image (Collapse_Spaces));
         end if;
         Set_State (Parser, State);
         Next_Token (Input, Parser, Id);
         Str_Start := Id;
         Str_End := Id;

         while Id.Typ /= T.Typ and then Id.Typ /= End_Of_Input loop
            Str_End := Id;
            case Id.Typ is
               when Double_String_Delimiter =>
                  Str_End.First := Parser.Buffer_Length + 1;
                  Put_In_Buffer (Parser, Quotation_Mark);
                  Str_End.Last := Parser.Buffer_Length;
                  Possible_End := Str_End;
                  Had_Space := False;
               when Single_String_Delimiter =>
                  Str_End.First := Parser.Buffer_Length + 1;
                  Put_In_Buffer (Parser, Apostrophe);
                  Str_End.Last := Parser.Buffer_Length;
                  Possible_End := Str_End;
                  Had_Space := False;
               when Start_Of_Tag =>
                  if Possible_End = Null_Token then
                     Fatal_Error (Parser, Error_Attribute_Less_Than, Id);
                  else
                     Fatal_Error
                       (Parser, Error_Attribute_Less_Than_Suggests
                        & Location (Parser, Possible_End.Location), Id);
                  end if;
               when Char_Ref =>
                  --  3.3.3 item 3: character references are kept as is
                  if Get_String (Id) = Space_Sequence then
                     if Collapse_Spaces and Had_Space then
                        Reset_Buffer (Parser, Id);
                     end if;
                     Had_Space := True;
                     Last_Space := Parser.Buffer_Length;
                  else
                     Had_Space := False;
                  end if;

               when others =>
                  if Normalize or Collapse_Spaces then
                     declare
                        Str : constant Byte_Sequence :=
                          Parser.Buffer (Id.First .. Id.Last);
                     begin
                        Reset_Buffer (Parser, Id);
                        Index := Str'First;
                        while Index <= Str'Last loop
                           Encoding.Read (Str, Index, C);

                           --  ??? If we have a character reference, we must
                           --  replace the character it represents, and not do
                           --  entity replacement. How to do that, we have lost
                           --  that information

                           --  When parsing an attribute value, we should still
                           --  process white spaces, therefore the test for
                           --  Ignore_Special
                           if Is_White_Space (C) then
                              if not Collapse_Spaces or not Had_Space then
                                 Put_In_Buffer
                                   (Parser, Unicode.Names.Basic_Latin.Space);
                              end if;
                              Had_Space := True;
                              Last_Space := Parser.Buffer_Length;
                           else
                              Had_Space := False;
                              Put_In_Buffer (Parser, C);
                           end if;
                        end loop;
                     end;
                     Str_End.Last := Parser.Buffer_Length;
                  end if;
            end case;
            Next_Token (Input, Parser, Id);
         end loop;

         if Collapse_Spaces and then Had_Space and then Last_Space /= 0 then
            Str_End.Last := Last_Space - 1;
         end if;

         if Id.Typ = End_Of_Input then
            if Possible_End = Null_Token then
               Fatal_Error (Parser, Error_Unterminated_String);
            else
               Fatal_Error (Parser, Error_Unterminated_String_Suggests
                            & Location (Parser, Possible_End.Location), T);
            end if;
         end if;
         Set_State (Parser, Saved_State);
      end Get_String;

      ------------------
      -- Get_External --
      ------------------

      procedure Get_External
        (Id : in out Token;
         System_Start, System_End, Public_Start, Public_End : out Token;
         Allow_Publicid : Boolean := False)
      is
         Had_Space : Boolean;
         C : Unicode_Char;
         Index : Natural;
      begin
         System_Start := Null_Token;
         System_End := Null_Token;
         Public_Start := Null_Token;
         Public_End := Null_Token;

         --  Check the arguments for PUBLIC
         if Id.Typ = Public then
            Next_Token_Skip_Spaces (Input, Parser, Id, Must_Have => True);
            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               Fatal_Error (Parser, Error_Public_String);
            else
               Get_String
                 (Id, Non_Interpreted_String_State, Public_Start, Public_End);

               Index := Public_Start.First;
               while Index <= Public_End.Last loop
                  Encoding.Read (Parser.Buffer.all, Index, C);

                  if not Is_Pubid_Char (C) then
                     Fatal_Error
                       (Parser, Error_Public_Invalid & "'"
                        & Debug_Encode (C) & "'", Public_Start);
                  end if;
               end loop;
            end if;

            Next_Token (Input, Parser, Id);
            Had_Space := (Id.Typ = Space);
            if Had_Space then
               Next_Token (Input, Parser, Id);
            elsif Allow_Publicid then
               return;
            end if;

            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               if not Allow_Publicid then
                  Fatal_Error (Parser, Error_Public_Sysid);
               end if;
            else
               if not Had_Space then
                  Fatal_Error (Parser, Error_Public_Sysid_Space, Id);
               end if;
               Get_String
                 (Id, Non_Interpreted_String_State, System_Start, System_End);
               Next_Token (Input, Parser, Id);
            end if;

            --  Check the arguments for SYSTEM
         elsif Id.Typ = System then
            Next_Token_Skip_Spaces (Input, Parser, Id, Must_Have => True);
            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               Fatal_Error (Parser, Error_System_String);
            else
               Get_String
                 (Id, Non_Interpreted_String_State, System_Start, System_End);
               Next_Token (Input, Parser, Id);
            end if;
         end if;
      end Get_External;

      -----------------
      -- Get_Name_NS --
      -----------------

      procedure Get_Name_NS (Id : in out Token; NS_Id, Name_Id : out Token) is
      begin
         Name_Id := Id;

         if Id.Typ = Text then
            Fatal_Error
              (Parser, Error_Invalid_Name & "'"
               & Parser.Buffer (Id.First .. Id.Last) & "'", Id);
         --  An empty namespace ? This seems to be useful only for the XML
         --  conformance suite, so we only handle the case of a single ':'
         --  to mean both an empty prefix and empty local name.
         elsif Name_Id.Typ = Colon then
            Name_Id.Typ := Text;
            NS_Id := Name_Id;
            Next_Token (Input, Parser, Id);

         elsif Id.Typ /= Name then
            Fatal_Error (Parser, Error_Is_Name, Id);

         else
            Next_Token (Input, Parser, Id);
            if Id.Typ = Colon then
               NS_Id := Name_Id;
               Next_Token (Input, Parser, Name_Id);
               if Name_Id.Typ /= Name then
                  Fatal_Error (Parser, Error_Is_Name);
               end if;
               Next_Token (Input, Parser, Id);
            else
               NS_Id := Null_Token;
            end if;
         end if;
      end Get_Name_NS;

      ----------------------
      -- Parse_Entity_Def --
      ----------------------

      procedure Parse_Entity_Def (Id : in out Token) is
         Is_Parameter : Token := Null_Token;
         Name_Id : Token;
         Def_Start, Def_End : Token := Null_Token;
         Ndata_Id : Token := Null_Token;
         Public_Start, Public_End : Token := Null_Token;
         System_Start, System_End : Token := Null_Token;
         Had_Space : Boolean;
         Sym : Symbol;
      begin
         Set_State (Parser, Entity_Def_State);
         Next_Token_Skip_Spaces (Input, Parser, Name_Id, True);

         if Debug_Internal then
            Put_Line ("Parsing entity definition "
                      & Parser.Buffer (Name_Id.First .. Name_Id.Last));
         end if;

         if Name_Id.Typ = Text
           and then Parser.Buffer (Name_Id.First .. Name_Id.Last) =
           Percent_Sign_Sequence
         then
            Is_Parameter := Name_Id;
            Next_Token_Skip_Spaces (Input, Parser, Name_Id);
         end if;

         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, Error_Is_Name);
         end if;

         Check_Valid_Name_Or_NCname (Parser, Name_Id);

         Next_Token_Skip_Spaces (Input, Parser, Id, Must_Have => True);

         if Id.Typ = Public or else Id.Typ = System then
            Get_External
              (Id, System_Start, System_End, Public_Start, Public_End);

            if Contains_URI_Fragment
              (Parser.Buffer (System_Start.First .. System_End.Last))
            then
               Error (Parser, Error_System_URI, Id);
            end if;

            Had_Space := (Id.Typ = Space);
            if Had_Space then
               Next_Token (Input, Parser, Id);
            end if;

            if Id.Typ = Ndata then
               if not Had_Space then
                  Fatal_Error (Parser, Error_Ndata_Space, Id);
               end if;

               if Is_Parameter /= Null_Token then
                  Fatal_Error (Parser, Error_Ndata_ParamEntity, Id);
               end if;
               Next_Token_Skip_Spaces (Input, Parser, Ndata_Id, True);

               if Ndata_Id.Typ /= Text and then Ndata_Id.Typ /= Name then
                  Fatal_Error (Parser, Error_Ndata_String);
               else
                  Sym := Find_Symbol (Parser, Ndata_Id);

                  if Parser.Feature_Validation
                    and then Get (Parser.Notations, Sym) = Null_Notation
                  then
                     --  The notation might be declared later in the same DTD
                     Set (Parser.Notations,
                       (Name             => Sym,
                        Declaration_Seen => False));
                  end if;

                  Next_Token_Skip_Spaces (Input, Parser, Id);
               end if;
            end if;

         elsif Id.Typ = Double_String_Delimiter
           or else Id.Typ = Single_String_Delimiter
         then
            Get_String (Id, Entity_Str_Def_State, Def_Start, Def_End);
            Next_Token_Skip_Spaces (Input, Parser, Id);
         else
            Fatal_Error (Parser, Error_Entity_Definition);
         end if;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, Error_Entity_Definition_Unterminated);
         end if;

         --  Only report the first definition

         Sym := Find_Symbol
           (Parser,
            Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
            & Parser.Buffer (Name_Id.First .. Name_Id.Last));

         if Get (Parser.Entities, Sym) /= null then
            null;

         elsif Def_End /= Null_Token then
            Set (Parser.Entities,
                 new Entity_Entry'
                   (Name => Find_Symbol
                      (Parser,
                       Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
                       & Parser.Buffer (Name_Id.First .. Name_Id.Last)),
                    Value => Find_Symbol
                      (Parser,
                       Parser.Buffer (Def_Start.First .. Def_End.Last)),
                    Public       => No_Symbol,
                    Unparsed     => False,
                    External_Declaration => (Parser.Inputs /= null
                       and then Parser.Inputs.External)
                       or else Parser.In_External_Entity,
                    External     => False,
                 Already_Read => False));
            if Debug_Internal then
               Put_Line ("Internal_Entity_Decl: "
                         & Parser.Buffer (Name_Id.First .. Name_Id.Last) & "="
                         & Parser.Buffer (Def_Start.First .. Def_End.Last)
                         & " length="
                         & Integer'Image (Def_End.Last - Def_Start.First + 1));
            end if;
            Internal_Entity_Decl
              (Parser,
               Name => Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
               & Parser.Buffer (Name_Id.First .. Name_Id.Last),
               Value => Parser.Buffer (Def_Start.First .. Def_End.Last));

         elsif Ndata_Id /= Null_Token then
            Set (Parser.Entities,
                 new Entity_Entry'
                   (Name => Find_Symbol
                      (Parser,
                       Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
                       & Parser.Buffer (Name_Id.First .. Name_Id.Last)),
                    Value        => No_Symbol,
                    Public       => No_Symbol,
                    Unparsed     => True,
                    External_Declaration => (Parser.Inputs /= null
                       and then Parser.Inputs.External)
                       or else Parser.In_External_Entity,
                    External     => False,
                 Already_Read => True));
            Unparsed_Entity_Decl
              (Parser,
               Name => Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
               & Parser.Buffer (Name_Id.First .. Name_Id.Last),
               System_Id =>
                 Parser.Buffer (System_Start.First .. System_End.Last),
               Notation_Name =>
                 Parser.Buffer (Ndata_Id.First .. Ndata_Id.Last));

         else
            Set
              (Parser.Entities,
               new Entity_Entry'
                 (Name => Find_Symbol
                    (Parser,
                     Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
                     & Parser.Buffer (Name_Id.First .. Name_Id.Last)),
                  Value => Find_Symbol
                    (Parser,
                     Parser.Buffer (System_Start.First .. System_End.Last)),
                  Public       => Find_Symbol
                    (Parser,
                     Parser.Buffer (Public_Start.First .. Public_End.Last)),
                  Unparsed     => False,
                  External_Declaration => (Parser.Inputs /= null
                     and then Parser.Inputs.External)
                     or else Parser.In_External_Entity,
                  External     => True,
                  Already_Read => False));

            External_Entity_Decl
              (Parser,
               Name => Parser.Buffer (Is_Parameter.First .. Is_Parameter.Last)
               & Parser.Buffer (Name_Id.First .. Name_Id.Last),
               Public_Id => Parser.Buffer
                 (Public_Start.First .. Public_End.Last),
               System_Id => Parser.Buffer
                 (System_Start.First .. System_End.Last));
         end if;

         if Is_Parameter /= Null_Token then
            Reset_Buffer (Parser, Is_Parameter);
         else
            Reset_Buffer (Parser, Name_Id);
         end if;
         Set_State (Parser, DTD_State);
      end Parse_Entity_Def;

      -----------------------
      -- Parse_Element_Def --
      -----------------------

      procedure Parse_Element_Def (Id : in out Token) is
         Name_Id : Token;
         M : Element_Model_Ptr;
         M2 : Content_Model;
         NS_Id : Token;
      begin
         Set_State (Parser, Element_Def_State);

         Next_NS_Token_Skip_Spaces (Input, Parser, NS_Id, Name_Id);

         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, Error_Is_Name);
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id, Must_Have => True);

         case Id.Typ is
            when Empty  => M := new Element_Model (Empty);
            when Any    => M := new Element_Model (Anything);
            when Open_Paren =>
               Parse_Element_Model
                 (Input, Parser, M, Attlist => False, Open_Was_Read => True);
            when others =>
               Fatal_Error (Parser, "Invalid content model: expecting"
                            & " '(', 'EMPTY' or 'ANY'", Id);
         end case;
         Next_Token_Skip_Spaces (Input, Parser, Id);

         if Id.Typ /= End_Of_Tag then
            Free (M);
            Fatal_Error (Parser, "Expecting end of ELEMENT definition");
         end if;

         M2 := Create_Model (M);
         Element_Decl
           (Parser, Parser.Buffer (Name_Id.First .. Name_Id.Last), M2);
         Unref (M2);

         if NS_Id /= Null_Token then
            Reset_Buffer (Parser, NS_Id);
         else
            Reset_Buffer (Parser, Name_Id);
         end if;

         Set_State (Parser, DTD_State);
      end Parse_Element_Def;

      ------------------------
      -- Parse_Notation_Def --
      ------------------------

      procedure Parse_Notation_Def (Id : in out Token) is
         Public_Start, Public_End : Token := Null_Token;
         System_Start, System_End : Token := Null_Token;
         Name_Id : Token;
         Sym     : Symbol;
      begin
         Set_State (Parser, Element_Def_State);
         Next_Token_Skip_Spaces (Input, Parser, Name_Id);

         Check_Valid_Name_Or_NCname (Parser, Name_Id);

         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, Error_Is_Name);
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);

         if Id.Typ = Public or else Id.Typ = System then
            Get_External
              (Id, System_Start, System_End, Public_Start, Public_End, True);
            if Id.Typ = Space then
               Next_Token (Input, Parser, Id);
            end if;
         else
            Fatal_Error (Parser, Error_Invalid_Notation_Decl);
         end if;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "Expecting end of NOTATION definition");
         end if;

         if Contains_URI_Fragment
           (Parser.Buffer (System_Start.First .. System_End.Last))
         then
            Error (Parser, Error_System_URI);
         end if;

         if Parser.Hooks.Notation_Decl /= null then
            Parser.Hooks.Notation_Decl
              (Parser'Access,
               Name => Parser.Buffer (Name_Id.First .. Name_Id.Last),
               Public_Id =>
                 Parser.Buffer (Public_Start.First .. Public_End.Last),
               System_Id =>
                 Parser.Buffer (System_Start.First .. System_End.Last));
         end if;

         Notation_Decl
           (Parser,
            Name => Parser.Buffer (Name_Id.First .. Name_Id.Last),
            Public_Id => Parser.Buffer (Public_Start.First .. Public_End.Last),
            System_Id =>
              Parser.Buffer (System_Start.First .. System_End.Last));

         if Parser.Feature_Validation then
            Sym := Find_Symbol (Parser, Name_Id);
            Remove (Parser.Notations, Sym);
            Set (Parser.Notations,
                 (Name             => Sym,
                  Declaration_Seen => True));
         end if;

         Set_State (Parser, DTD_State);
         Reset_Buffer (Parser, Name_Id);
      end Parse_Notation_Def;

      -----------------------
      -- Parse_Attlist_Def --
      -----------------------

      procedure Parse_Attlist_Def (Id : in out Token) is
         M : Element_Model_Ptr;
         M2 : Content_Model;
         Default_Start, Default_End : Token;
         Ename_Id, Ename_NS_Id, Name_Id, NS_Id, Type_Id : Token;
         Default_Id : Token;
         Attr : Attributes_Table.Element_Ptr;
         Last : Natural;
         Default_Decl : Default_Declaration;
         Att_Type : Attribute_Type;
         Ename, SName : Symbol;
      begin
         Set_State (Parser, Element_Def_State);

         Next_NS_Token_Skip_Spaces (Input, Parser, Ename_NS_Id, Ename_Id);

         if Ename_Id.Typ /= Name then
            Fatal_Error (Parser, Error_Is_Name, Ename_Id);
         end if;

         Ename := Find_Symbol (Parser, Ename_Id);

         Attr := Get_Ptr (Parser.Default_Atts, Ename);
         if Attr = null then
            declare
               Attr2 : constant Attributes_Entry :=
                 (Element_Name => Ename,
                  Attributes   => null);
            begin
               Set (Parser.Default_Atts, Attr2);
               Attr := Get_Ptr (Parser.Default_Atts, Ename);
            end;
         end if;

         if Attr.Attributes = null then
            Last := 0;
         else
            Last := Attr.Attributes'Last;
         end if;

         if Id.Typ = Space then
            Next_Token_Skip_Spaces (Input, Parser, Id);
         end if;

         loop
            --  Temporarily disable In_Attlist, so that the names like "NAME"
            --  are parsed as names and not as NMTOKEN.
            Set_State (Parser, Attribute_Def_Name_State);

            Next_Token_Skip_Spaces (Input, Parser, Id);
            exit when Id.Typ = End_Of_Tag or else Id.Typ = End_Of_Input;

            Get_Name_NS (Id, NS_Id, Name_Id);
            SName := Find_Symbol (Parser, Name_Id);

            if Id.Typ /= Space then
               Fatal_Error (Parser, Error_Expecting_Space, Id);  --  3.3
            end if;

            Set_State (Parser, Attribute_Def_State);
            Next_Token_Skip_Spaces (Input, Parser, Id);

            Type_Id := Id;
            Default_Start := Null_Token;
            Default_End := Null_Token;
            case Type_Id.Typ is
               when Id_Type  => Att_Type := Sax.Attributes.Id;
               when Idref    => Att_Type := Sax.Attributes.Idref;
               when Idrefs   => Att_Type := Sax.Attributes.Idrefs;
               when Cdata    => Att_Type := Sax.Attributes.Cdata;
               when Nmtoken  => Att_Type := Sax.Attributes.Nmtoken;
               when Nmtokens => Att_Type := Sax.Attributes.Nmtokens;
               when Entity   => Att_Type := Sax.Attributes.Entity;
               when Entities => Att_Type := Sax.Attributes.Entities;
               when Notation =>
                  Att_Type := Notation;
                  Next_Token (Input, Parser, Id);
                  if Id.Typ /= Space then
                     Fatal_Error
                       (Parser,  --  3.3.1
                        "Space is required between NOTATION keyword"
                        & " and list of enumerated", Id);
                  end if;
                  Parse_Element_Model (Input, Parser, M, True, False);

                  if Parser.Feature_Validation then
                     for J in M.List'Range loop
                        if Get (Parser.Notations, M.List (J).Name) /=
                          Null_Notation
                        then
                           Error
                             (Parser, Error_Notation_Undeclared
                              & Get (M.List (J).Name).all, Id);
                        end if;
                     end loop;
                  end if;

               when Open_Paren =>
                  Att_Type := Enumeration;
                  Parse_Element_Model (Input, Parser, M, True, True);

               when others =>
                  Fatal_Error (Parser, Error_Attlist_Type);
            end case;

            declare
               QName : constant Byte_Sequence :=
                 Qname_From_Name (Parser, NS_Id, Name_Id);
               Default_Val : Symbol;
            begin
               Next_Token_Skip_Spaces (Input, Parser, Default_Id, True);
               if Default_Id.Typ = Implied then
                  Default_Decl := Sax.Attributes.Implied;
               elsif Default_Id.Typ = Required then
                  Default_Decl := Sax.Attributes.Required;
               else
                  Id := Default_Id;
                  if Default_Id.Typ = Fixed then
                     Next_Token_Skip_Spaces (Input, Parser, Id, True);
                     Default_Decl := Sax.Attributes.Fixed;
                  else
                     Default_Decl := Sax.Attributes.Default;
                  end if;

                  if Id.Typ = Double_String_Delimiter
                    or else Id.Typ = Single_String_Delimiter
                  then
                     Get_String
                       (Id, Attlist_Str_Def_State, Default_Start, Default_End,
                        Normalize => True, Collapse_Spaces => True);

                     --  Errata 9 on XML 1.0 specs: the default value must be
                     --  syntactically correct. Validity will only be checked
                     --  if the attribute is used.

                     Default_Val := Find_Symbol
                       (Parser, Default_Start, Default_End);

                     if Parser.Feature_Validation then
                        Check_Attribute_Value
                          (Parser,
                           Local_Name => SName,
                           Typ       => Att_Type,
                           Value     => Default_Val,
                           Error_Loc => Default_Start);
                     end if;
                  else
                     Fatal_Error
                       (Parser, "Invalid default value for attribute");
                  end if;
               end if;

               if Parser.Feature_Validation
                 and then Att_Type = Sax.Attributes.Id
                 and then Default_Decl /= Sax.Attributes.Implied
                 and then Default_Decl /= Sax.Attributes.Required
               then
                  Error
                    (Parser,
                     "Default value for an ID attribute must be"
                     & " IMPLIED or REQUIRED",
                     Default_Id);
               end if;

               --  Always report the attribute, even when we know the value
               --  won't be used. We can't do it coherently otherwise, in case
               --  an attribute is seen in the external subset, and then
               --  overriden in the internal subset.
               M2 := Create_Model (M);
               Attribute_Decl
                 (Parser,
                  Ename => Parser.Buffer (Ename_Id.First .. Ename_Id.Last),
                  Aname => QName,
                  Typ   => Att_Type,
                  Content => M2,
                  Value_Default => Default_Decl,
                  Value => Parser.Buffer
                    (Default_Start.First .. Default_End.Last));
               Unref (M2);

               Add
                 (Parser       => Parser,
                  Attr         => Attr.Attributes,
                  Count        => Last,
                  If_Unique    => True,
                  Location     => Name_Id.Location,
                  Local_Name   => SName,
                  Prefix       => Find_Symbol (Parser, NS_Id),
                  Value        => Default_Val,
                  Att_Type     => Att_Type,
                  Default_Decl => Default_Decl);
            end;

            --  M will be freed automatically when the Default_Atts field is
            --  freed. However, we need to reset it for the next attribute
            --  in the list.
            M := null;

            if NS_Id /= Null_Token then
               Reset_Buffer (Parser, NS_Id);
            else
               Reset_Buffer (Parser, Name_Id);
            end if;
            Set_State (Parser, Element_Def_State);
         end loop;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "Expecting end of ATTLIST definition");
         end if;

         Set_State (Parser, DTD_State);

         if Ename_NS_Id /= Null_Token then
            Reset_Buffer (Parser, Ename_NS_Id);
         else
            Reset_Buffer (Parser, Ename_Id);
         end if;

      exception
         when others =>
            Free (M);
            raise;
      end Parse_Attlist_Def;

      -----------------
      -- Check_Model --
      -----------------

      procedure Check_Model is
      begin
         null;
      end Check_Model;

      ----------------
      -- Get_String --
      ----------------

      function Get_String (Str : Token) return String is
      begin
         return Parser.Buffer (Str.First .. Str.Last);
      end Get_String;

      ----------------
      -- Get_String --
      ----------------

      function Get_String (First, Last : Token) return String is
      begin
         return Parser.Buffer (First.First .. Last.Last);
      end Get_String;

      --------------------------------
      -- Check_And_Define_Namespace --
      --------------------------------

      procedure Check_And_Define_Namespace
        (Prefix, URI : Symbol; Location : Sax.Locators.Location) is
      begin
         if Prefix = Empty_String then
            if URI = Empty_String then
               --  [2] Empty value is legal for the default namespace, and
               --  provides unbinding
               null;
            end if;

         else
            if Prefix = Parser.Xmlns_Sequence then
               Fatal_Error  --  NS 3
                 (Parser, "Cannot redefine the xmlns prefix", Location);

            elsif URI = Empty_String then
               Fatal_Error
                 (Parser,  --  NS 2.2
                  "Cannot use an empty URI for namespaces", Location);

            elsif Prefix = Parser.Xml_Sequence then
               if URI /= Parser.Namespaces_URI_Sequence then
                  Fatal_Error  --  NS 3
                    (Parser, "Cannot redefine the xml prefix", Location);
               end if;

            elsif URI = Parser.Namespaces_URI_Sequence then
               Fatal_Error
                 (Parser,  --  NS 3
                  "Cannot bind the namespace URI to a prefix other"
                  & " than xml", Location);
            end if;
         end if;

         if URI /= Empty_String
           and then not Is_Valid_IRI
             (Get (URI).all, Version => Parser.XML_Version)
         then
            Error
              (Parser,
               "Invalid absolute IRI (Internationalized Resource"
               & " Identifier) for namespace: """ & Get (URI).all & """",
               Location);
            --  NS 2
         end if;

         Add_Namespace (Parser, Parser.Current_Node, Prefix, URI);
      end Check_And_Define_Namespace;

      ----------------------------
      -- Add_Default_Attributes --
      ----------------------------

      procedure Add_Default_Attributes
        (DTD_Attr : Sax_Attribute_Array_Access)
      is
         Found    : Boolean;
         Is_Xmlns : Boolean;
      begin
         --  Add all the default attributes to the element.
         --  We shouldn't add an attribute if it was overriden by the user

         if DTD_Attr /= null then
            for J in DTD_Attr'Range loop
               --  We must compare Qnames, since namespaces haven't been
               --  resolved in the default attributes.
               if DTD_Attr (J).Default_Decl = Default
                 or else DTD_Attr (J).Default_Decl = Fixed
               then
                  Found := False;

                  for A in 1 .. Parser.Attributes.Count loop
                     if Parser.Attributes.List (A).Local_Name =
                         DTD_Attr (J).Local_Name
                       and then Parser.Attributes.List (A).Prefix =
                         DTD_Attr (J).Prefix
                     then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Is_Xmlns := DTD_Attr (J).Prefix = Parser.Xmlns_Sequence;

                     if Parser.Feature_Namespace_Prefixes
                       or else not Is_Xmlns
                     then
                        Add
                          (Parser       => Parser,
                           Attr         => Parser.Attributes.List,
                           Count        => Parser.Attributes.Count,
                           If_Unique    => True,
                           Location     => No_Location,
                           Local_Name   => DTD_Attr (J).Local_Name,
                           Prefix       => DTD_Attr (J).Prefix,
                           Value        => DTD_Attr (J).Value,
                           Att_Type     => DTD_Attr (J).Att_Type,
                           Default_Decl => DTD_Attr (J).Default_Decl);
                     end if;

                     --  Is this a namespace declaration ?
                     if Is_Xmlns then
                        --  Following warning is because for parser that don't
                        --  read external DTDs, the behavior would be different
                        --  for the same document.
                        Warning
                          (Parser,
                           "namespace-declaring attribute inserted via "
                           & "DTD defaulting mechanisms are not good style");
                        Add_Namespace
                          (Parser, Parser.Current_Node,
                           Prefix => DTD_Attr (J).Local_Name,
                           URI    => DTD_Attr (J).Value);
                     end if;
                  end if;
               end if;
            end loop;
         end if;
      end Add_Default_Attributes;

      ----------------------------------
      -- Resolve_Attribute_Namespaces --
      ----------------------------------

      procedure Resolve_Attribute_Namespaces is
         NS         : XML_NS;
      begin
         if Parser.Feature_Namespace then
            for J in 1 .. Parser.Attributes.Count loop
               Find_NS (Parser, Parser.Attributes.List (J).Prefix, NS,
                        Include_Default_NS => False);
               if NS = No_XML_NS then
                  Fatal_Error
                    (Parser, Error_Prefix_Not_Declared
                     & Get (Parser.Attributes.List (J).Prefix).all);
               end if;

               for A in 1 .. J - 1 loop
                  if Get_URI (Parser.Attributes.List (A).NS) = Get_URI (NS)
                    and then Parser.Attributes.List (A).Local_Name =
                      Parser.Attributes.List (J).Local_Name
                  then
                     Fatal_Error --  3.1
                       (Parser, "Attributes may appear only once: "
                        & To_QName
                          (Get_URI (NS),
                           Parser.Attributes.List (J).Local_Name),
                        Parser.Attributes.List (J).Location);
                  end if;
               end loop;

               Parser.Attributes.List (J).NS := NS;
            end loop;
         end if;
      end Resolve_Attribute_Namespaces;

      ----------------------
      -- Parse_Attributes --
      ----------------------

      procedure Parse_Attributes
        (Elem_NS_Id, Elem_Name_Id : Token; Id : in out Token)
      is
         Elem : constant Symbol := Find_Symbol
           (Parser, Qname_From_Name (Parser, Elem_NS_Id, Elem_Name_Id));
         Attr : constant Sax_Attribute_Array_Access := Get
           (Parser.Default_Atts, Elem).Attributes;
         --  The attributes as defined in the DTD

         Attr_NS_Id   : Token;
         Attr_Name_Id : Token;
         Value_Start  : Token;
         Value_End    : Token;
         Add_Attr     : Boolean;
         A            : Integer;
         Attr_Name, Attr_Prefix, Attr_Value : Symbol;
         Attr_Type    : Attribute_Type;

         function Find_Declaration return Integer;
         --  Return the position of the declaration for Attr_Prefix:Attr_Name
         --  in Attr, or -1 if no declaration exists

         procedure Check_Required_Attributes;
         --  Check whether all required attributes have been defined

         ----------------------
         -- Find_Declaration --
         ----------------------

         function Find_Declaration return Integer is
         begin
            if Attr /= null then
               --  First test: same prefix and local name. We will test later
               --  for a same URI

               for A in Attr'Range loop
                  if Attr (A).Local_Name = Attr_Name
                    and then Attr (A).Prefix = Attr_Prefix
                  then
                     return A;
                  end if;
               end loop;
            end if;
            return -1;
         end Find_Declaration;

         -------------------------------
         -- Check_Required_Attributes --
         -------------------------------

         procedure Check_Required_Attributes is
            Found : Boolean;
         begin
            if Parser.Feature_Validation and then Attr /= null then
               for A in Attr'Range loop
                  if Attr (A).Default_Decl = Required then
                     Found := False;

                     for T in 1 .. Parser.Attributes.Count loop
                        if Parser.Attributes.List (T).Local_Name =
                            Attr (A).Local_Name
                          and then Parser.Attributes.List (T).Prefix =
                            Attr (A).Prefix
                        then
                           Found := True;
                           exit;
                        end if;
                     end loop;

                     if not Found then
                        Error
                          (Parser, "[VC 3.3.2] Required attribute '"
                           & To_QName (Attr (A).Prefix, Attr (A).Local_Name)
                           & "' must be defined");
                     end if;
                  end if;
               end loop;
            end if;
         end Check_Required_Attributes;

      begin
         Parser.Attributes.Count := 0;

         while Id.Typ /= End_Of_Tag
           and then Id.Typ /= End_Of_Input
           and then Id.Typ /= End_Of_Start_Tag
         loop
            Get_Name_NS (Id, Attr_NS_Id, Attr_Name_Id);
            if Id.Typ = Space then
               Next_Token (Input, Parser, Id);
            end if;

            if Id.Typ /= Equal then
               Fatal_Error  --  3.1
                 (Parser, "Attributes must have an explicit value", Id);
            end if;

            Attr_Name   := Find_Symbol (Parser, Attr_Name_Id);
            Attr_Prefix := Find_Symbol (Parser, Attr_NS_Id);

            A := Find_Declaration;

            Next_Token_Skip_Spaces (Input, Parser, Id);
            if Id.Typ /= Double_String_Delimiter
              and then Id.Typ /= Single_String_Delimiter
            then
               Fatal_Error  --  3.1
                 (Parser, "Attribute values must be quoted", Id);
            end if;

            --  3.3.3: If the attribute's type is not CDATA, we must
            --  normalize it, ie collapse sequence of spaces.
            --  ??? What if the information comes from an XML Schema instead
            --  of a DTD
            --  ??? That should be done only after we have processed the
            --  namespaces, otherwise we do not know what attribute we are
            --  dealing with
            --  In XML Schema 1.1 Part 1, Section 3.1.4, it is indicated that
            --  we should always normalize attribute values according to the
            --  whitespace property of their type. As a result, we do not
            --  normalize here by default if the attribute was registered, and
            --  it will be done by the schema parser if we are using one
            --  (see Hook_Start_Element).

            Get_String
              (Id, Attr_Value_State, Value_Start, Value_End,
               Normalize       => True,
               Collapse_Spaces => A /= -1
                  and then Attr (A).Att_Type /= Cdata);

            Attr_Value := Find_Symbol (Parser, Value_Start, Value_End);
            Add_Attr   := True;

            --  Is this a namespace declaration ?

            if Parser.Feature_Namespace
              and then Attr_Prefix = Parser.Xmlns_Sequence
            then
               Check_And_Define_Namespace
                 (Prefix   => Attr_Name,
                  URI      => Attr_Value,
                  Location => Attr_Name_Id.Location);
               Add_Attr := Parser.Feature_Namespace_Prefixes;

            --  Is this the declaration of the default namespace (xmlns="uri")

            elsif Parser.Feature_Namespace
              and then Attr_NS_Id = Null_Token
              and then Attr_Name = Parser.Xmlns_Sequence
            then
               if Get (Attr_Value).all = Xmlns_URI_Sequence
                 or else Get (Attr_Value).all = Namespaces_URI_Sequence
               then
                  Fatal_Error
                    (Parser,
                     "The xml namespace cannot be declared as the default"
                     & " namespace");
               end if;

               --  We might have a FIXED declaration for this attribute in the
               --  DTD, as per the XML Conformance testsuite
               if Parser.Feature_Validation
                 and then A /= -1
               then
                  if Attr (A).Default_Decl = Fixed
                    and then Attr (A).Value /= Attr_Value
                  then
                     Error
                       (Parser,
                        "[VC 3.3.2] xmlns attribute doesn't match FIXED value",
                        Value_Start);
                  end if;
               end if;

               Check_And_Define_Namespace
                 (Prefix   => Empty_String,
                  URI      => Attr_Value,
                  Location => Attr_Name_Id.Location);
               Add_Attr := Parser.Feature_Namespace_Prefixes;

            else
               --  All attributes must be defined (including xml:lang, that
               --  requires additional testing afterwards)
               if Parser.Feature_Validation then
                  if Attr = null then
                     Error
                       (Parser, "[VC] No attribute allowed for element "
                        & Get (Parser.Current_Node.Name).all,
                        Attr_Name_Id);
                  elsif A = -1 then
                     Error
                       (Parser, "[VC] Attribute not declared in DTD: "
                        & To_QName (Attr_Prefix, Attr_Name),
                        Attr_Name_Id);
                  end if;
               end if;

               if Get_String (Attr_NS_Id) = Xml_Sequence then
                  if Get_String (Attr_Name_Id) = Lang_Sequence then
                     Test_Valid_Lang
                       (Parser, Get_String (Value_Start, Value_End));

                  elsif Get_String (Attr_Name_Id) = Space_Word_Sequence then
                     Test_Valid_Space
                       (Parser, Get_String (Value_Start, Value_End));
                  end if;
               end if;
            end if;

            --  Register the attribute in the temporary list, until we can
            --  properly resolve namespaces

            if Add_Attr then
               if Debug_Internal then
                  Put_Line
                    ("Register attribute: "
                     & Qname_From_Name (Parser, Attr_NS_Id, Attr_Name_Id)
                     & " value=" & Get_String (Value_Start, Value_End));
               end if;

               if A /= -1 then
                  if Attr (A).Default_Decl = Fixed
                    and then Attr (A).Value /= Attr_Value
                  then
                     Error
                       (Parser, "[VC 3.3.2] Fixed attribute '"
                        & To_QName (Attr_Prefix, Attr_Name)
                        & "' must have the defined value",
                        Attr_Name_Id.Location);
                  end if;

                  Attr_Type := Attr (A).Att_Type;
               else
                  Attr_Type := Cdata;
               end if;

               Add
                 (Parser     => Parser,
                  Attr       => Parser.Attributes.List,
                  Count      => Parser.Attributes.Count,
                  If_Unique  => False,
                  Location   => Attr_Name_Id.Location,
                  Local_Name => Attr_Name,
                  Prefix     => Attr_Prefix,
                  Att_Type   => Attr_Type,
                  Value      => Attr_Value);
            end if;

            if Attr_NS_Id /= Null_Token then
               Reset_Buffer (Parser, Attr_NS_Id);
            else
               Reset_Buffer (Parser, Attr_Name_Id);
            end if;

            Next_Token (Input, Parser, Id);
            if Id.Typ = Space then
               Next_Token (Input, Parser, Id);
            elsif Id.Typ /= End_Of_Tag and then Id.Typ /= End_Of_Start_Tag then
               Fatal_Error (Parser, Error_Expecting_Space, Id);
            end if;
         end loop;

         Check_Required_Attributes;

         Add_Default_Attributes (Attr);

         --  Check attribute values. We must do that after adding the default
         --  attributes, so that they are properly checked as well. It would be
         --  nice to be able to check them only once, but that can't be done
         --  when they are declared (since they might be referencing entities
         --  declared after them in the DTD)

         if Parser.Feature_Validation then
            for Att in 1 .. Parser.Attributes.Count loop
               Check_Attribute_Value
                 (Parser,
                  Local_Name => Parser.Attributes.List (Att).Local_Name,
                  Typ        => Parser.Attributes.List (Att).Att_Type,
                  Value      => Parser.Attributes.List (Att).Value,
                  Error_Loc  => Elem_Name_Id);
            end loop;
         end if;
      end Parse_Attributes;

      ---------------------
      -- Parse_Start_Tag --
      ---------------------

      procedure Parse_Start_Tag is
         Open_Id : constant Token := Id;
         Elem_Name_Id, Elem_NS_Id : Token;
         NS : XML_NS;

      begin
         Set_State (Parser, Tag_State);

         Parser.Current_Node := new Element'
           (NS             => No_XML_NS,
            Name           => No_Symbol,
            Namespaces     => No_XML_NS,
            Start          => Id.Location,
            Start_Tag_End  => Id.Location,
            Parent         => Parser.Current_Node);

         Next_Token (Input, Parser, Id);
         Get_Name_NS (Id, Elem_NS_Id, Elem_Name_Id);

         Parser.Current_Node.Name := Find_Symbol (Parser, Elem_Name_Id);

         if Parser.Current_Node.Parent = null then
            Parser.Num_Toplevel_Elements := Parser.Num_Toplevel_Elements + 1;
            if Parser.Num_Toplevel_Elements > 1 then
               Fatal_Error   --  2.1
                 (Parser, "Too many children for top-level node,"
                  & " when adding <"
                  & Qname_From_Name (Parser, Elem_NS_Id, Elem_Name_Id)
                  & ">", Open_Id);
            end if;

            if Parser.Feature_Validation then
               if Parser.DTD_Name = No_Symbol then
                  Error  --  VC 2.8
                    (Parser, "No DTD defined for this document", Id);

               elsif Parser.DTD_Name /= Parser.Current_Node.Name then
                  Error
                    (Parser, "[VC 2.8] Name of root element doesn't match name"
                     & " of DTD ('"
                     & Get (Parser.DTD_Name).all & "')", Id);
               end if;
            end if;

         elsif Parser.Feature_Validation then
            Check_Model;
         end if;

         if Elem_NS_Id /= Null_Token
           and then Get_String (Elem_NS_Id) = Xmlns_Sequence
         then
            Fatal_Error (Parser, "Elements must not have the prefix xmlns");
         end if;

         --  Call the hook before checking the attributes. This might mean we
         --  are passing incorrect attributes (or missing ones), but the hook
         --  is used for validation (otherwise standard users should use
         --  Start_Element itself).
         --  We want the count of elements in the NS to not include the current
         --  context.

         if Debug_Internal then
            Put_Line
              ("Start_Element "
               & Qname_From_Name (Parser, Elem_NS_Id, Elem_Name_Id));
         end if;

         --  We need to process the attributes first, because they might define
         --  the namespace for the element

         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
            Parse_Attributes (Elem_NS_Id, Elem_Name_Id, Id);

         elsif Id.Typ /= End_Of_Tag
           and then Id.Typ /= End_Of_Start_Tag
         then
            Fatal_Error (Parser, Error_Expecting_Space, Id);

         else
            --  We still need to check the attributes, in case we have none but
            --  some where required
            Parse_Attributes (Elem_NS_Id, Elem_Name_Id, Id);
         end if;

         Resolve_Attribute_Namespaces;

         --  And report the elements to the callbacks

         Set_State (Parser, Default_State);
         Find_NS (Parser, Elem_NS_Id, NS);

         Parser.Current_Node.NS := NS;

         if Parser.Hooks.Start_Element /= null then
            Parser.Hooks.Start_Element
              (Parser'Unchecked_Access, Parser.Current_Node,
               Parser.Attributes);
         end if;

         --  This does not take into account the use of the namespace by the
         --  attributes.
         --  ??? That would be costly to again do a Find_NS for each of the
         --  attributes. ??? We don't do a Find_NS anymore, so that would be
         --  doable in fact.
         Increment_Count (NS);

         Parser.Current_Node.Start_Tag_End := Get_Location (Parser.Locator);
         Start_Element
           (Parser,
            NS         => NS,
            Local_Name => Parser.Current_Node.Name,
            Atts       => Parser.Attributes);

         if Id.Typ = End_Of_Start_Tag then
            End_Element;
         end if;

         if Elem_NS_Id /= Null_Token then
            Reset_Buffer (Parser, Elem_NS_Id);
         else
            Reset_Buffer (Parser, Elem_Name_Id);
         end if;

         if Id.Typ = End_Of_Input then
            Fatal_Error (Parser, "Unexpected end of stream");
         end if;
      end Parse_Start_Tag;

      ----------------------------
      -- Parse_Doctype_Contents --
      ----------------------------

      procedure Parse_Doctype_Contents is
         Start_Id : Symbol;

         Num_Include : Natural := 0;
         --  Number of <![INCLUDE[ sections at the top of the external
         --  subset.

         Num_Ignore : Natural := 0;
         --  Number of <![IGNORE[ and <![INCLUDE[ sections, starting at the
         --  first ignore section.
      begin
         loop
            Next_Token_Skip_Spaces (Input, Parser, Id);
            Start_Id := Id.Location.System_Id;

            if Id.Typ = Ignore then
               Num_Ignore := Num_Ignore + 1;

            elsif Id.Typ = Include or else Id.Typ = Start_Conditional then
               if Num_Ignore > 0 then
                  Num_Ignore := Num_Ignore + 1;
               else
                  Num_Include := Num_Include + 1;
               end if;

            elsif Id.Typ = End_Conditional then
               if Num_Include + Num_Ignore = 0 then
                  Fatal_Error (Parser, Error_Unexpected_Chars3, Id);
               elsif Num_Ignore > 0 then
                  Num_Ignore := Num_Ignore - 1;
               else
                  Num_Include := Num_Include  - 1;
               end if;

            elsif Id.Typ = End_Of_Input then
               exit;

            elsif Num_Ignore = 0 then
               case Id.Typ is
                  when End_Of_Tag | Internal_DTD_End =>
                     exit;
                  when Entity_Def => Parse_Entity_Def (Id);
                  when Element_Def => Parse_Element_Def (Id);
                  when Notation => Parse_Notation_Def (Id);
                  when Attlist_Def => Parse_Attlist_Def (Id);
                  when Text | Name =>
                     if Id.First < Id.Last then
                        Fatal_Error
                          (Parser,  "Unexpected character in the DTD");
                     else
                        Reset_Buffer (Parser, Id);
                     end if;
                  when Comment =>
                     Comment (Parser, Parser.Buffer (Id.First .. Id.Last));
                     Reset_Buffer (Parser, Id);
                  when Start_Of_PI =>
                     Parse_PI (Id);
                  when others =>
                     Fatal_Error  --  2.8
                       (Parser, "Element not allowed in the DTD", Id);
               end case;

            else
               Reset_Buffer (Parser, Id);
            end if;

            --  XML 1.0 Errata 14 or XML 1.1 section 4.3.2: nesting of entities
            --  doesn't apply for well-formedness in the DTD
            if Parser.Feature_Validation then
               if Start_Id /= Id.Location.System_Id then
                  Error (Parser, Error_Entity_Self_Contained, Id);
               end if;
            end if;
         end loop;

         if Num_Ignore + Num_Include /= 0 then
            Fatal_Error  --  3.4
              (Parser, "Conditional section must be properly terminated",
               Id);
         end if;
      end Parse_Doctype_Contents;

      -------------------
      -- Parse_Doctype --
      -------------------

      procedure Parse_Doctype is
         Public_Start, Public_End : Token := Null_Token;
         System_Start, System_End : Token := Null_Token;
         Name_Id : Token;
         NS_Id : Token;
      begin
         Set_State (Parser, DTD_State);

         Next_NS_Token_Skip_Spaces (Input, Parser, NS_Id, Name_Id);

         if Name_Id.Typ /= Name then
            Fatal_Error (Parser, "Expecting name after <!DOCTYPE");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);

         Get_External (Id, System_Start, System_End, Public_Start, Public_End);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         end if;
         Start_DTD
           (Parser,
            Name => Parser.Buffer (Name_Id.First .. Name_Id.Last),
            Public_Id => Parser.Buffer (Public_Start.First .. Public_End.Last),
            System_Id =>
              Parser.Buffer (System_Start.First .. System_End.Last));

         if Parser.Feature_Validation then
            Parser.DTD_Name := Find_Symbol (Parser, Name_Id);
         end if;

         if Id.Typ = Internal_DTD_Start then
            Parse_Doctype_Contents;
            if Id.Typ /= Internal_DTD_End then
               Fatal_Error  --  2.8
                 (Parser, "Expecting end of internal subset ']>'", Id);
            end if;
         elsif Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "Expecting end of DTD");
         end if;

         --  Read the external subset if required. This needs to be read
         --  after the internal subset only, so that the latter gets
         --  priority (XML specifications 2.8)
         if System_End.Last >= System_Start.First then
            declare
               Loc : constant Sax.Locators.Location :=
                 Get_Location (Parser.Locator);
               System : constant Symbol :=
                 Find_Symbol
                   (Parser,
                    Parser.Buffer (System_Start.First .. System_End.Last));
               URI : constant Symbol :=
                 Resolve_URI (Parser, System_Id (Parser), System);
               In_External : constant Boolean := Parser.In_External_Entity;
               Input_F : File_Input;
               Saved_Last_Read : constant Unicode_Char := Parser.Last_Read;
            begin
               Open (Get (URI).all, Input_F);

               --  Protect against the case where the last character read was
               --  a LineFeed.
               Parser.Last_Read := Unicode_Char'Val (16#00#);
               Parser.Last_Read_Is_Valid := False;

               Set_Line_Number (Parser.Locator, 1);
               Set_Column_Number (Parser.Locator, Prolog_Size (Input_F));
               Set_System_Id (Parser.Locator, URI);
               Set_Public_Id (Parser.Locator, System);

               if NS_Id /= Null_Token then
                  Reset_Buffer (Parser, NS_Id);
               else
                  Reset_Buffer (Parser, Name_Id);
               end if;

               Parser.In_External_Entity := True;

               Syntactic_Parse (Parser, Input_F);
               Close (Input_F);
               Parser.In_External_Entity := In_External;

               Set_Location (Parser.Locator, Loc);
               Parser.Last_Read := Saved_Last_Read;
               Parser.Last_Read_Is_Valid := True;
            exception
               when Name_Error =>
                  Close (Input_F);
                  Error
                    (Parser,
                     "External subset not found: "
                     & Parser.Buffer (System_Start.First .. System_End.Last),
                     Id);

                  if NS_Id /= Null_Token then
                     Reset_Buffer (Parser, NS_Id);
                  else
                     Reset_Buffer (Parser, Name_Id);
                  end if;

               when others =>
                  Close (Input_F);
                  raise;
            end;

         else
            if NS_Id /= Null_Token then
               Reset_Buffer (Parser, NS_Id);
            else
               Reset_Buffer (Parser, Name_Id);
            end if;
         end if;

         --  Check that all declarations are fully declared
         if Parser.Feature_Validation then
            declare
               Iter : Notations_Table.Iterator := First (Parser.Notations);
            begin
               while Iter /= Notations_Table.No_Iterator loop
                  if not Current (Iter).Declaration_Seen then
                     Error (Parser, Error_Notation_Undeclared
                            & Get (Current (Iter).Name).all);
                  end if;
                  Next (Parser.Notations, Iter);
               end loop;
            end;
         end if;

         Parser.In_External_Entity := False;
         End_DTD (Parser);
         Set_State (Parser, Default_State);
      end Parse_Doctype;

      -----------------
      -- End_Element --
      -----------------

      procedure End_Element is
      begin
         if Parser.Hooks.End_Element /= null then
            Parser.Hooks.End_Element
              (Parser'Unchecked_Access, Parser.Current_Node);
         end if;

         End_Element
           (Parser, NS => Parser.Current_Node.NS,
            Local_Name => Parser.Current_Node.Name);

         --  Tag must end in the same entity
         if Parser.Feature_Validation
           and then
             Id.Location.System_Id /= Parser.Current_Node.Start.System_Id
         then
            Error (Parser, Error_Entity_Self_Contained, Id);
         end if;

         Close_Namespaces (Parser, Parser.Current_Node.Namespaces);

         --  Move back to the parent node (after freeing the current node)
         Free (Parser.Current_Node);
      end End_Element;

      -------------------
      -- Parse_End_Tag --
      -------------------

      procedure Parse_End_Tag is
         Open_Id : constant Token := Id;
         NS_Id, Name_Id : Token := Null_Token;
      begin
         Set_State (Parser, Tag_State);

         Next_Token (Input, Parser, Id);
         Get_Name_NS (Id, NS_Id, Name_Id);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         end if;

         if Id.Typ /= End_Of_Tag then
            Fatal_Error (Parser, "Tags must end with a '>' symbol", Id);
            --  3.1
         end if;

         if Parser.Current_Node = null then
            Fatal_Error --  3
              (Parser, "No start tag found for this end tag", Id);
         end if;

         --  Tag must end in the same entity
         if Parser.Feature_Validation
           and then Id.Location.System_Id /=
             Parser.Current_Node.Start.System_Id
         then
            Error (Parser, Error_Entity_Self_Contained, Id);
         end if;

         if Parser.Current_Node = null then
            Fatal_Error
              (Parser,  --  WF element type match
               "Unexpected closing tag", Open_Id);

         elsif Parser.Buffer (NS_Id.First .. NS_Id.Last) /=
           Get (Get_Prefix (Parser.Current_Node.NS)).all
           or else Parser.Buffer (Name_Id.First .. Name_Id.Last) /=
           Get (Parser.Current_Node.Name).all
         then
            --  Well-Formedness Constraint: Element Type Match
            if Get_Prefix (Parser.Current_Node.NS) /= Empty_String then
               Fatal_Error
                 (Parser,  --  WF element type match
                  "Name differ for closing tag (expecting "
                  & Get (Get_Prefix (Parser.Current_Node.NS)).all
                  & ':' & Get (Parser.Current_Node.Name).all
                  & ", opened line"
                  & Integer'Image (Parser.Current_Node.Start.Line)
                  & ')',
                  Open_Id);
            else
               Fatal_Error
                 (Parser, --  WF element type match
                  "Name differ for closing tag ("
                  & "expecting " & Get (Parser.Current_Node.Name).all
                  & ", opened line"
                  & Integer'Image (Parser.Current_Node.Start.Line)
                  & ')',
                  Open_Id);
            end if;
         end if;

         End_Element;

         Set_State (Parser, Default_State);
         if NS_Id /= Null_Token then
            Reset_Buffer (Parser, NS_Id);
         else
            Reset_Buffer (Parser, Name_Id);
         end if;
      end Parse_End_Tag;

      -------------------------
      -- Check_Version_Value --
      -------------------------

      procedure Check_Version_Value (Id : in out Token) is
         C : Unicode_Char;
         J : Natural;
         Value_Start, Value_End : Token;
         Tmp_Version : XML_Versions;
      begin
         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Equal then
            Fatal_Error (Parser, "Expecting '=' sign", Id);
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Double_String_Delimiter
           and then Id.Typ /= Single_String_Delimiter
         then
            Fatal_Error (Parser, "Expecting version value", Id);
         end if;
         Get_String (Id, Attr_Value_State, Value_Start, Value_End);

         J := Value_Start.First;
         while J <= Value_End.Last loop
            Encoding.Read (Parser.Buffer.all, J, C);
            if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
              and then
                 not (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
              and then not (C in Digit_Zero .. Digit_Nine)
              and then C /= Low_Line
              and then C /= Period
              and then C /= Unicode.Names.Basic_Latin.Colon
              and then C /= Hyphen_Minus
            then
               Fatal_Error  --  2.8
                 (Parser, "Illegal version number in <?xml?> processing"
                  & " instruction", Value_Start);
            end if;
         end loop;

         if Parser.Buffer (Value_Start.First .. Value_End.Last) = "1.1" then
            Tmp_Version := XML_1_1;

         elsif Parser.Buffer (Value_Start.First .. Value_End.Last) = "1.0" then
            Tmp_Version := XML_1_0;

         else
            case Parser.XML_Version is
               when XML_1_0_Third_Edition
                  | XML_1_0_Fourth_Edition =>
                  Error
                    (Parser, "Unsupported version of XML: "
                     & Parser.Buffer (Value_Start.First .. Value_End.Last));

               when XML_1_0_Fifth_Edition
                  | XML_1_0
                  | XML_1_1 =>
                  null;
            end case;
         end if;

         if Parser.In_External_Entity
           and then
             ((Tmp_Version = XML_1_1
               and then Parser.XML_Version /= XML_1_1)
              or else
                (Tmp_Version /= XML_1_1
                 and then Parser.XML_Version = XML_1_1))
         then
            Fatal_Error
              (Parser,
               "External entity doesn't have the same"
               & " XML version as document");
         end if;

         --  Override the version in the parser, but only if the one set
         --  doesn't match yet. In particular, this allows users to set their
         --  preferred edition of XML 1.0

         if Tmp_Version = XML_1_1
           and then Parser.XML_Version /= XML_1_1
         then
            Parser.XML_Version := XML_1_1;
         elsif Tmp_Version = XML_1_0
           and then Parser.XML_Version = XML_1_1
         then
            Parser.XML_Version := XML_1_0;
         end if;

         Next_Token (Input, Parser, Id);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         elsif Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "values must be separated by spaces", Id);
         end if;
      end Check_Version_Value;

      --------------------------
      -- Check_Encoding_Value --
      --------------------------

      procedure Check_Encoding_Value (Id : in out Token) is
         Inp : Input_Source_Access := Input'Unchecked_Access;
         C : Unicode_Char;
         J : Natural;
         Value_Start, Value_End : Token;
         Tmp : Positive;
      begin
         --  If we are parsing an external entity, everything applies to it.
         --  See test xmltest/valid/ext-sa/008.xml
         if Parser.Inputs /= null then
            Inp := Parser.Inputs.Input;
         end if;

         Next_Token_Skip_Spaces (Inp.all, Parser, Id);
         if Id.Typ /= Equal then
            Fatal_Error (Parser, "Expecting '=' sign");
         end if;

         Next_Token_Skip_Spaces (Inp.all, Parser, Id);
         if Id.Typ /= Double_String_Delimiter
           and then Id.Typ /= Single_String_Delimiter
         then
            Fatal_Error (Parser, "Expecting encoding value");
         end if;
         Get_String (Id, Attr_Value_State, Value_Start, Value_End);

         if Value_End.Last < Value_Start.First then
            Fatal_Error   --  4.3.3
              (Parser, "Empty value for encoding not allowed");
         else
            Tmp := Value_Start.First;
            Encoding.Read (Parser.Buffer.all, Tmp, C);
            if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
              and then not
                (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
            then
               Fatal_Error   --  4.3.3
                 (Parser, "Illegal character '"
                  & Debug_Encode (C) & "' in encoding value", Value_Start);
            end if;

            J := Value_Start.First + Encoding.Width (C);
            while J <= Value_End.Last loop
               Encoding.Read (Parser.Buffer.all, J, C);
               if not (C in Latin_Small_Letter_A .. Latin_Small_Letter_Z)
                 and then not
                   (C in Latin_Capital_Letter_A .. Latin_Capital_Letter_Z)
                 and then not (C in Digit_Zero .. Digit_Nine)
                 and then C /= Period
                 and then C /= Low_Line
                 and then C /= Hyphen_Minus
               then
                  Fatal_Error  --  4.3.3
                    (Parser, "Illegal character '"
                     & Debug_Encode (C) & "' in encoding value",
                     Value_Start);
               end if;
            end loop;
         end if;

         --  Check we indeed have a following space

         Next_Token (Inp.all, Parser, Id);
         if Id.Typ = Space then
            Next_Token (Inp.all, Parser, Id);
         elsif Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "values must be separated by spaces", Id);
         end if;

         --  Change the encoding for the streams, if needed
         Set_Stream_Encoding
           (Inp.all, Parser.Buffer (Value_Start.First .. Value_End.Last));
      end Check_Encoding_Value;

      ----------------------------
      -- Check_Standalone_Value --
      ----------------------------

      procedure Check_Standalone_Value (Id : in out Token) is
         Value_Start, Value_End : Token;
      begin
         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Equal then
            Fatal_Error (Parser, "Expecting '=' sign");
         end if;

         Next_Token_Skip_Spaces (Input, Parser, Id);
         if Id.Typ /= Double_String_Delimiter
           and then Id.Typ /= Single_String_Delimiter
         then
            Fatal_Error
              (Parser, "Parameter to 'standalone' must be quoted", Id);
         end if;
         Get_String (Id, Attr_Value_State, Value_Start, Value_End);

         if Parser.Buffer (Value_Start.First .. Value_End.Last) /= Yes_Sequence
           and then Parser.Buffer (Value_Start.First .. Value_End.Last) /=
             No_Sequence
         then
            Fatal_Error
              (Parser,   --  2.9 [32]
               "Invalid value for standalone parameter in <?xml?>",
               Value_Start);
         end if;

         Parser.Standalone_Document :=
           Parser.Buffer (Value_Start.First .. Value_End.Last) =
           Yes_Sequence;

         Next_Token (Input, Parser, Id);
         if Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         elsif Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "values must be separated by spaces", Id);
         end if;
      end Check_Standalone_Value;

      --------------
      -- Parse_PI --
      --------------

      procedure Parse_PI (Id : in out Token) is
         State : constant Parser_State := Get_State (Parser);
         Open_Id : constant Token := Id;
         Name_Id, Data_Start : Token;
         Data_End : Token := Null_Token;
      begin
         Set_State (Parser, PI_State);

         Next_Token (Input, Parser, Name_Id);
         if Name_Id.Typ /= Name then
            Fatal_Error
              (Parser,  --  2.6
               "Processing Instruction must specify a target name",
               Name_Id);
         end if;

         Check_Valid_Name_Or_NCname (Parser, Name_Id);

         Next_Token (Input, Parser, Id);
         if Id.Typ /= Space and then Id.Typ /= End_Of_PI then
            Fatal_Error (Parser, "Must have space between target and data");
         elsif Id.Typ = Space then
            Next_Token (Input, Parser, Id);
         end if;

         --  Special handling for <?xml?>
         if Parser.Buffer (Name_Id.First .. Name_Id.Last) = Xml_Sequence then

            if Open_Id.Location.Line /= 1
              or else
                (Parser.Inputs = null
                 and then Open_Id.Location.Column /= 1 + Prolog_Size (Input))
              or else
                (Parser.Inputs /= null
                 and then Open_Id.Location.Column /=
                   1 + Prolog_Size (Parser.Inputs.Input.all))
              or else (Parser.Inputs /= null
                       and then not Parser.Inputs.External)
            then
               Fatal_Error
                 (Parser,   --  2.8
                  "<?xml?> instruction must be first in document",
                  Open_Id);
            end if;

            --  ??? No true for text declaratinos 4.3.1 (external parsed
            --  entities)
            Set_State (Parser, Tag_State);

            if Parser.Buffer (Id.First .. Id.Last) = Version_Sequence then
               Check_Version_Value (Id);
            elsif not Parser.In_External_Entity then
               Fatal_Error
                 (Parser, "'version' must be the first argument to <?xml?>",
                  Id);
            end if;

            if Id.Typ = Name
              and then Parser.Buffer (Id.First .. Id.Last) = Encoding_Sequence
            then
               Check_Encoding_Value (Id);
            elsif Parser.In_External_Entity then
               Fatal_Error
                 (Parser, "'encoding' must be specified for <?xml?> in"
                  & " external entities", Id);
            end if;

            if not Parser.In_External_Entity
              and then Id.Typ = Name
              and then Parser.Buffer (Id.First .. Id.Last) =
                Standalone_Sequence
            then
               Check_Standalone_Value (Id);
            end if;

            if Id.Typ /= End_Of_PI then
               if Parser.In_External_Entity then
                  Fatal_Error
                    (Parser,
                     "Text declarations <?xml?> in external entity cannot"
                     & " specify parameters other than 'version' and"
                     & " 'encoding'", Id);
               else
                  Fatal_Error
                    (Parser,
                     "<?xml..?> arguments can only be 'version', 'encoding' or"
                     & " 'standalone', in that order", Id);
               end if;
            end if;

         else
            --  (2.6)[17]: Name can not be 'xml' (case insensitive)
            declare
               C : Unicode_Char;
               J : Natural := Name_Id.First;
            begin
               Encoding.Read (Parser.Buffer.all, J, C);

               if C = Latin_Small_Letter_X
                 or else C = Latin_Capital_Letter_X
               then
                  Encoding.Read (Parser.Buffer.all, J, C);

                  if C = Latin_Capital_Letter_M
                    or else C = Latin_Small_Letter_M
                  then
                     Encoding.Read (Parser.Buffer.all, J, C);

                     if (C = Latin_Capital_Letter_L
                         or else C = Latin_Small_Letter_L)
                       and then J = Name_Id.Last + 1
                     then
                        Fatal_Error
                          (Parser,   --  2.6
                           "'"
                           & Parser.Buffer (Name_Id.First .. Name_Id.Last)
                           & "' is not a valid processing instruction target",
                           Name_Id);
                     end if;
                  end if;
               end if;
            end;

            Data_Start := Id;

            while Id.Typ /= End_Of_PI and then Id.Typ /= End_Of_Input loop
               Data_End := Id;

               if Id.Typ = Double_String_Delimiter then
                  Put_In_Buffer (Parser, """");
                  Data_End.Last := Data_End.Last + 1;
               elsif Id.Typ = Single_String_Delimiter then
                  Put_In_Buffer (Parser, "'");
                  Data_End.Last := Data_End.Last + 1;
               end if;

               Next_Token (Input, Parser, Id);
            end loop;

            if Id.Typ = End_Of_Input then
               Fatal_Error  --  2.6
                 (Parser, "Processing instruction must end with '?>'",
                  Open_Id);
            end if;

            Processing_Instruction
              (Parser,
               Target => Parser.Buffer (Name_Id.First .. Name_Id.Last),
               Data   => Parser.Buffer (Data_Start.First .. Data_End.Last));
         end if;

         Set_State (Parser, State);
         Reset_Buffer (Parser, Name_Id);
      end Parse_PI;

   begin
      --  Initialize the parser with the first character of the stream.
      if Eof (Input) then
         return;
      end if;
      Next_Char (Input, Parser);

      if Parser.State.In_DTD then
         Parse_Doctype_Contents;
      end if;

      loop
         --  Unless in string, buffer should be empty at this point. Strings
         --  are special-cased just in case we are currently substituting
         --  entities while in a string.
         pragma Assert (Parser.State.Ignore_Special
                        or else Parser.Buffer_Length = 0);

         Next_Token (Input, Parser, Id,
                     Coalesce_Space => Parser.Current_Node /= null);
         exit when Id.Typ = End_Of_Input;

         case Id.Typ is
            when Start_Of_PI =>
               Parse_PI (Id);

            when Cdata_Section =>
               if Parser.Current_Node = null then
                  Fatal_Error  --  2.1
                    (Parser, "Non-white space found at top level", Id);
               end if;
               Start_Cdata (Parser);

               if Parser.Hooks.Characters /= null then
                  Parser.Hooks.Characters
                    (Parser'Unchecked_Access,
                     Parser.Buffer (Id.First .. Id.Last));
               end if;

               Characters (Parser, Parser.Buffer (Id.First .. Id.Last));
               End_Cdata (Parser);
               Reset_Buffer (Parser, Id);

            when Text | Name =>
               if Parser.Current_Node = null then
                  Fatal_Error  --  2.1
                    (Parser, "Non-white space found at top level", Id);
               end if;

               if Parser.Hooks.Characters /= null then
                  Parser.Hooks.Characters
                    (Parser'Unchecked_Access,
                     Parser.Buffer (Id.First .. Id.Last));
               end if;

               Characters (Parser, Parser.Buffer (Id.First .. Id.Last));
               Reset_Buffer (Parser, Id);

            when Sax.Readers.Space =>
               --   If "xml:space" attribute is preserve
               --   then same as Text

               if Parser.Hooks.Whitespace /= null then
                  Parser.Hooks.Whitespace
                    (Parser'Unchecked_Access,
                     Parser.Buffer (Id.First .. Id.Last));
               end if;

               Ignorable_Whitespace
                 (Parser, Parser.Buffer (Id.First .. Id.Last));
               Reset_Buffer (Parser, Id);

            when Comment =>
               Comment (Parser, Parser.Buffer (Id.First .. Id.Last));
               Reset_Buffer (Parser, Id);

            when Start_Of_Tag =>
               Parse_Start_Tag;

            when Start_Of_End_Tag =>
               Parse_End_Tag;

            when Doctype_Start =>
               Parse_Doctype;

            when others =>
               Fatal_Error (Parser, "Currently ignored: "
                            & Token_Type'Image (Id.Typ));
         end case;
      end loop;
   end Syntactic_Parse;

   ----------
   -- Free --
   ----------

   procedure Free (Parser : in out Sax_Reader'Class) is
      Tmp, Tmp2 : Element_Access;
   begin
      Close_Inputs (Parser, Parser.Inputs);
      Close_Inputs (Parser, Parser.Close_Inputs);

      Free (Parser.Default_Namespaces);
      Free (Parser.Buffer);
      Parser.Buffer_Length := 0;

      Parser.Attributes.Count := 0;
      Unchecked_Free (Parser.Attributes.List);

      --  Free the nodes, in case there are still some open
      Tmp := Parser.Current_Node;
      while Tmp /= null loop
         Tmp2 := Tmp.Parent;
         Free (Tmp);
         Tmp := Tmp2;
      end loop;

      --  Free the content model for the default attributes
      --  is done automatically when the attributes are reset

      if Parser.Hooks.Data /= null then
         Free (Parser.Hooks.Data.all);
         Unchecked_Free (Parser.Hooks.Data);
      end if;

      --  Free the internal tables
      Reset (Parser.Entities);
      Reset (Parser.Default_Atts);
      Reset (Parser.Notations);

      Free (Parser.Locator);
   end Free;

   ---------------
   -- Set_Hooks --
   ---------------

   procedure Set_Hooks
     (Handler        : in out Sax_Reader;
      Data           : Hook_Data_Access  := null;
      Start_Element  : Start_Element_Hook := null;
      End_Element    : End_Element_Hook   := null;
      Characters     : Characters_Hook    := null;
      Whitespace     : Whitespace_Hook    := null;
      Doc_Locator    : Set_Doc_Locator_Hook := null;
      Notation_Decl  : Notation_Decl_Hook := null) is
   begin
      if Handler.Hooks.Data /= null then
         Free (Handler.Hooks.Data.all);
         Unchecked_Free (Handler.Hooks.Data);
      end if;

      Handler.Hooks :=
        (Data           => Data,
         Start_Element  => Start_Element,
         End_Element    => End_Element,
         Characters     => Characters,
         Whitespace     => Whitespace,
         Doc_Locator    => Doc_Locator,
         Notation_Decl  => Notation_Decl);
   end Set_Hooks;

   ------------------------
   -- Initialize_Symbols --
   ------------------------

   procedure Initialize_Symbols (Parser : in out Sax_Reader) is
   begin
      if Parser.Lt_Sequence = No_Symbol then
         if Get (Parser.Symbols) = null then
            if Debug_Internal then
               Put_Line ("Initialize_Symbols: creating new table");
            end if;
            Parser.Symbols := Sax.Utils.Allocate;
         end if;

         Parser.Lt_Sequence    := Find_Symbol (Parser, Lt_Sequence);
         Parser.Gt_Sequence    := Find_Symbol (Parser, Gt_Sequence);
         Parser.Amp_Sequence   := Find_Symbol (Parser, Amp_Sequence);
         Parser.Apos_Sequence  := Find_Symbol (Parser, Apos_Sequence);
         Parser.Quot_Sequence  := Find_Symbol (Parser, Quot_Sequence);
         Parser.Xmlns_Sequence := Find_Symbol (Parser, Xmlns_Sequence);
         Parser.Xml_Sequence   := Find_Symbol (Parser, Xml_Sequence);
         Parser.Symbol_Percent := Find_Symbol (Parser, "%");
         Parser.Symbol_Ampersand := Find_Symbol (Parser, "&");
         Parser.Namespaces_URI_Sequence :=
           Find_Symbol (Parser, Namespaces_URI_Sequence);
      end if;
   end Initialize_Symbols;

   ----------------------
   -- Close_Namespaces --
   ----------------------

   procedure Close_Namespaces
     (Parser : in out Sax_Reader'Class; List : XML_NS)
   is
      NS : XML_NS := List;
   begin
      while NS /= No_XML_NS loop
         if Get_Prefix (NS) /= Empty_String
           and then Get_Prefix (NS) /= Parser.Xmlns_Sequence
         then
            End_Prefix_Mapping (Parser, Get_Prefix (NS));
         end if;
         NS := Next_In_List (NS);
      end loop;
   end Close_Namespaces;

   -----------
   -- Parse --
   -----------

   procedure Parse
     (Parser : in out Sax_Reader;
      Input  : in out Input_Sources.Input_Source'Class) is
   begin
      Initialize_Symbols (Parser);

      Parser.Locator := Sax.Locators.Create;
      Parser.Public_Id := Find_Symbol (Parser, Get_Public_Id (Input));
      Set_Public_Id (Parser.Locator, Parser.Public_Id);
      Parser.System_Id := Find_Symbol (Parser, Get_System_Id (Input));
      Set_System_Id (Parser.Locator, Parser.System_Id);
      Set_Column_Number (Parser.Locator, Prolog_Size (Input));
      Set_Line_Number (Parser.Locator, 1);
      Parser.Lookup_Char := Unicode.Unicode_Char'Last;
      Parser.Current_Node := null;
      Parser.Num_Toplevel_Elements := 0;
      Parser.Previous_Char_Was_CR := False;
      Parser.Ignore_State_Special := False;
      Parser.In_External_Entity := False;
      Parser.Last_Read_Is_Valid := False;
      Parser.Buffer := new Byte_Sequence (1 .. Initial_Buffer_Length);
      Set_State (Parser, Default_State);

      Add_Namespace_No_Event
        (Parser,
         Prefix => Parser.Xml_Sequence,
         URI    => Find_Symbol
           (Parser,
            Encodings.From_Utf32
              (Basic_8bit.To_Utf32 ("http://www.w3.org/XML/1998/namespace"))));
      Add_Namespace_No_Event
        (Parser, Parser.Xmlns_Sequence, Parser.Xmlns_Sequence);
      Add_Namespace_No_Event (Parser, Empty_String, Empty_String);

      if Parser.Hooks.Doc_Locator /= null then
         Parser.Hooks.Doc_Locator (Parser, Parser.Locator);
      end if;

      Set_Document_Locator (Sax_Reader'Class (Parser), Parser.Locator);

      Start_Document (Sax_Reader'Class (Parser));
      Syntactic_Parse (Sax_Reader'Class (Parser), Input);
      Close_Namespaces (Parser, Parser.Default_Namespaces);

      --  All the nodes must have been closed at the end of the document
      if Parser.Current_Node /= null then
         Fatal_Error   --  2.1
           (Parser, "Node <" & Get (Parser.Current_Node.Name).all
            & "> is not closed");
      end if;

      if Parser.Num_Toplevel_Elements = 0 then
         Fatal_Error (Parser, "No root element specified"); --  2.1
      end if;

      End_Document (Sax_Reader'Class (Parser));

      Free (Parser);

   exception
      when others =>
         Free (Parser);
         raise;
   end Parse;

   ----------
   -- Hash --
   ----------

   function Hash (Str : String) return Unsigned_32 is
      Result : Unsigned_32 := Str'Length;
   begin
      for J in Str'Range loop
         Result := Rotate_Left (Result, 1) +
           Unsigned_32 (Character'Pos (Str (J)));
      end loop;

      return Result;
   end Hash;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Entity : Entity_Entry_Access) return Symbol is
   begin
      return Entity.Name;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (Att : in out Attributes_Entry) is
   begin
      Unchecked_Free (Att.Attributes);
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Att : Attributes_Entry) return Symbol is
   begin
      return Att.Element_Name;
   end Get_Key;

   ----------
   -- Free --
   ----------

   procedure Free (Notation : in out Notation_Entry) is
      pragma Unreferenced (Notation);
   begin
      null;
   end Free;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Notation : Notation_Entry) return Symbol is
   begin
      return Notation.Name;
   end Get_Key;

   -----------------
   -- Get_Feature --
   -----------------

   function Get_Feature (Parser : Sax_Reader; Name : String) return Boolean is
   begin
      if Name = Namespace_Feature then
         return Parser.Feature_Namespace;

      elsif Name = Namespace_Prefixes_Feature then
         return Parser.Feature_Namespace_Prefixes;

      elsif Name = External_General_Entities_Feature then
         return Parser.Feature_External_General_Entities;

      elsif Name = External_Parameter_Entities_Feature then
         return Parser.Feature_External_Parameter_Entities;

      elsif Name = Validation_Feature then
         return Parser.Feature_Validation;

      elsif Name = Parameter_Entities_Feature then
         return False;  --  ??? Unsupported for now

      elsif Name = Test_Valid_Chars_Feature then
         return Parser.Feature_Test_Valid_Chars;

      elsif Name = Schema_Validation_Feature then
         return Parser.Feature_Schema_Validation;
      end if;

      return False;
   end Get_Feature;

   -----------------
   -- Set_Feature --
   -----------------

   procedure Set_Feature
     (Parser : in out Sax_Reader; Name : String; Value : Boolean) is
   begin
      if Name = Namespace_Feature then
         Parser.Feature_Namespace := Value;

      elsif Name = Namespace_Prefixes_Feature then
         Parser.Feature_Namespace_Prefixes := Value;

      elsif Name = External_General_Entities_Feature then
         Parser.Feature_External_General_Entities := Value;

      elsif Name = External_Parameter_Entities_Feature then
         Parser.Feature_External_Parameter_Entities := Value;

      elsif Name = Validation_Feature then
         Parser.Feature_Validation := Value;

      elsif Name = Test_Valid_Chars_Feature then
         Parser.Feature_Test_Valid_Chars := Value;

      elsif Name = Schema_Validation_Feature then
         Parser.Feature_Schema_Validation := Value;
      end if;
   end Set_Feature;

   -----------------
   -- Fatal_Error --
   -----------------

   procedure Fatal_Error
     (Handler : in out Sax_Reader; Except : Sax_Parse_Exception'Class)
   is
      pragma Warnings (Off, Handler);
   begin
      Raise_Exception
        (XML_Fatal_Error'Identity,
         Get_Message (Except));
   end Fatal_Error;

   --------------------------
   -- Start_Prefix_Mapping --
   --------------------------

   procedure Start_Prefix_Mapping
     (Handler : in out Reader;
      Prefix  : Sax.Symbols.Symbol;
      URI     : Sax.Symbols.Symbol)
   is
   begin
      Start_Prefix_Mapping
        (Reader'Class (Handler), Get (Prefix).all, Get (URI).all);
   end Start_Prefix_Mapping;

   ------------------------
   -- End_Prefix_Mapping --
   ------------------------

   procedure End_Prefix_Mapping (Handler : in out Reader; Prefix : Symbol) is
   begin
      End_Prefix_Mapping
        (Reader'Class (Handler), Get (Prefix).all);
   end End_Prefix_Mapping;

   -------------------
   -- Start_Element --
   -------------------

   procedure Start_Element
     (Handler       : in out Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol;
      Atts          : Sax_Attribute_List)
   is
      Attributes : Sax.Attributes.Attributes := Create_Attribute_List (Atts);
   begin
      Start_Element
        (Reader'Class (Handler),
         Namespace_URI => Get (Get_URI (NS)).all,
         Local_Name    => Get (Local_Name).all,
         Qname         => Qname_From_Name (Get_Prefix (NS), Local_Name),
         Atts          => Attributes);
      Clear (Attributes);

   exception
      when others =>
         Clear (Attributes);
         raise;
   end Start_Element;

   -----------------
   -- End_Element --
   -----------------

   procedure End_Element
     (Handler       : in out Reader;
      NS            : Sax.Utils.XML_NS;
      Local_Name    : Sax.Symbols.Symbol) is
   begin
      End_Element
        (Reader'Class (Handler),
         Namespace_URI => Get (Get_URI (NS)).all,
         Local_Name    => Get (Local_Name).all,
         Qname         => Qname_From_Name (Get_Prefix (NS), Local_Name));
   end End_Element;

   --------------------
   -- Skipped_Entity --
   --------------------

   procedure Skipped_Entity
     (Handler : in out Reader;
      Name    : Sax.Symbols.Symbol) is
   begin
      Skipped_Entity (Reader'Class (Handler), Get (Name).all);
   end Skipped_Entity;

   ------------------
   -- Start_Entity --
   ------------------

   procedure Start_Entity
     (Handler : in out Reader;
      Name    : Sax.Symbols.Symbol) is
   begin
      Start_Entity (Reader'Class (Handler), Get (Name).all);
   end Start_Entity;

   ----------------
   -- End_Entity --
   ----------------

   procedure End_Entity
     (Handler : in out Reader;
      Name    : Sax.Symbols.Symbol) is
   begin
      End_Entity (Reader'Class (Handler), Get (Name).all);
   end End_Entity;

   --------------------
   -- Resolve_Entity --
   --------------------

   function Resolve_Entity
     (Handler   : Sax_Reader;
      Public_Id : Unicode.CES.Byte_Sequence;
      System_Id : Unicode.CES.Byte_Sequence)
      return Input_Sources.Input_Source_Access
   is
      pragma Warnings (Off, Handler);
      pragma Warnings (Off, Public_Id);
      pragma Warnings (Off, System_Id);
   begin
      return null;
   end Resolve_Entity;

   --------------------
   -- Get_Hooks_Data --
   --------------------

   function Get_Hooks_Data (Handler : Sax_Reader) return Hook_Data_Access is
   begin
      return Handler.Hooks.Data;
   end Get_Hooks_Data;

   ------------------------------------
   -- Use_Basename_In_Error_Messages --
   ------------------------------------

   procedure Use_Basename_In_Error_Messages
     (Parser       : in out Sax_Reader;
      Use_Basename : Boolean := True)
   is
   begin
      Parser.Basename_In_Messages := Use_Basename;
   end Use_Basename_In_Error_Messages;

   ------------------------------------
   -- Use_Basename_In_Error_Messages --
   ------------------------------------

   function Use_Basename_In_Error_Messages
     (Parser : Sax_Reader) return Boolean is
   begin
      return Parser.Basename_In_Messages;
   end Use_Basename_In_Error_Messages;

   ------------
   -- Get_NS --
   ------------

   function Get_NS (Elem : Element_Access) return XML_NS is
   begin
      return Elem.NS;
   end Get_NS;

   --------------------
   -- Get_Local_Name --
   --------------------

   function Get_Local_Name (Elem : Element_Access) return Symbol is
   begin
      return Elem.Name;
   end Get_Local_Name;

   --------------
   -- To_QName --
   --------------

   function To_QName
     (Namespace_URI, Local_Name : Sax.Symbols.Symbol)
      return Unicode.CES.Byte_Sequence is
   begin
      if Namespace_URI = Empty_String then
         return Get (Local_Name).all;
      else
         return '{' & Get (Namespace_URI).all & '}' & Get (Local_Name).all;
      end if;
   end To_QName;

   --------------
   -- To_QName --
   --------------

   function To_QName
     (Elem : Element_Access) return Unicode.CES.Byte_Sequence is
   begin
      return To_QName (Get_URI (Elem.NS), Elem.Name);
   end To_QName;

   ----------------------
   -- Set_Symbol_Table --
   ----------------------

   procedure Set_Symbol_Table
     (Parser  : in out Sax_Reader;
      Symbols : Symbol_Table) is
   begin
      Parser.Lt_Sequence := No_Symbol;
      Parser.Symbols := Symbols;
   end Set_Symbol_Table;

   ----------------------
   -- Get_Symbol_Table --
   ----------------------

   function Get_Symbol_Table (Parser : Sax_Reader'Class) return Symbol_Table is
   begin
      return Parser.Symbols;
   end Get_Symbol_Table;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (List : Sax_Attribute_List;
      URI  : Sax.Symbols.Symbol;
      Local_Name : Sax.Symbols.Symbol) return Integer is
   begin
      for A in 1 .. List.Count loop
         if Get_URI (List.List (A).NS) = URI
           and then List.List (A).Local_Name = Local_Name
         then
            return A;
         end if;
      end loop;
      return -1;
   end Get_Index;

   ---------------
   -- Get_Index --
   ---------------

   function Get_Index
     (Handler    : Sax_Reader'Class;
      List       : Sax_Attribute_List;
      URI        : Unicode.CES.Byte_Sequence;
      Local_Name : Unicode.CES.Byte_Sequence) return Integer is
   begin
      return Get_Index
        (List,
         URI        => Find_Symbol (Handler, URI),
         Local_Name => Find_Symbol (Handler, Local_Name));
   end Get_Index;

   ---------------
   -- Get_Value --
   ---------------

   function Get_Value
     (List : Sax_Attribute_List; Index : Integer) return Sax.Symbols.Symbol is
   begin
      if Index < 0 then
         return No_Symbol;
      else
         return List.List (Index).Value;
      end if;
   end Get_Value;

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value
     (List  : Sax_Attribute_List;
      Index : Integer;
      Val   : Sax.Symbols.Symbol) is
   begin
      List.List (Index).Value := Val;
   end Set_Value;

   ------------------
   -- Get_Location --
   ------------------

   function Get_Location
     (List : Sax_Attribute_List; Index : Integer) return Sax.Locators.Location
   is
   begin
      if Index < 0 then
         return No_Location;
      else
         return List.List (Index).Location;
      end if;
   end Get_Location;

   ------------------------
   -- Start_Tag_Location --
   ------------------------

   function Start_Tag_Location
     (Elem : Element_Access) return Sax.Locators.Location is
   begin
      return Elem.Start;
   end Start_Tag_Location;

   ----------------------------
   -- Start_Tag_End_Location --
   ----------------------------

   function Start_Tag_End_Location
     (Elem : Element_Access) return Sax.Locators.Location is
   begin
      return Elem.Start_Tag_End;
   end Start_Tag_End_Location;

   ------------------------------
   -- Get_Non_Normalized_Value --
   ------------------------------

   function Get_Non_Normalized_Value
     (List : Sax_Attribute_List; Index : Integer) return Sax.Symbols.Symbol is
   begin
      return List.List (Index).Non_Normalized_Value;
   end Get_Non_Normalized_Value;

   --------------------------
   -- Get_Value_As_Boolean --
   --------------------------

   function Get_Value_As_Boolean
     (List : Sax_Attribute_List; Index : Integer; Default : Boolean := False)
      return Boolean
   is
      Val : Symbol;
   begin
      if Index < 0 then
         return Default;
      else
         Val := Get_Value (List, Index);
         return Get (Val).all = "true" or else Get (Val).all = "1";
      end if;
   end Get_Value_As_Boolean;

   --------------------------
   -- Set_Normalized_Value --
   --------------------------

   procedure Set_Normalized_Value
     (List : Sax_Attribute_List; Index : Integer; Value : Sax.Symbols.Symbol)
   is
   begin
      List.List (Index).Value := Value;
   end Set_Normalized_Value;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (List : Sax_Attribute_List; Index : Integer)
      return Sax.Attributes.Attribute_Type is
   begin
      return List.List (Index).Att_Type;
   end Get_Type;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (List : Sax_Attribute_List; Index : Integer;
      Typ  : Sax.Attributes.Attribute_Type) is
   begin
      List.List (Index).Att_Type := Typ;
   end Set_Type;

   ----------------
   -- Get_Length --
   ----------------

   function Get_Length (List : Sax_Attribute_List) return Natural is
   begin
      return List.Count;
   end Get_Length;

   ----------------
   -- Get_Prefix --
   ----------------

   function Get_Prefix
     (List : Sax_Attribute_List; Index : Integer) return Sax.Symbols.Symbol is
   begin
      return Get_Prefix (List.List (Index).NS);
   end Get_Prefix;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (List : Sax_Attribute_List; Index : Integer) return Qualified_Name is
   begin
      return (NS    => Get_URI (List.List (Index).NS),
              Local => List.List (Index).Local_Name);
   end Get_Name;

   ---------------
   -- Get_Qname --
   ---------------

   function Get_Qname
     (List : Sax_Attribute_List; Index : Integer)
      return Unicode.CES.Byte_Sequence
   is
   begin
      return Qname_From_Name (Get_Prefix (List.List (Index).NS),
                              List.List (Index).Local_Name);
   end Get_Qname;

   ----------------------
   -- Current_Location --
   ----------------------

   function Current_Location
     (Handler : Sax_Reader) return Sax.Locators.Location is
   begin
      return Get_Location (Handler.Locator);
   end Current_Location;

   ---------------------
   -- Set_XML_Version --
   ---------------------

   procedure Set_XML_Version
     (Parser : in out Sax_Reader; XML : XML_Versions := XML_1_0_Fifth_Edition)
   is
   begin
      if XML = XML_1_0 then
         Parser.XML_Version := XML_1_0_Fifth_Edition;
      else
         Parser.XML_Version := XML;
      end if;
   end Set_XML_Version;

   ---------------------
   -- Get_XML_Version --
   ---------------------

   function Get_XML_Version (Parser : Sax_Reader) return XML_Versions is
   begin
      return Parser.XML_Version;
   end Get_XML_Version;

end Sax.Readers;
