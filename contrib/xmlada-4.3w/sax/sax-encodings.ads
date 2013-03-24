------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
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

with Unicode.CES.Utf8;
with Unicode.CES.Utf32;
use Unicode.CES;

package Sax.Encodings is
   Encoding : constant Unicode.CES.Encoding_Scheme :=
     Unicode.CES.Utf8.Utf8_Encoding;
   --  The default encoding used internally by XML/Ada, in particular in the
   --  DOM tree. All characters are unicode.
   --  The default value of UTF8 is economical in terms of memory, supports the
   --  whole unicode range of characters, but is slightly slower to process.
   --
   --  You can change this to Basic_8bit, which is both very fast and with
   --  reduced memory usage, but will only apply to Latin-1 documents.
   --
   --  You can also change this to UTF32 for instance, which will use more
   --  memory, but is faster to process.
   --
   --  In all cases, if you modify this value, you will need to reencode all
   --  the strings below in the new encoding.

   function From_Utf32
     (Str : Unicode.CES.Utf32.Utf32_LE_String)
      return Unicode.CES.Utf8.Utf8_String
      renames Unicode.CES.Utf8.From_Utf32;

   ----------------
   --  Constants --
   ----------------
   --  This is a set of constant strings that need to be defined for the
   --  parser. We do not initialize them through calls to Encoding.Encode, for
   --  efficiency reasons, and so that we know in advance the length of the
   --  byte_sequence (no memory allocation).
   --  These strings must be encoded with the default encoding.

   Amp_Sequence           : constant Byte_Sequence := "amp";
   Amp_DOM_Sequence       : constant Byte_Sequence := "&amp;";
   Any_Sequence           : constant Byte_Sequence := "ANY";
   Apos_Sequence          : constant Byte_Sequence := "apos";
   Attlist_Sequence       : constant Byte_Sequence := "ATTLIST";
   Cdata_Sequence         : constant Byte_Sequence := "CDATA";
   Cr_Sequence            : constant Byte_Sequence := "&#13;";
   Doctype_Sequence       : constant Byte_Sequence := "DOCTYPE";
   Element_Sequence       : constant Byte_Sequence := "LEMENT";
   Empty_Sequence         : constant Byte_Sequence := "EMPTY";
   Encoding_Sequence      : constant Byte_Sequence := "encoding";
   Entit_Sequence         : constant Byte_Sequence := "ENTIT";
   Id_Sequence            : constant Byte_Sequence := "ID";
   Ies_Sequence           : constant Byte_Sequence := "IES";
   Fixed_Sequence         : constant Byte_Sequence := "FIXED";
   Gt_Sequence            : constant Byte_Sequence := "gt";
   Gt_DOM_Sequence        : constant Byte_Sequence := "&gt;";
   Implied_Sequence       : constant Byte_Sequence := "IMPLIED";
   Include_Sequence       : constant Byte_Sequence := "INCLUDE";
   Ignore_Sequence        : constant Byte_Sequence := "IGNORE";
   Lang_Sequence          : constant Byte_Sequence := "lang";
   Lf_Sequence            : constant Byte_Sequence := "&#10;";
   Lt_Sequence            : constant Byte_Sequence := "lt";
   Lt_DOM_Sequence        : constant Byte_Sequence := "&lt;";
   Mtoken_Sequence        : constant Byte_Sequence := "MTOKEN";
   Ndata_Sequence         : constant Byte_Sequence := "NDATA";
   Otation_Sequence       : constant Byte_Sequence := "OTATION";
   No_Sequence            : constant Byte_Sequence := "no";
   Notation_Sequence      : constant Byte_Sequence := "NOTATION";
   Ntity_Sequence         : constant Byte_Sequence := "NTITY";
   Pcdata_Sequence        : constant Byte_Sequence := "#PCDATA";
   Public_Sequence        : constant Byte_Sequence := "PUBLIC";
   Quot_Sequence          : constant Byte_Sequence := "quot";
   Quot_DOM_Sequence      : constant Byte_Sequence := "&quot;";
   Ref_Sequence           : constant Byte_Sequence := "REF";
   Required_Sequence      : constant Byte_Sequence := "REQUIRED";
   Standalone_Sequence    : constant Byte_Sequence := "standalone";
   Tab_Sequence           : constant Byte_Sequence := "&#9;";
   System_Sequence        : constant Byte_Sequence := "SYSTEM";
   Version_Sequence       : constant Byte_Sequence := "version";
   Xml_Sequence           : constant Byte_Sequence := "xml";
   Xmlns_Sequence         : constant Byte_Sequence := "xmlns";
   Yes_Sequence           : constant Byte_Sequence := "yes";
   True_Sequence          : constant Byte_Sequence := "true";
   False_Sequence         : constant Byte_Sequence := "false";
   Vertical_Line_Sequence : constant Byte_Sequence := "|";
   Comma_Sequence         : constant Byte_Sequence := ",";
   Closing_Parenthesis_Sequence : constant Byte_Sequence := ")";
   Opening_Parenthesis_Sequence : constant Byte_Sequence := "(";
   Star_Sequence          : constant Byte_Sequence := "*";
   Question_Mark_Sequence : constant Byte_Sequence := "?";
   Plus_Sign_Sequence     : constant Byte_Sequence := "+";
   Colon_Sequence         : constant Byte_Sequence := ":";
   Percent_Sign_Sequence  : constant Byte_Sequence := "%";
   Space_Word_Sequence    : constant Byte_Sequence := "space";
   Default_Sequence       : constant Byte_Sequence := "default";
   Preserve_Sequence      : constant Byte_Sequence := "preserve";
   Space_Sequence         : constant Byte_Sequence := " ";
   Space_Numeric_Sequence : constant Byte_Sequence := "&#32;";
   Less_Than_Sequence     : constant Byte_Sequence := "<";
   Greater_Than_Sequence  : constant Byte_Sequence := ">";
   Equals_Sign_Sequence   : constant Byte_Sequence := "=";
   Quotation_Mark_Sequence      : constant Byte_Sequence := """";
   Slash_Sequence         : constant Byte_Sequence := "/";
   URN_Sequence           : constant Byte_Sequence := "urn:";
   Namespaces_URI_Sequence : constant Byte_Sequence :=
     "http://www.w3.org/XML/1998/namespace";
   Xmlns_URI_Sequence : constant Byte_Sequence :=
     "http://www.w3.org/2000/xmlns/";
   Cdata_Section_Name_Sequence : constant Byte_Sequence :=
     "#cdata-section";
   Comment_Name_Sequence : constant Byte_Sequence := "#comment";
   Document_Name_Sequence : constant Byte_Sequence := "#document";
   Document_Fragment_Name_Sequence : constant Byte_Sequence :=
     "#document-fragment";
   Text_Name_Sequence : constant Byte_Sequence := "#text";

   Error_Handler_Sequence  : constant Byte_Sequence := "error-handler";
   Canonical_Form_Sequence : constant Byte_Sequence := "canonical-form";
   Cdata_Sections_Sequence : constant Byte_Sequence := "cdata-sections";
   Comments_Sequence       : constant Byte_Sequence := "comments";
   Datatype_Normalization_Sequence  : constant Byte_Sequence :=
     "datatype-normalization";
   Discard_Default_Content_Sequence : constant Byte_Sequence :=
     "discard-default-content";
   Entities_Sequence : constant Byte_Sequence := "entities";
   Infoset_Sequence : constant Byte_Sequence := "infoset";
   Namespaces_Sequence : constant Byte_Sequence := "namespaces";
   Namespace_Declarations_Sequence : constant Byte_Sequence :=
     "namespace-declarations";
   Normalize_Characters_Sequence : constant Byte_Sequence :=
     "normalize-characters";
   Split_Cdata_Sections_Sequence : constant Byte_Sequence :=
     "split-cdata-sections";
   Validate_Sequence : constant Byte_Sequence := "validate";
   Validate_If_Schema_Sequence : constant Byte_Sequence :=
     "validate-if-schema";
   Whitespace_In_Element_Sequence : constant Byte_Sequence :=
     "whitespace-in-element-content";
   Mailto_Sequence : constant Byte_Sequence := "mailto:";

end Sax.Encodings;
