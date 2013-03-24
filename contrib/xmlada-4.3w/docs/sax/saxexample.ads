with Sax.Readers;
with Unicode.CES;
with Sax.Attributes;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package SaxExample is

   type String_Access is access String;

   type Reader is new Sax.Readers.Reader with record
       Current_Pref  : Unbounded_String;
       Current_Value : Unbounded_String;
   end record;

   procedure Start_Element
     (Handler       : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "";
      Atts          : Sax.Attributes.Attributes'Class);

   procedure End_Element
     (Handler : in out Reader;
      Namespace_URI : Unicode.CES.Byte_Sequence := "";
      Local_Name    : Unicode.CES.Byte_Sequence := "";
      Qname         : Unicode.CES.Byte_Sequence := "");

   procedure Characters
     (Handler : in out Reader;
      Ch      : Unicode.CES.Byte_Sequence);

end SaxExample;
