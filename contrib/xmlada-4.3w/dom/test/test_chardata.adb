with DOM.Core;                 use DOM.Core;
with DOM.Core.Documents;       use DOM.Core.Documents;
with DOM.Core.Nodes;           use DOM.Core.Nodes;
with Sax.Encodings;            use Sax.Encodings;
with Unicode.CES;              use Unicode.CES;
with Unicode.CES.Basic_8bit;   use Unicode.CES.Basic_8bit;
with DOM.Core.Character_Datas; use DOM.Core.Character_Datas;

with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Chardata is

   function "-" (Str : String) return Byte_Sequence;
   procedure Expect (Got, Expected : DOM_String);

   ---------
   -- Str --
   ---------

   function "-" (Str : String) return Byte_Sequence is
   begin
      return Sax.Encodings.From_Utf32 (Unicode.CES.Basic_8bit.To_Utf32 (Str));
   end "-";

   ------------
   -- Expect --
   ------------

   procedure Expect (Got, Expected : DOM_String) is
   begin
      if Got /= Expected then
         Put_Line ("Expected: --" & Expected & "--");
         Put_Line ("Got     : --" & Got & "--");
      end if;
   end Expect;

   Implementation : DOM_Implementation;
   Doc  : Node;
   Elem : Node;
begin
   Doc := Create_Document (Implementation);
   Elem := Append_Child (Doc, Create_Element (Doc, -"elem"));
   Elem := Append_Child (Elem, Create_Text_Node (Doc, -"Dummy text"));

   Append_Data (Elem, -"appended");
   Expect (Data (Elem), -"Dummy textappended");

   Delete_Data (Elem, 3, 3);
   Expect (Data (Elem), -"Dumtextappended");

   Replace_Data (Elem, 3, 3, -"replaced");
   Expect (Data (Elem), -"Dumreplacedtappended");
   Free (Doc);
end Test_Chardata;
