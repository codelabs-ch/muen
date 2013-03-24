with DOM.Core;                 use DOM.Core;
with DOM.Core.Documents;       use DOM.Core.Documents;
with DOM.Core.Nodes;           use DOM.Core.Nodes;
with Sax.Encodings;            use Sax.Encodings;
with Unicode.CES;              use Unicode.CES;
with Unicode.CES.Basic_8bit;   use Unicode.CES.Basic_8bit;
--  with DOM.Core.Character_Datas; use DOM.Core.Character_Datas;
with DOM.Core.Texts;           use DOM.Core.Texts;

with Ada.Text_IO; use Ada.Text_IO;

procedure Test_Node is

   function "-" (Str : String) return Byte_Sequence;
   ---------
   -- Str --
   ---------

   function "-" (Str : String) return Byte_Sequence is
   begin
      return Sax.Encodings.From_Utf32 (Unicode.CES.Basic_8bit.To_Utf32 (Str));
   end "-";

   Implementation : DOM_Implementation;
   Doc : Document;
   Elem1, Elem2, Elem : Node;
begin
   Doc := Create_Document (Implementation);
   Elem := Append_Child (Doc, Create_Element_NS (Doc, -"", -"foo"));
   Elem := Append_Child (Elem, Create_Element  (Doc, -"toto"));
   Elem1 := Append_Child (Elem, Create_Text_Node (Doc, -"Dummy text"));
   Elem2 := Append_Child (Elem, Create_Element  (Doc, -"toto"));
   Elem2 := Append_Child (Elem2, Create_Text_Node (Doc, -"Dummy text2"));

   Put_Line ("------  Tree ------");
   Print (Doc);
   New_Line;

   Put_Line ("------  Find toto -----");
   Print (Get_Elements_By_Tag_Name (Doc, -"toto"));
   New_Line;

   Put_Line ("------ Split_Text 3 ----");
   Elem2 := Split_Text (Elem2, 3);
   Print (Doc);
   New_Line;

   Put_Line ("------ Normalize -----");
   Normalize (Doc);
   Print (Doc);
   New_Line;

   Free (Doc);
end Test_Node;
