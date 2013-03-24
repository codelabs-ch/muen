with Input_Sources.File; use Input_Sources.File;
with Sax.Readers;        use Sax.Readers;
with DOM.Readers;        use DOM.Readers;
with DOM.Core;           use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Nodes;     use DOM.Core.Nodes;
with DOM.Core.Attrs;     use DOM.Core.Attrs;
with Ada.Text_IO;        use Ada.Text_IO;

procedure DomExample2 is
   Input  : File_Input;
   Reader : Tree_Reader;
   Doc    : Document;
   List   : Node_List;
   N      : Node;
   A      : Attr;
   C      : Node;
begin
   Set_Public_Id (Input, "Preferences file");
   Open ("pref.xml", Input);

   Set_Feature (Reader, Validation_Feature, False);
   Set_Feature (Reader, Namespace_Feature, False);

   Parse (Reader, Input);
   Close (Input);

   Doc := Get_Tree (Reader); 

   List := Get_Elements_By_Tag_Name (Doc, "pref");

   for Index in 1 .. Length (List) loop
       N := Item (List, Index - 1);
       A := Get_Named_Item (Attributes (N), "name");
       Put_Line ("Value of """ & Value (A) & """ is "
                 & Node_Value (First_Child (N)));
   end loop; 

   Free (List);

   Free (Reader);
end DomExample2;
