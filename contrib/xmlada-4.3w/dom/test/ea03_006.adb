with DOM.Core; use DOM.Core;
with DOM.Core.Documents; use DOM.Core.Documents;
with DOM.Core.Elements;  use DOM.Core.Elements;
with DOM.Core.Nodes;     use DOM.Core.Nodes;

procedure EA03_006 is
   Impl : DOM_Implementation;
   Doc : Document;
   Child, Element : Node;
   pragma Unreferenced (Child);
begin
   Doc := Create_Document (Impl);

   Element := Create_Element (Doc, "myelem");
   Set_Attribute (Element, "id", "42");
   Child := Append_Child (Doc, Element);

   Print (Element);
end EA03_006;
