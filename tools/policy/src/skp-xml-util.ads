with DOM.Core;

package Skp.Xml.Util
is

   --  Return child element of given node. The name of the child element must
   --  match the specified tag name. If the element does not exist and the
   --  Required parameter is True an exception is raised. Otherwise, if
   --  Required is False, null is returned.
   function Get_Element_By_Tag_Name
     (Node     : DOM.Core.Element;
      Tag_Name : String;
      Required : Boolean := True)
      return DOM.Core.Node;

   --  Return child element attribute of given node. The child element and
   --  attribute name must match the specified tag and attribute. If the
   --  element or attribute does not exist and the Required parameter is True
   --  an exception is raised. Otherwise, if Required is False, an empty string
   --  is returned.
   function Get_Element_Attr_By_Tag_Name
     (Node      : DOM.Core.Element;
      Tag_Name  : String;
      Attr_Name : String;
      Required  : Boolean := True)
      return String;

   --  Invoke the given process procedure for each tag with specified name in
   --  the given XML data.
   procedure For_Each_Node
     (Node     : DOM.Core.Element;
      Tag_Name : String;
      Process  : not null access procedure (Node : DOM.Core.Node));

   --  Convert XML memory size string to word64.
   function To_Memory_Size (Str : String) return SK.Word64;

   Conversion_Error : exception;

end Skp.Xml.Util;
