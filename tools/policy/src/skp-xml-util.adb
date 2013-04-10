with DOM.Core.Nodes;
with DOM.Core.Elements;

package body Skp.Xml.Util
is

   -------------------------------------------------------------------------

   procedure For_Each_Node
     (Node     : DOM.Core.Element;
      Tag_Name : String;
      Process  : not null access procedure (Node : DOM.Core.Node))
   is
      package DC renames DOM.Core;

      List : DC.Node_List;
   begin
      List := DC.Elements.Get_Elements_By_Tag_Name
        (Elem => Node,
         Name => Tag_Name);

      for Index in 1 .. DC.Nodes.Length (List => List) loop
         Process (Node => DC.Nodes.Item
                  (List  => List,
                   Index => Index - 1));
      end loop;
      DOM.Core.Free (List => List);
   end For_Each_Node;

   -------------------------------------------------------------------------

   function Get_Element_Attr_By_Tag_Name
     (Node      : DOM.Core.Element;
      Tag_Name  : String;
      Attr_Name : String;
      Required  : Boolean := True)
      return String
   is
      use type DOM.Core.Node;

      Val_Node : constant DOM.Core.Node := Util.Get_Element_By_Tag_Name
        (Node     => Node,
         Tag_Name => Tag_Name,
         Required => Required);
   begin
      if Val_Node /= null then
         return DOM.Core.Elements.Get_Attribute
           (Elem => Val_Node,
            Name => Attr_Name);
      else
         return "";
      end if;
   end Get_Element_Attr_By_Tag_Name;

   -------------------------------------------------------------------------

   function Get_Element_By_Tag_Name
     (Node     : DOM.Core.Element;
      Tag_Name : String;
      Required : Boolean := True)
      return DOM.Core.Node
   is
      List : DOM.Core.Node_List;
   begin
      List := DOM.Core.Elements.Get_Elements_By_Tag_Name
        (Elem => Node,
         Name => Tag_Name);

      if DOM.Core.Nodes.Length (List => List) = 0 then
         DOM.Core.Free (List => List);
         if Required then
            raise Processing_Error with "XML element '" & Tag_Name
              & "' missing";
         else
            return null;
         end if;
      end if;

      return Node : DOM.Core.Node do
         Node := DOM.Core.Nodes.Item (List  => List,
                                      Index => 0);
         DOM.Core.Free (List => List);
      end return;
   end Get_Element_By_Tag_Name;

   -------------------------------------------------------------------------

   function To_Memory_Size (Str : String) return SK.Word64
   is
      use type SK.Word64;

      Nr_Str : constant String    := Str (Str'First .. Str'Last - 1);
      Unit   : constant Character := Str (Str'Last);
      Value  : SK.Word64;
   begin
      Value := SK.Word64'Value (Nr_Str);

      case Unit is
         when 'k' | 'K' => return Value * (2 ** 10);
         when 'm' | 'M' => return Value * (2 ** 20);
         when 'g' | 'G' => return Value * (2 ** 30);
         when others    => raise Conversion_Error with "Invalid unit '" & Unit
              & "' in size conversion";
      end case;

   exception
      when Constraint_Error =>
         raise Conversion_Error with "Invalid size string '" & Str & "'";
   end To_Memory_Size;

end Skp.Xml.Util;
