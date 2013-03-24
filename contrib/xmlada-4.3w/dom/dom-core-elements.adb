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

with DOM.Core.Attrs;     use DOM.Core.Attrs;
with DOM.Core.Documents; use DOM.Core.Documents;
with Sax.Symbols;        use Sax.Symbols;

package body DOM.Core.Elements is
   use Nodes_Htable;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (Elem : Element; Name : DOM_String)
      return DOM_String
   is
      Att : constant Attr := Get_Named_Item (Elem.Attributes, Name);
   begin
      if Att /= null then
         return Node_Value (Att);
      else
         return "";
      end if;
   end Get_Attribute;

   ----------------------
   -- Get_Attribute_NS --
   ----------------------

   function Get_Attribute_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String)
      return DOM_String
   is
      Att : constant Attr := Get_Named_Item_NS
        (Elem.Attributes, Namespace_URI, Local_Name);
   begin
      if Att /= null then
         return Node_Value (Att);
      else
         return "";
      end if;
   end Get_Attribute_NS;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (Elem : Element; Name : DOM_String; Value : DOM_String)
   is
      Att   : constant Attr := Create_Attribute
        (Owner_Document (Elem), Name);
   begin
      Set_Value (Att, Value);
      Set_Named_Item_NS (Elem.Attributes, Att);
   end Set_Attribute;

   ----------------------
   -- Set_Attribute_NS --
   ----------------------

   procedure Set_Attribute_NS
     (Elem : Element;
      Namespace_URI : DOM_String;
      Qualified_Name : DOM_String;
      Value : DOM_String)
   is
      Att   : constant Attr := Create_Attribute_NS
        (Owner_Document (Elem), Namespace_URI, Qualified_Name);
   begin
      Set_Value (Att, Value);
      Set_Named_Item_NS (Elem.Attributes, Att);
   end Set_Attribute_NS;

   ----------------------
   -- Remove_Attribute --
   ----------------------

   procedure Remove_Attribute (Elem : Element; Name : DOM_String) is
   begin
      Remove_Named_Item (Elem.Attributes, Name);
   end Remove_Attribute;

   -------------------------
   -- Remove_Attribute_NS --
   -------------------------

   procedure Remove_Attribute_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String) is
   begin
      Remove_Named_Item_NS (Elem.Attributes, Namespace_URI, Local_Name);
   end Remove_Attribute_NS;

   ------------------------
   -- Get_Attribute_Node --
   ------------------------

   function Get_Attribute_Node (Elem : Element; Name : DOM_String)
      return Attr is
   begin
      return Get_Named_Item (Elem.Attributes, Name);
   end Get_Attribute_Node;

   ---------------------------
   -- Get_Attribute_Node_Ns --
   ---------------------------

   function Get_Attribute_Node_NS
     (Elem : Element; Namespace_URI : DOM_String; Local_Name : DOM_String)
      return Attr is
   begin
      return Get_Named_Item_NS (Elem.Attributes, Namespace_URI, Local_Name);
   end Get_Attribute_Node_NS;

   ------------------------
   -- Set_Attribute_Node --
   ------------------------

   function Set_Attribute_Node (Elem : Element; New_Attr : Attr) return Attr is
   begin
      if Owner_Element (New_Attr) /= null then
         raise Inuse_Attribute_Err;
      end if;
      Set_Named_Item (Elem.Attributes, New_Attr);
      New_Attr.Owner_Element := Elem;
      return New_Attr;
   end Set_Attribute_Node;

   ---------------------------
   -- Set_Attribute_Node_NS --
   ---------------------------

   function Set_Attribute_Node_NS (Elem : Element; New_Attr : Attr)
      return Attr is
   begin
      if Owner_Element (New_Attr) /= null then
         raise Inuse_Attribute_Err;
      end if;
      Set_Named_Item_NS (Elem.Attributes, New_Attr);
      New_Attr.Owner_Element := Elem;
      return New_Attr;
   end Set_Attribute_Node_NS;

   ---------------------------
   -- Remove_Attribute_Node --
   ---------------------------

   function Remove_Attribute_Node (Elem : Element; Old_Attr : Attr)
      return Attr is
   begin
      pragma Assert (Owner_Element (Old_Attr) = Elem);
      Remove_Named_Item (Elem.Attributes, Old_Attr);
      return Old_Attr;
   end Remove_Attribute_Node;

   ------------------------------
   -- Remove_Attribute_Node_NS --
   ------------------------------

   function Remove_Attribute_Node_NS (Elem : Element; Old_Attr : Attr)
      return Attr renames Remove_Attribute_Node;

   ------------------------------
   -- Get_Elements_By_Tag_Name --
   ------------------------------

   function Get_Elements_By_Tag_Name (Elem : Element; Name : DOM_String := "*")
      return Node_List
   is
      procedure Get_Elements_From_Node (N : Node; List : in out Node_List);
      --  Depth search in N or its children/sibling for matching children.

      ----------------------------
      -- Get_Elements_From_Node --
      ----------------------------

      procedure Get_Elements_From_Node (N : Node; List : in out Node_List) is
         L : constant Node_List := Child_Nodes (N);
      begin
         if N.Node_Type = Element_Node
           and then (Name = "*" or else Node_Name (N) = Name)
         then
            Append (List, N);
         end if;

         for J in 0 .. L.Last loop
            Get_Elements_From_Node (L.Items (J), List);
         end loop;
      end Get_Elements_From_Node;

      L : Node_List;
   begin
      Get_Elements_From_Node (Elem, L);
      return L;
   end Get_Elements_By_Tag_Name;

   ---------------------------------
   -- Get_Elements_By_Tag_Name_NS --
   ---------------------------------

   function Get_Elements_By_Tag_Name_NS
     (Elem : Element;
      Namespace_URI : DOM_String := "*";
      Local_Name : DOM_String := "*")
      return Node_List
   is
      procedure Get_Elements_From_Node (N : Node; List : in out Node_List);
      --  Depth search in N or its children/sibling for matching children.

      ----------------------------
      -- Get_Elements_From_Node --
      ----------------------------

      procedure Get_Elements_From_Node (N : Node; List : in out Node_List) is
         L : constant Node_List := Child_Nodes (N);
      begin
         if N.Node_Type = Element_Node
           and then (Namespace_URI = "*"
                     or else DOM.Core.Nodes.Namespace_URI (N) = Namespace_URI)
           and then
             (Local_Name = "*"
              or else Symbol'(DOM.Core.Nodes.Local_Name (N)) = Local_Name)
         then
            Append (List, N);
         end if;

         for J in 0 .. L.Last loop
            Get_Elements_From_Node (L.Items (J), List);
         end loop;
      end Get_Elements_From_Node;

      L : Node_List;
   begin
      Get_Elements_From_Node (Elem, L);
      return L;
   end Get_Elements_By_Tag_Name_NS;

   ----------------------
   -- Set_Id_Attribute --
   ----------------------

   procedure Set_Id_Attribute
     (Elem  : Element;
      Name  : DOM_String;
      Is_Id : Boolean)
   is
      Id_Attr : constant Attr := Get_Attribute_Node (Elem, Name);
   begin
      if Id_Attr = null then
         raise Not_Found_Err;
      end if;

      Id_Attr.Is_Id := Is_Id;

      if Is_Id then
         Document_Add_Id
           (Owner_Document (Elem), Id => Id_Attr.Attr_Value, Elem => Elem);
      else
         Document_Remove_Id (Owner_Document (Elem), Id  => Id_Attr.Attr_Value);
      end if;
   end Set_Id_Attribute;

   -------------------------
   -- Set_Id_Attribute_NS --
   -------------------------

   procedure Set_Id_Attribute_NS
     (Elem          : Element;
      Namespace_URI : DOM_String;
      Local_Name    : DOM_String;
      Is_Id         : Boolean)
   is
      Id_Attr : constant Attr := Get_Attribute_Node_NS
        (Elem, Namespace_URI, Local_Name);
   begin
      if Id_Attr = null then
         raise Not_Found_Err;
      end if;

      Id_Attr.Is_Id := Is_Id;

      if Is_Id then
         Document_Add_Id
           (Owner_Document (Elem), Id   => Id_Attr.Attr_Value, Elem => Elem);
      else
         Document_Remove_Id (Owner_Document (Elem), Id  => Id_Attr.Attr_Value);
      end if;
   end Set_Id_Attribute_NS;

   ---------------------------
   -- Set_Id_Attribute_Node --
   ---------------------------

   procedure Set_Id_Attribute_Node
     (Elem : Element; Id_Attr : Attr; Is_Id : Boolean) is
   begin
      if Owner_Element (Id_Attr) /= Elem then
         raise Not_Found_Err;
      end if;

      Id_Attr.Is_Id := Is_Id;

      if Is_Id then
         Document_Add_Id
           (Owner_Document (Elem), Id   => Id_Attr.Attr_Value, Elem => Elem);
      else
         Document_Remove_Id (Owner_Document (Elem), Id  => Id_Attr.Attr_Value);
      end if;
   end Set_Id_Attribute_Node;

end DOM.Core.Elements;
