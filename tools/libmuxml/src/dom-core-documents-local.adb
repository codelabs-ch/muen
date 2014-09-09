------------------------------------------------------------------------------
--                     XML/Ada - An XML suite for Ada95                     --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
with DOM.Core.Nodes;            use DOM.Core.Nodes;
with DOM.Core.Elements;         use DOM.Core.Elements;
with Sax.Symbols;               use Sax.Symbols;
with Sax.Utils;                 use Sax.Utils;
pragma Warnings (On);

package body DOM.Core.Documents.Local
is

   -------------------------------------------------------------------------

   function Adopt_Node (Doc : Document; Source : Node) return Node is
      Old_Doc : constant Document := Owner_Document (Source);

      procedure Copy (S : in out Symbol);
      procedure Copy (S : in out Node_Name_Def);
      --  Duplicate the symbol into the new document

      procedure Recurse (Parent : Node; N : Node);
      --  Adopt node recursively.
      --  Parent is the new parent for the node

      procedure Copy (S : in out Symbol) is
      begin
         if S /= Sax.Symbols.No_Symbol then
            S := Find (Doc.Symbols, Get (S).all);
         end if;
      end Copy;

      procedure Copy (S : in out Node_Name_Def) is
      begin
         Copy (S.Prefix);
         Copy (S.Local_Name);
         Copy (S.Namespace);
      end Copy;

      procedure Recurse (Parent : Node; N : Node) is
         Dest  : Integer;
         Dummy : Attr;
         pragma Unreferenced (Dummy);
      begin
         case N.Node_Type is
            when Document_Node | Document_Type_Node =>
               raise Not_Supported_Err with
                 "Cannot adopt a document or document type node";

            when Attribute_Node =>
               if N.Is_Id then
                  Document_Remove_Id (Old_Doc, N.Attr_Value);
                  Copy (N.Attr_Value);

                  if Parent /= null then
                     Document_Add_Id (Doc, N.Attr_Value, Parent);
                  end if;
               else
                  Copy (N.Attr_Value);
               end if;

               if Parent = null then
                  Dummy := Remove_Attribute_Node
                    (Element (N.Owner_Element), Attr (N));
               end if;

               Copy (N.Attr_Name);

            when Document_Fragment_Node =>
               if N.Doc_Frag_Children.Items /= null then
                  for J in
                    N.Doc_Frag_Children.Items'First .. N.Doc_Frag_Children.Last
                  loop
                     Recurse (N, N.Doc_Frag_Children.Items (J));
                  end loop;
               end if;

            when Element_Node =>
               Copy (N.Name);

               --  Default attributes must be discarded

               if N.Attributes.Items /= null then
                  Dest := N.Attributes.Items'First - 1;
                  for A in N.Attributes.Items'First .. N.Attributes.Last loop
                     if N.Attributes.Items (A).Specified then
                        Dest := Dest + 1;
                        Recurse (N, N.Attributes.Items (A));
                        if A /= Dest then
                           N.Attributes.Items (Dest) := N.Attributes.Items (A);
                           N.Attributes.Items (A) := null;
                        end if;
                     end if;
                  end loop;
                  N.Attributes.Last := Dest;
               end if;

               if N.Children.Items /= null then
                  for A in N.Children.Items'First .. N.Children.Last loop
                     Recurse (N, N.Children.Items (A));
                  end loop;
               end if;

            when Entity_Node =>
               raise Not_Supported_Err with "Cannot adopt an entity node";

            when Notation_Node =>
               raise Not_Supported_Err with "Cannot adopt a notation node";

            when Entity_Reference_Node =>
               Copy (N.Entity_Reference_Name);

            when Processing_Instruction_Node =>
               Copy (N.Target);
               Copy (N.Pi_Data);

            when Text_Node =>
               null;  --  nothing to do

            when Cdata_Section_Node =>
               null;  --  nothing to do

            when Comment_Node =>
               null;  --  nothing to do
         end case;
      end Recurse;

   begin
      --  ??? Should raise No_Modification_Allowed_Err if source is readonly

      Recurse (null, Source);
      Source.Parent_Is_Owner := True;
      Source.Parent := Node (Doc);
      return Source;
   end Adopt_Node;

end DOM.Core.Documents.Local;
