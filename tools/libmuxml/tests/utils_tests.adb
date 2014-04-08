--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with DOM.Core.Nodes;
with DOM.Core.Documents;
with DOM.Core.Elements;

with Muxml.Utils;

package body Utils_Tests
is

   use Ahven;
   use Muxml;

   -------------------------------------------------------------------------

   procedure Append_Child
   is
      use type DOM.Core.Node;

      Impl : DOM.Core.DOM_Implementation;
      Data : XML_Data_Type;
      Node, Child : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Node := DOM.Core.Documents.Create_Element (Doc      => Data.Doc,
                                                 Tag_Name => "node");
      Child := DOM.Core.Documents.Create_Element (Doc     => Data.Doc,
                                                  Tag_Name => "child");
      Utils.Append_Child (Node      => Node,
                          New_Child => Child);

      Assert (Condition => DOM.Core.Nodes.Has_Child_Nodes (N => Node),
              Message   => "Error appending child");
      Assert (Condition => DOM.Core.Nodes.First_Child (N => Node) = Child,
              Message   => "Child mismatch");
   end Append_Child;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "XML utility tests");
      T.Add_Test_Routine
        (Routine => Append_Child'Access,
         Name    => "Append XML child node");
      T.Add_Test_Routine
        (Routine => Merge_Nodes'Access,
         Name    => "Merge XML nodes");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Merge_Nodes
   is
      Data : Muxml.XML_Data_Type;
      Impl : DOM.Core.DOM_Implementation;
      Doc  : constant DOM.Core.Document
        := DOM.Core.Create_Document (Implementation => Impl);
      Node, Tmp : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Data,
                   Kind => Muxml.VCPU_Profile,
                   File => "data/vcpu_profile.xml");

      --  Construct the following XML structure:
      --  <vcpu><segments><cs selector="16#ffff#>text</cs></segments></vcpu>

      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "cs");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "access",
         Value => "16#cafe#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "selector",
         Value => "16#ffff#");
      Utils.Append_Child
        (Node      => Node,
         New_Child => DOM.Core.Documents.Create_Text_Node
           (Doc  => Doc,
            Data => "text"));
      Tmp := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "segments");
      Utils.Append_Child (Node      => Tmp,
                          New_Child => Node);
      Node := DOM.Core.Documents.Create_Element
        (Doc      => Doc,
         Tag_Name => "vcpu");
      Utils.Append_Child (Node      => Node,
                          New_Child => Tmp);
      Utils.Append_Child
        (Node      => Doc,
         New_Child => Node);

      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/segments/cs",
               Name  => "selector") = "16#0008#",
              Message   => "Unexpected cs selector attribute in vcpu policy");

      Utils.Merge (Left  => Data.Doc,
                   Right => Doc);

      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/segments/cs",
               Name  => "access") = "16#cafe#",
              Message   => "Error merging XML nodes: cs access");
      Assert (Condition => Utils.Get_Attribute
              (Doc   => Data.Doc,
               XPath => "/vcpu/segments/cs",
               Name  => "selector") = "16#ffff#",
              Message   => "Error merging XML nodes: cs selector");
   end Merge_Nodes;

end Utils_Tests;
