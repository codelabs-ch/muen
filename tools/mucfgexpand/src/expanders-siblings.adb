--
--  Copyright (C) 2018  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2018  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

with Expanders.Types;

package body Expanders.Siblings
is

   -------------------------------------------------------------------------

   procedure Add_Subject_Profile_VCPU (Data : in out Muxml.XML_Data_Type)
   is
      Siblings : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[not(sibling)]");
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject[sibling]");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Sib_Ref_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "sibling");
            Sib_Ref : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Sib_Ref_Node,
                 Name => "ref");
            Sib_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Siblings,
                 Ref_Attr  => "name",
                 Ref_Value => Sib_Ref);
            Profile_Str : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Sib_Node,
                 Name => "profile");
            Profile : constant Types.Subject_Profile_Type
              := Types.Subject_Profile_Type'Value (Profile_Str);
            Sib_VCPU_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Sib_Node,
                 XPath => "vcpu");
            Subj_VCPU_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Subj_Node,
                 XPath => "vcpu");
         begin
            Mulog.Log (Msg => "Setting profile of subject '" & Subj_Name
                       & "' to " & Profile'Img & " (VCPU profile "
                       & Types.Subj_VCPU_Profile_Map (Profile)'Img
                       & ") of sibling");
            DOM.Core.Elements.Set_Attribute
              (Elem  => Subj_Node,
               Name  => "profile",
               Value => Profile_Str);

            declare
               Dummy_vCPU_Node : DOM.Core.Node
                 := DOM.Core.Nodes.Clone_Node
                   (N    => Sib_VCPU_Node,
                    Deep => True);
            begin
               Dummy_vCPU_Node := DOM.Core.Nodes.Insert_Before
                 (N         => Subj_Node,
                  New_Child => Dummy_vCPU_Node,
                  Ref_Child => Subj_VCPU_Node);
               Muxml.Utils.Merge
                 (Left      => Dummy_vCPU_Node,
                  Right     => Subj_VCPU_Node,
                  List_Tags => (1 => Ada.Strings.Unbounded.To_Unbounded_String
                                ("msr")));
               Dummy_vCPU_Node := DOM.Core.Nodes.Remove_Child
                 (N         => Subj_Node,
                  Old_Child => Subj_VCPU_Node);
               DOM.Core.Nodes.Free (N => Dummy_vCPU_Node);
            end;
         end;
      end loop;
   end Add_Subject_Profile_VCPU;

end Expanders.Siblings;
