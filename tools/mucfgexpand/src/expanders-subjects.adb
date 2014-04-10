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
with DOM.Core.Elements;
with DOM.Core.Documents;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mucfgvcpu;

with Expanders.XML_Utils;

package body Expanders.Subjects
is

   -------------------------------------------------------------------------

   procedure Add_Binaries (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/binary");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Bin_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "filename");
            Filesize : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "size");
            Virtual_Address : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Bin_Node,
                 Name => "virtualAddress");
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node (N => Bin_Node);
            Subj_Mem_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  =>  McKae.XML.XPath.XIA.XPath_Query
                 (N     => Subj_Node,
                  XPath => "memory"),
               Index => 0);
            Subj_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Subj_Node,
               Name => "name");
         begin
            Mulog.Log (Msg => "Mapping binary '" & Filename & "' with size "
                       & Filesize & " at virtual address " & Virtual_Address
                       & " of subject '" & Subj_Name & "'");
            XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|bin",
               Address     => "",
               Size        => Filesize,
               Caching     => "WB",
               Alignment   => "16#1000#",
               File_Name   => Filename,
               File_Format => "bin_raw",
               File_Offset => "none");
            Muxml.Utils.Append_Child
              (Node      => Subj_Mem_Node,
               New_Child => XML_Utils.Create_Virtual_Memory_Node
                 (Policy        => Data,
                  Logical_Name  => "binary",
                  Physical_Name => Subj_Name & "|bin",
                  Address       => Virtual_Address,
                  Writable      => True,
                  Executable    => True));

            XML_Utils.Remove_Child
              (Node       => Subj_Node,
               Child_Name => "binary");
         end;
      end loop;
   end Add_Binaries;

   -------------------------------------------------------------------------

   procedure Add_Tau0 (Data : in out Muxml.XML_Data_Type)
   is
      Tau0_CPU : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Data.Doc,
           XPath => "/system/scheduling/majorFrame/cpu/"
           & "minorFrame[@subject='tau0']/..",
           Name  => "id");
      Subjects_Node : constant DOM.Core.Node
        := DOM.Core.Nodes.Item
          (List  => McKae.XML.XPath.XIA.XPath_Query
             (N     => Data.Doc,
              XPath => "/system/subjects"),
           Index => 0);
      Tau0_Node : DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "subject");
      Mem_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "memory");
      Bin_Node  : constant DOM.Core.Node
        := DOM.Core.Documents.Create_Element
          (Doc      => Data.Doc,
           Tag_Name => "binary");
   begin
      Mulog.Log (Msg => "Adding tau0 subject");

      Tau0_Node := DOM.Core.Nodes.Insert_Before
        (N         => Subjects_Node,
         New_Child => Tau0_Node,
         Ref_Child => DOM.Core.Nodes.First_Child (N => Subjects_Node));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "id",
         Value => "0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "name",
         Value => "tau0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "profile",
         Value => "native");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Tau0_Node,
         Name  => "cpu",
         Value => Tau0_CPU);

      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "bootparams"));

      Muxml.Utils.Append_Child
        (Node      => Mem_Node,
         New_Child => XML_Utils.Create_Virtual_Memory_Node
           (Policy        => Data,
            Logical_Name  => "sys_interface",
            Physical_Name => "sys_interface",
            Address       => "16#001f_f000#",
            Writable      => True,
            Executable    => False));
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => Mem_Node);

      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "devices"));
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "events"));

      DOM.Core.Elements.Set_Attribute
        (Elem  => Bin_Node,
         Name  => "filename",
         Value => "tau0");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Bin_Node,
         Name  => "size",
         Value => "16#0001_4000#");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Bin_Node,
         Name  => "virtualAddress",
         Value => "16#1000#");
      Muxml.Utils.Append_Child
        (Node      => Tau0_Node,
         New_Child => Bin_Node);
   end Add_Tau0;

   -------------------------------------------------------------------------

   procedure Handle_Monitors (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject/monitor/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Monitored_Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Parent_Node
                (N => DOM.Core.Nodes.Parent_Node
                   (N => Monitored_Subj_Node));
         begin
            XML_Utils.Remove_Child
              (Node       => Subj_Node,
               Child_Name => "monitor");
         end;
      end loop;
   end Handle_Monitors;

   -------------------------------------------------------------------------

   procedure Handle_Profile (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            use type DOM.Core.Node;

            Subj : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Nodes,
                 Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj,
                 Name => "name");
            Profile : constant Mucfgvcpu.Profile_Type
              := Mucfgvcpu.Profile_Type'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Subj,
                    Name => "profile"));
            VCPU_Node : DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subj,
                    XPath => "vcpu"),
                 Index => 0);
         begin
            if VCPU_Node = null then
               VCPU_Node := DOM.Core.Nodes.Insert_Before
                 (N         => Subj,
                  New_Child => DOM.Core.Documents.Create_Element
                    (Doc      => Data.Doc,
                     Tag_Name => "vcpu"),
                  Ref_Child => DOM.Core.Nodes.First_Child (N => Subj));
            end if;

            Mulog.Log (Msg => "Setting profile of subject '" & Subj_Name
                       & "' to " & Profile'Img);
            Mucfgvcpu.Set_VCPU_Profile (Profile => Profile,
                                        Node    => VCPU_Node);
            DOM.Core.Elements.Remove_Attribute
              (Elem => Subj,
               Name => "profile");
         end;
      end loop;
   end Handle_Profile;

end Expanders.Subjects;
