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

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

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

   procedure Handle_Profile (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Nodes,
               Index => I);
         begin
            DOM.Core.Elements.Remove_Attribute
              (Elem => Subj,
               Name => "profile");
         end;
      end loop;
   end Handle_Profile;

end Expanders.Subjects;
