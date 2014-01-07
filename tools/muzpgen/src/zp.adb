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

with Ada.Strings.Fixed;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Muxml;
with Mulog;

with Zp.Generator;

package body Zp
is

   use McKae.XML.XPath.XIA;

   --  Extract subject name from given string. Raises a processing error
   --  exception on failure.
   function To_Subject_Name (Str : String) return String;

   --  Return bootparams of subject associated with memory element given by
   --  name.
   function Get_Bootparams
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String;

   --  Return zero-page guest-physical address extracted from mapping of
   --  physical memory with given name.
   function Get_Guest_Physical
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String;

   -------------------------------------------------------------------------

   function Get_Bootparams
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => XPath_Query
           (N     => XML_Data.Doc,
            XPath => "/system/subjects/subject[@name='"
            & To_Subject_Name (Str => Memory_Name) & "']/bootparams/text()"),
         Index => 0);
   begin
      if Node /= null then
         return DOM.Core.Nodes.Node_Value (N => Node);
      else
         return "";
      end if;
   end Get_Bootparams;

   -------------------------------------------------------------------------

   function Get_Guest_Physical
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => XPath_Query
           (N     => XML_Data.Doc,
            XPath => "/system/subjects/subject/memory/memory/physical[@name='"
            & Memory_Name & "']/../@virtualAddress"),
         Index => 0);
   begin
      return DOM.Core.Nodes.Node_Value (N => Node);
   end Get_Guest_Physical;

   -------------------------------------------------------------------------

   procedure Process
     (Policy     : String;
      Output_Dir : String)
   is
      Zps  : DOM.Core.Node_List;
      Data : Muxml.XML_Data_Type;
   begin
      Mulog.Log (Msg => "Using output directory '" & Output_Dir & "'");
      Mulog.Log (Msg => "Processing policy '" & Policy & "'");
      Muxml.Parse (Data => Data,
                   File => Policy);
      Zps := XPath_Query (N     => Data.Doc,
                          XPath => "/system/memory/memory/file[@format='zp']");

      Mulog.Log (Msg => "Found" & DOM.Core.Nodes.Length (List => Zps)'Img
                 & " zero-page file(s)");

      for I in 1 .. DOM.Core.Nodes.Length (List => Zps) loop
         declare
            Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Zps,
                                      Index => I - 1);
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "filename");
            Memname  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
                 Name => "name");
            Physaddr : constant String
              := Get_Guest_Physical
                (XML_Data    => Data,
                 Memory_Name => Memname);
         begin
            Mulog.Log (Msg => "Guest-physical address of " & Memname
                       & " zero-page is " & Physaddr);
            Zp.Generator.Write
              (Filename         => Output_Dir & "/" & Filename,
               Cmdline          => Get_Bootparams
                 (XML_Data    => Data,
                  Memory_Name => Memname),
               Physical_Address => Natural'Value (Physaddr));
         end;
      end loop;
   end Process;

   -------------------------------------------------------------------------

   function To_Subject_Name (Str : String) return String
   is
      Udrl_Idx : constant Natural := Ada.Strings.Fixed.Index
        (Source  => Str,
         Pattern => "|");
   begin
      return Str (Str'First .. Udrl_Idx - 1);
   end To_Subject_Name;

end Zp;
