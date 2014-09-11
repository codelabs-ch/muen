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

with Ada.Strings.Unbounded;

with DOM.Core.Nodes;
with DOM.Core.Elements;
with DOM.Core.Documents.Local;

with McKae.XML.XPath.XIA;

with Muxml.Utils;

package body Mergers
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   Platform_List_Tags : constant Muxml.Utils.Tags_Type
     := (1 => U ("device"),
         2 => U ("memoryBlock"));

   -------------------------------------------------------------------------

   procedure Merge_Platform
     (Policy        : in out Muxml.XML_Data_Type;
      Platform_File :        String)
   is
      use type DOM.Core.Node;

      Platform      : Muxml.XML_Data_Type;
      Platform_Node : DOM.Core.Node;
      Top_Node      : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Platform,
                   Kind => Muxml.Platform_Config,
                   File => Platform_File);
      Platform_Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/platform");

      Top_Node := DOM.Core.Documents.Local.Adopt_Node
        (Doc    => Policy.Doc,
         Source => DOM.Core.Documents.Local.Clone_Node
           (N    => DOM.Core.Documents.Get_Element (Doc => Platform.Doc),
            Deep => True));

      if Platform_Node = null then
         declare
            Sys_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system");
            Mem_Node : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Sys_Node,
                 XPath => "memory");
         begin
            Platform_Node := DOM.Core.Documents.Create_Element
              (Doc      => Policy.Doc,
               Tag_Name => "platform");
            Platform_Node := DOM.Core.Nodes.Insert_Before
              (N         => Sys_Node,
               New_Child => Platform_Node,
               Ref_Child => Mem_Node);
         end;
      else
         Muxml.Utils.Merge
           (Left      => Top_Node,
            Right     => Platform_Node,
            List_Tags => Platform_List_Tags);
      end if;

      Platform_Node := DOM.Core.Nodes.Replace_Child
        (N         => DOM.Core.Nodes.Parent_Node (N => Platform_Node),
         New_Child => Top_Node,
         Old_Child => Platform_Node);
      DOM.Core.Nodes.Free (N => Platform_Node);

      --  The platform document must not be freed since some resources
      --  referenced by the merged DOM tree are not copied to the Node's
      --  document. This can be removed as soon as XML/Ada supports import of
      --  nodes into a document.

      Platform.Doc := null;
      pragma Unreferenced (Platform);
   end Merge_Platform;

   -------------------------------------------------------------------------

   procedure Merge_XIncludes
     (Policy  : in out Muxml.XML_Data_Type;
      Basedir :        String)
   is
      Includes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "//include");
   begin
      if DOM.Core.Nodes.Length (List => Includes) = 0 then
         return;
      end if;

      for I in 0 .. DOM.Core.Nodes.Length (List => Includes) - 1 loop
         declare
            Inc_Node : DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Includes,
                 Index => I);
            Href     : constant String
              := Basedir & "/" & DOM.Core.Elements.Get_Attribute
                (Elem => Inc_Node,
                 Name => "href");
            Content  : Muxml.XML_Data_Type;
            Top_Node : DOM.Core.Node;
         begin
            Muxml.Parse (Data => Content,
                         Kind => Muxml.None,
                         File => Href);

            Merge_XIncludes (Policy  => Content,
                             Basedir => Basedir);
            Top_Node := DOM.Core.Documents.Local.Adopt_Node
              (Doc    => Policy.Doc,
               Source => DOM.Core.Documents.Local.Clone_Node
                 (N    => DOM.Core.Documents.Get_Element (Doc => Content.Doc),
                  Deep => True));

            Inc_Node :=  DOM.Core.Nodes.Replace_Child
              (N         => DOM.Core.Nodes.Parent_Node (N => Inc_Node),
               New_Child => Top_Node,
               Old_Child => Inc_Node);
            DOM.Core.Nodes.Free (N => Inc_Node);

            --  The included document must not be freed since some resources
            --  referenced by the merged DOM tree are not copied to the Node's
            --  document. This can be removed as soon as XML/Ada supports
            --  import of nodes into a document.

            Content.Doc := null;
            pragma Unreferenced (Content);
         end;
      end loop;
   end Merge_XIncludes;

end Mergers;
