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
with DOM.Core.Documents;

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
   begin
      Muxml.Parse (Data => Platform,
                   Kind => Muxml.Platform_Config,
                   File => Platform_File);

      Platform_Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/platform");

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
      end if;

      Muxml.Utils.Merge
        (Left      => Platform_Node,
         Right     => DOM.Core.Documents.Get_Element (Doc => Platform.Doc),
         List_Tags => Platform_List_Tags);

      --  The platform document must not be freed since some resources
      --  referenced by the merged DOM tree are not copied to the Node's
      --  document. This can be removed as soon as XML/Ada supports import of
      --  nodes into a document.

      Platform.Doc := null;
      pragma Unreferenced (Platform);
   end Merge_Platform;

end Mergers;
