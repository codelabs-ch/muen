--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

   Hardware_List_Tags : constant Muxml.Utils.Tags_Type
     := (1 => U ("device"),
         2 => U ("memoryBlock"),
         3 => U ("reservedMemory"));

   procedure Add_Missing_Elements (HW_Node : DOM.Core.Node);

   -------------------------------------------------------------------------

   procedure Add_Missing_Elements (HW_Node : DOM.Core.Node)
   is
   begin
      Muxml.Utils.Add_Child
        (Parent     => HW_Node,
         Child_Name => "devices");
      Muxml.Utils.Add_Child
        (Parent     => HW_Node,
         Child_Name => "memory",
         Ref_Names  => (1 => U ("devices")));
      Muxml.Utils.Add_Child
        (Parent     => HW_Node,
         Child_Name => "processor",
         Ref_Names  => (1 => U ("memory")));
   end Add_Missing_Elements;

   -------------------------------------------------------------------------

   procedure Merge_Hardware
     (Policy        : in out Muxml.XML_Data_Type;
      Hardware_File :        String)
   is
      use type DOM.Core.Node;

      Hardware      : Muxml.XML_Data_Type;
      Hardware_Node : DOM.Core.Node;
      Top_Node      : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Hardware,
                   Kind => Muxml.None,
                   File => Hardware_File);
      Hardware_Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/hardware");

      Top_Node := DOM.Core.Documents.Local.Adopt_Node
        (Doc    => Policy.Doc,
         Source => DOM.Core.Documents.Local.Clone_Node
           (N    => DOM.Core.Documents.Get_Element (Doc => Hardware.Doc),
            Deep => True));

      Add_Missing_Elements (HW_Node => Top_Node);

      if Hardware_Node = null then
         Muxml.Utils.Add_Child
           (Parent     => Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system"),
            Child_Name => "hardware",
            Ref_Names  => (1 => U ("platform"),
                           2 => U ("kernelDiagnosticsDevice")));
         Hardware_Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/hardware");
      else
         Add_Missing_Elements (HW_Node => Hardware_Node);
         Muxml.Utils.Merge
           (Left      => Top_Node,
            Right     => Hardware_Node,
            List_Tags => Hardware_List_Tags);
      end if;

      Hardware_Node := DOM.Core.Nodes.Replace_Child
        (N         => DOM.Core.Nodes.Parent_Node (N => Hardware_Node),
         New_Child => Top_Node,
         Old_Child => Hardware_Node);
      DOM.Core.Nodes.Free (N => Hardware_Node);
   end Merge_Hardware;

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
         end;
      end loop;
   end Merge_XIncludes;

end Mergers;
