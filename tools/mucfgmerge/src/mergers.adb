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

with Mulog;
with Muxml.Utils;

package body Mergers
is

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   --  Add missing elements of given hardware section.
   procedure Add_Missing_HW_Elements (HW_Node : DOM.Core.Node);

   --  Add missing elements of given platform section.
   procedure Add_Missing_PL_Elements (PL_Node : DOM.Core.Node);

   --  Merge specified file into given policy as section specified by name. The
   --  given tags are treated as list elements during merge. If the section is
   --  missing in the policy, a new element is inserted before the given
   --  reference(s). Missing section elements are created by using the provided
   --  procedure.
   procedure Merge_Section
     (Policy            : in out Muxml.XML_Data_Type;
      Section_File      :        String;
      Section_Name      :        String;
      Section_List_Tags :        Muxml.Utils.Tags_Type;
      Section_Ref_Names :        Muxml.Utils.Tags_Type;
      Add_Missing_Elems : not null access procedure (Node : DOM.Core.Node));

   -------------------------------------------------------------------------

   procedure Add_Missing_HW_Elements (HW_Node : DOM.Core.Node)
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
   end Add_Missing_HW_Elements;

   -------------------------------------------------------------------------

   procedure Add_Missing_PL_Elements (PL_Node : DOM.Core.Node)
   is
      Mappings : DOM.Core.Node;
   begin
      Muxml.Utils.Add_Child
        (Parent     => PL_Node,
         Child_Name => "mappings");

      Mappings := Muxml.Utils.Get_Element
        (Doc   => PL_Node,
         XPath => "mappings");

      Muxml.Utils.Add_Child
        (Parent     => Mappings,
         Child_Name => "classes");
      Muxml.Utils.Add_Child
        (Parent     => Mappings,
         Child_Name => "aliases",
         Ref_Names  => (1 => U ("classes")));
   end Add_Missing_PL_Elements;

   -------------------------------------------------------------------------

   procedure Merge_Hardware
     (Policy        : in out Muxml.XML_Data_Type;
      Hardware_File :        String)
   is
   begin
      Merge_Section
        (Policy            => Policy,
         Section_File      => Hardware_File,
         Section_Name      => "hardware",
         Section_List_Tags => (1 => U ("device"),
                               2 => U ("memoryBlock"),
                               3 => U ("reservedMemory")),
         Section_Ref_Names => (1 => U ("platform"),
                               2 => U ("kernelDiagnosticsDevice")),
         Add_Missing_Elems => Add_Missing_HW_Elements'Access);
   end Merge_Hardware;

   -------------------------------------------------------------------------

   procedure Merge_Platform
     (Policy        : in out Muxml.XML_Data_Type;
      Platform_File :        String)
   is
   begin
      Merge_Section
        (Policy            => Policy,
         Section_File      => Platform_File,
         Section_Name      => "platform",
         Section_List_Tags => (1 => U ("alias"),
                               2 => U ("resource"),
                               3 => U ("class"),
                               4 => U ("device")),
         Section_Ref_Names => (1 => U ("kernelDiagnosticsDevice")),
         Add_Missing_Elems => Add_Missing_PL_Elements'Access);
   end Merge_Platform;

   -------------------------------------------------------------------------

   procedure Merge_Section
     (Policy            : in out Muxml.XML_Data_Type;
      Section_File      :        String;
      Section_Name      :        String;
      Section_List_Tags :        Muxml.Utils.Tags_Type;
      Section_Ref_Names :        Muxml.Utils.Tags_Type;
      Add_Missing_Elems : not null access procedure (Node : DOM.Core.Node))
   is
      use type DOM.Core.Node;

      Section      : Muxml.XML_Data_Type;
      Section_Node : DOM.Core.Node;
      Top_Node     : DOM.Core.Node;
   begin
      Muxml.Parse (Data => Section,
                   Kind => Muxml.None,
                   File => Section_File);
      Section_Node := Muxml.Utils.Get_Element
        (Doc   => Policy.Doc,
         XPath => "/system/" & Section_Name);

      Top_Node := DOM.Core.Documents.Local.Adopt_Node
        (Doc    => Policy.Doc,
         Source => DOM.Core.Documents.Local.Clone_Node
           (N    => DOM.Core.Documents.Get_Element (Doc => Section.Doc),
            Deep => True));

      Add_Missing_Elems (Node => Top_Node);

      if Section_Node = null then
         Muxml.Utils.Add_Child
           (Parent     => Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system"),
            Child_Name => Section_Name,
            Ref_Names  => Section_Ref_Names);
         Section_Node := Muxml.Utils.Get_Element
           (Doc   => Policy.Doc,
            XPath => "/system/" & Section_Name);
      else
         Add_Missing_Elems (Node => Section_Node);
         Muxml.Utils.Merge
           (Left      => Top_Node,
            Right     => Section_Node,
            List_Tags => Section_List_Tags);
      end if;

      Section_Node := DOM.Core.Nodes.Replace_Child
        (N         => DOM.Core.Nodes.Parent_Node (N => Section_Node),
         New_Child => Top_Node,
         Old_Child => Section_Node);
      DOM.Core.Nodes.Free (N => Section_Node);
   end Merge_Section;

   -------------------------------------------------------------------------

   procedure Merge_XIncludes
     (Policy    : in out Muxml.XML_Data_Type;
      Base_Dirs :        Merge.Utils.String_Array)
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
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Inc_Node,
                 Name => "href");
            Path     : constant String
              := Merge.Utils.Lookup_File
                (Filename    => Filename,
                 Directories => Base_Dirs);
            Content  : Muxml.XML_Data_Type;
            Top_Node : DOM.Core.Node;
         begin
            Muxml.Parse (Data => Content,
                         Kind => Muxml.None,
                         File => Path);

            Mulog.Log (Msg => "Merging included file '" & Path & "'");
            Merge_XIncludes (Policy    => Content,
                             Base_Dirs => Base_Dirs);
            Top_Node := DOM.Core.Documents.Local.Adopt_Node
              (Doc    => Policy.Doc,
               Source => DOM.Core.Documents.Local.Clone_Node
                 (N    => DOM.Core.Documents.Get_Element (Doc => Content.Doc),
                  Deep => True));

            Inc_Node := DOM.Core.Nodes.Replace_Child
              (N         => DOM.Core.Nodes.Parent_Node (N => Inc_Node),
               New_Child => Top_Node,
               Old_Child => Inc_Node);
            DOM.Core.Nodes.Free (N => Inc_Node);
         end;
      end loop;
   end Merge_XIncludes;

end Mergers;
