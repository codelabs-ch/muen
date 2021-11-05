--
--  Copyright (C) 2014, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with DOM.Core.Documents.Local;
with McKae.XML.XPath.XIA;
with Muxml.Utils;

package body Mutools.Mergers
is
   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   procedure Merge_Config_Section
     (Policy     : in out Muxml.XML_Data_Type;
      New_Config :        DOM.Core.Node;
      Clone      :        Boolean := False)
   is
      use type DOM.Core.Node;

      System_Cfg : DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/config");
      New_Bools  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => New_Config,
           XPath => "boolean");
      New_Ints   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => New_Config,
           XPath => "integer");
      New_Strs   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => New_Config,
           XPath => "string");

      --  Add given nodes to global config by inserting them before the
      --  specified reference tags.
      procedure Add_To_Config
        (Nodes    : DOM.Core.Node_List;
         Ref_Tags : Muxml.Utils.Tags_Type);

      ----------------------------------------------------------------------

      procedure Add_To_Config
        (Nodes    : DOM.Core.Node_List;
         Ref_Tags : Muxml.Utils.Tags_Type)
      is
         Count : constant Natural := DOM.Core.Nodes.Length (List => Nodes);
      begin
         for I in 0 .. Count - 1 loop
            declare
               Node : DOM.Core.Node
                 := DOM.Core.Nodes.Item (List  => Nodes,
                                         Index => I);
            begin
               if Clone then
                  Node := DOM.Core.Documents.Local.Adopt_Node
                    (Doc    => Policy.Doc,
                     Source => DOM.Core.Documents.Local.Clone_Node
                       (N    => Node,
                        Deep => True));
               end if;
               Muxml.Utils.Insert_Before (Parent    => System_Cfg,
                                          New_Child => Node,
                                          Ref_Names => Ref_Tags);
            end;
         end loop;
      end Add_To_Config;
   begin
      if System_Cfg = null then
         declare
            Sys_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system");
         begin
            System_Cfg := DOM.Core.Documents.Create_Element
              (Doc      => Policy.Doc,
               Tag_Name => "config");
            System_Cfg := DOM.Core.Nodes.Insert_Before
              (N         => Sys_Node,
               New_Child => System_Cfg,
               Ref_Child => DOM.Core.Nodes.First_Child (N => Sys_Node));
         end;
      end if;

      if New_Config /= null then
         Add_To_Config (Nodes    => New_Bools,
                        Ref_Tags => (1 => U ("integer"),
                                     2 => U ("string")));
         Add_To_Config (Nodes    => New_Ints,
                        Ref_Tags => (1 => U ("string")));
         Add_To_Config (Nodes    => New_Strs,
                        Ref_Tags => Muxml.Utils.No_Tags);
      end if;
   end Merge_Config_Section;

end Mutools.Mergers;
