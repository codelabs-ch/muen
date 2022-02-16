--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with McKae.XML.XPath.XIA;
with Mucfgcheck.Validation_Errors;
with Mulog;

package body Mucfgcheck.Templates
is

   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
            := McKae.XML.XPath.XIA.XPath_Query
                  (N     => XML_Data.Doc,
                   XPath => "//template");
   begin
      Mulog.Log (Msg => "Checking uniqueness of"
                        & DOM.Core.Nodes.Length (List => Nodes)'Img
                        & " template name(s)");

      Mucfgcheck.Attr_Uniqueness
         (Nodes     => Nodes,
          Attr_Name => "name",
          Error_Msg => "Template names are not unique.");
   end Name_Uniqueness;

   ------------------------------------------------------------------------

   procedure Template_Integrity (XML_Data : Muxml.XML_Data_Type)
   is
      Templates : constant DOM.Core.Node_List
                := McKae.XML.XPath.XIA.XPath_Query
                      (N     => XML_Data.Doc,
                       XPath => "//template");
   begin
      Mulog.Log (Msg => "Checking integrity of"
                        & DOM.Core.Nodes.Length (List => Templates)'Img
                        & " template(s)");
      for I in 0 .. DOM.Core.Nodes.Length (List => Templates) - 1 loop
         declare
            Template             : constant DOM.Core.Node
                                 := DOM.Core.Nodes.Item
                                       (List  => Templates,
                                        Index => I);
            Template_Body        : constant DOM.Core.Node_List
                                 := McKae.XML.XPath.XIA.XPath_Query
                                       (N     => Template,
                                        XPath => "./body");
            Template_Parameters  : constant DOM.Core.Node_List
                                 := McKae.XML.XPath.XIA.XPath_Query
                                       (N     => Template,
                                        XPath => "./parameters");
            Template_Expressions : constant DOM.Core.Node_List
                                 := McKae.XML.XPath.XIA.XPath_Query
                                       (N     => Template,
                                        XPath => "./expressions");
         begin
            case DOM.Core.Nodes.Length (List => Template_Body) is
               when 0 =>
                  Mucfgcheck.Validation_Errors.Insert
                  (Msg => "Found template definition without body.");
               when 1 =>
                  null;
               when others =>
                  Mucfgcheck.Validation_Errors.Insert
                     (Msg => "Found template definition with multiple bodies.");
            end case;

            case DOM.Core.Nodes.Length (List => Template_Parameters) is
               when 0 =>
                  Mucfgcheck.Validation_Errors.Insert
                     (Msg => "Found template definition"
                             & " without parameter declaration.");
               when 1 =>
                  null;
               when others =>
               Mucfgcheck.Validation_Errors.Insert
                  (Msg => "Found template definition"
                          & " with multiple parameter blocks.");
            end case;

            case DOM.Core.Nodes.Length (List => Template_Expressions) is
               when 0 | 1 =>
                  null;
               when others =>
               Mucfgcheck.Validation_Errors.Insert
                  (Msg => "Found template definition"
                          & " with multiple expressions blocks.");
            end case;
         end;
      end loop;

   end Template_Integrity;

end Mucfgcheck.Templates;
