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

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;

package body Validators.Scheduling
is

   use McKae.XML.XPath.XIA;

   -------------------------------------------------------------------------

   procedure CPU_Element_Count (XML_Data : Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive
        := Positive'Value
          (Muxml.Utils.Get_Attribute
               (Doc   => XML_Data.Doc,
                XPath => "/system/platform/processor",
                Name  => "logicalCpus"));
      Majors    : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "/system/scheduling/majorFrame");
   begin
      Mulog.Log (Msg => "Checking CPU element count in" & DOM.Core.Nodes.Length
                 (List => Majors)'Img & " scheduling major frame(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Majors) - 1 loop
         declare
            CPUs : constant DOM.Core.Node_List
              := XPath_Query (N     => DOM.Core.Nodes.Item
                              (List  => Majors,
                               Index => I),
                              XPath => "cpu");
         begin
            if DOM.Core.Nodes.Length (List => CPUs) /= CPU_Count then
               raise Validation_Error with "CPU element count of"
                 & DOM.Core.Nodes.Length (List => CPUs)'Img & " in scheduling "
                 & "plan invalid, logical CPU count is" & CPU_Count'Img;
            end if;
         end;
      end loop;
   end CPU_Element_Count;

   -------------------------------------------------------------------------

   procedure Subject_References (XML_Data : Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := XPath_Query (N     => XML_Data.Doc,
                        XPath => "//minorFrame/@subject");
   begin
      Mulog.Log (Msg => "Checking" & DOM.Core.Nodes.Length
                 (List => Nodes)'Img & " subject reference(s) in scheduling "
                 & "plan");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Name : constant String
              := DOM.Core.Nodes.Node_Value
                (N => DOM.Core.Nodes.Item
                     (List  => Nodes,
                      Index => I));
            Subjects  : constant DOM.Core.Node_List
              := XPath_Query (N     => XML_Data.Doc,
                              XPath => "//subject[@name='" & Subj_Name & "']");
         begin
            if DOM.Core.Nodes.Length (List => Subjects) = 0 then
               raise Validation_Error with "Subject '" & Subj_Name
                 & "' referenced in scheduling plan not found";
            end if;
         end;
      end loop;
   end Subject_References;

end Validators.Scheduling;
