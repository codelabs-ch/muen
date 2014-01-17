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

with Interfaces;

with Mulog;

with Acpi.FADT;
with Acpi.RSDP;
with Acpi.XSDT;

package body Acpi.Generator
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Cur_Subj  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Name      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Subj,
                 Name => "name");
            RSDP_Name : constant String := Name & "|acpi_rsdp";
            RSDP_File : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/memory/memory[@name='" & RSDP_Name
                 & "']/file[@format='acpi_rsdp']/@filename");
         begin
            if DOM.Core.Nodes.Length (List => RSDP_File) /= 0 then
               Mulog.Log (Msg => "Generating ACPI tables of subject '"
                          & Name & "'");
               declare
                  XSDT_Name : constant String := Name & "|acpi_xsdt";
                  FADT_Name : constant String := Name & "|acpi_facp";
                  DSDT_Name : constant String := Name & "|acpi_dsdt";

                  RSDP_Filename : constant String
                    := Output_Dir & "/" & DOM.Core.Nodes.Node_Value
                      (N => DOM.Core.Nodes.Item
                           (List  => RSDP_File,
                            Index => 0));

                  XSDT_Addr     : constant String
                    := DOM.Core.Nodes.Node_Value
                      (N => DOM.Core.Nodes.Item
                           (List  => McKae.XML.XPath.XIA.XPath_Query
                                (N     => Cur_Subj,
                                 XPath => "memory/memory/physical[@name='"
                                 & XSDT_Name & "']/../@virtualAddress"),
                            Index => 0));
                  XSDT_Filename : constant String
                    := Output_Dir & "/" & DOM.Core.Nodes.Node_Value
                      (N => DOM.Core.Nodes.Item
                           (List  => McKae.XML.XPath.XIA.XPath_Query
                                (N     => Policy.Doc,
                                 XPath => "/system/memory/memory[@name='"
                                 & XSDT_Name & "']/file[@format='"
                                 & "acpi_xsdt']/@filename"),
                            Index => 0));

                  FADT_Addr     : constant String
                    := DOM.Core.Nodes.Node_Value
                      (N => DOM.Core.Nodes.Item
                           (List  => McKae.XML.XPath.XIA.XPath_Query
                                (N     => Cur_Subj,
                                 XPath => "memory/memory/physical[@name='"
                                 & FADT_Name & "']/../@virtualAddress"),
                            Index => 0));
                  FADT_Filename : constant String
                    := Output_Dir & "/" & DOM.Core.Nodes.Node_Value
                      (N => DOM.Core.Nodes.Item
                           (List  => McKae.XML.XPath.XIA.XPath_Query
                                (N     => Policy.Doc,
                                 XPath => "/system/memory/memory[@name='"
                                 & FADT_Name & "']/file[@format='"
                                 & "acpi_facp']/@filename"),
                            Index => 0));

                  DSDT_Addr : constant String
                    := DOM.Core.Nodes.Node_Value
                      (N => DOM.Core.Nodes.Item
                           (List  => McKae.XML.XPath.XIA.XPath_Query
                                (N     => Cur_Subj,
                                 XPath => "memory/memory/physical[@name='"
                                 & DSDT_Name & "']/../@virtualAddress"),
                            Index => 0));
               begin
                  Mulog.Log (Msg => "Writing RSDP with XSDT "
                             & "guest-physical address " & XSDT_Addr
                             & " to '" & RSDP_Filename & "'");
                  Acpi.RSDP.Write
                    (XSDT_Address => Interfaces.Unsigned_64'Value (XSDT_Addr),
                     Filename     => RSDP_Filename);

                  Mulog.Log (Msg => "Writing XSDT table with FADT "
                             & "guest-physical address " & FADT_Addr
                             & " to '" & XSDT_Filename & "'");
                  Acpi.XSDT.Write
                    (FADT_Address => Interfaces.Unsigned_64'Value (FADT_Addr),
                     Filename     => XSDT_Filename);

                  Mulog.Log (Msg => "Writing FADT table with DSDT "
                             & "guest-physical address " & DSDT_Addr
                             & " to '" & FADT_Filename & "'");
                  Acpi.FADT.Write
                    (DSDT_Address => Interfaces.Unsigned_64'Value (DSDT_Addr),
                     Filename     => FADT_Filename);
               end;
            end if;
         end;
      end loop;
   end Write;

end Acpi.Generator;
