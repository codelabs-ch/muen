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
with Muxml.Utils;

with Acpi.DSDT;
with Acpi.FADT;
with Acpi.RSDP;
with Acpi.XSDT;

package body Acpi.Generator
is

   --  Return filename of file-backed memory region with given name contained
   --  in given list of nodes.
   function Get_Filename
     (Memory : DOM.Core.Node_List;
      Name   : String)
      return String;

   -------------------------------------------------------------------------

   function Get_Filename
     (Memory : DOM.Core.Node_List;
      Name   : String)
      return String
   is
      use type DOM.Core.Node;

      Mem_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
        (Nodes     => Memory,
         Ref_Attr  => "name",
         Ref_Value => Name);
   begin
      return (if Mem_Node = null then "" else Muxml.Utils.Get_Attribute
              (Doc   => Mem_Node,
               XPath => "file",
               Name  => "filename"));
   end Get_Filename;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Physical_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Cur_Subj  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Subjects,
                 Index => I);
            Subj_Mem  : DOM.Core.Node_List;
            Name      : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Cur_Subj,
                 Name => "name");
            RSDP_Name : constant String := Name & "|acpi_rsdp";
            RSDP_File : constant String
              := Get_Filename (Memory => Physical_Mem,
                               Name   => RSDP_Name);
         begin
            if RSDP_File /= "" then
               Mulog.Log (Msg => "Generating ACPI tables of subject '"
                          & Name & "'");

               Subj_Mem := McKae.XML.XPath.XIA.XPath_Query
                 (N     => Cur_Subj,
                  XPath => "memory/memory");
               declare
                  XSDT_Name : constant String := Name & "|acpi_xsdt";
                  FADT_Name : constant String := Name & "|acpi_fadt";
                  DSDT_Name : constant String := Name & "|acpi_dsdt";

                  RSDP_Filename : constant String
                    := Output_Dir & "/" & RSDP_File;
                  XSDT_Addr     : constant String
                    := Muxml.Utils.Get_Attribute
                    (Nodes     => Subj_Mem,
                     Ref_Attr  => "physical",
                     Ref_Value => XSDT_Name,
                     Attr_Name => "virtualAddress");
                  XSDT_Filename : constant String
                    := Output_Dir & "/" & Get_Filename
                    (Memory => Physical_Mem,
                     Name   => XSDT_Name);

                  FADT_Addr     : constant String
                    := Muxml.Utils.Get_Attribute
                      (Nodes     => Subj_Mem,
                       Ref_Attr  => "physical",
                       Ref_Value => FADT_Name,
                       Attr_Name => "virtualAddress");
                  FADT_Filename : constant String
                    := Output_Dir & "/" & Get_Filename
                      (Memory => Physical_Mem,
                       Name   => FADT_Name);

                  DSDT_Addr     : constant String
                    := Muxml.Utils.Get_Attribute
                      (Nodes     => Subj_Mem,
                       Ref_Attr  => "physical",
                       Ref_Value => DSDT_Name,
                       Attr_Name => "virtualAddress");
                  DSDT_Filename : constant String
                    := Output_Dir & "/" & Get_Filename
                      (Memory => Physical_Mem,
                       Name   => DSDT_Name);
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

                  Mulog.Log (Msg => "Writing DSDT table to '"
                             & DSDT_Filename & "'");
                  Acpi.DSDT.Write
                    (Policy       => Policy,
                     Subject      => Cur_Subj,
                     Subject_Name => Name,
                     Filename     => DSDT_Filename);
               end;
            end if;
         end;
      end loop;
   end Write;

end Acpi.Generator;
