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

with Interfaces.C.Extensions;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Muxml.Utils;
with Mutools.Types;

with Zp.Constants;

package body Zp.Utils
is

   package MT renames Mutools.Types;

   subtype Usable_Memory is
     MT.Subject_Memory with Static_Predicate => Usable_Memory in
       MT.Subject;

   subtype ACPI_Memory is
     MT.Subject_Memory with Static_Predicate => ACPI_Memory in
       MT.Subject_Acpi_Rsdp | MT.Subject_Acpi_Xsdt | MT.Subject_Acpi_Fadt
         | MT.Subject_Acpi_Dsdt;

   -------------------------------------------------------------------------

   function Create_e820_Map
     (Memory : DOM.Core.Node_List)
      return bootparam_h.boot_params_e820_map_array
   is
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => DOM.Core.Nodes.Owner_Document
             (N => DOM.Core.Nodes.Item
                (List  => Memory,
                 Index => 0)),
           XPath => "/system/memory/memory");
      e820_Map : bootparam_h.boot_params_e820_map_array := Null_e820_Map;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
         declare
            Virt_Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Memory,
               Index => I);
            Virt_Addr : constant Interfaces.C.Extensions.unsigned_long_long
              := Interfaces.C.Extensions.unsigned_long_long'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Virt_Node,
                    Name => "virtualAddress"));
            Phys_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Virt_Node,
               Name => "physical");
            Phys_Node : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Nodes     => Phys_Mem,
               Ref_Attr  => "name",
               Ref_Value => Phys_Name);
            Mem_Size : constant Interfaces.C.Extensions.unsigned_long_long
              := Interfaces.C.Extensions.unsigned_long_long'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => Phys_Node,
                    Name => "size"));
            Mem_Type : constant MT.Memory_Kind
              := MT.Memory_Kind'Value
                (DOM.Core.Elements.Get_Attribute (Elem => Phys_Node,
                                                  Name => "type"));
            C_Type : Interfaces.C.unsigned;
         begin
            case Mem_Type is
               when Usable_Memory =>
                  C_Type := Constants.E820_RAM;
               when ACPI_Memory =>
                  C_Type := Constants.E820_ACPI;
               when MT.Subject_Initrd =>
                  if Boolean'Value (DOM.Core.Elements.Get_Attribute
                                    (Elem => Virt_Node,
                                     Name => "writable"))
                  then
                     C_Type := Constants.E820_RAM;
                  else
                     C_Type := Constants.E820_RESERVED;
                  end if;
               when others =>
                  C_Type := Constants.E820_RESERVED;
            end case;

            e820_Map (I) := (addr   => Virt_Addr,
                             size   => Mem_Size,
                             c_type => C_Type);
         end;
      end loop;

      return e820_Map;
   end Create_e820_Map;

end Zp.Utils;
