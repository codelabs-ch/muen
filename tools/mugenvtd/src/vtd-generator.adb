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

with Ada.Streams.Stream_IO;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Paging.EPT;
with Paging.Layouts;

with Mulog;
with Muxml.Utils;
with Mutools.Files;

package body VTd.Generator
is

   --  Write device security domain pagetables as specified by the policy to
   --  the given output directory.
   procedure Write_Domain_Pagetables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Write_Domain_Pagetables (Output_Dir => Output_Dir,
                               Policy     => Policy);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Domain_Pagetables
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Domains : constant DOM.Core.Node_List := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/deviceDomains/domain");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Domains) - 1 loop
         declare
            Cur_Dom    : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Domains,
                                      Index => I);
            Dom_Name   : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Cur_Dom,
               Name => "name");
            PT_Node    : constant DOM.Core.Node := Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@type='system_pt' and "
               & "starts-with(string(@name),'" & Dom_Name & "')]");
            Filename   : constant String := Muxml.Utils.Get_Attribute
              (Doc   => PT_Node,
               XPath => "file",
               Name  => "filename");
            Tables_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (DOM.Core.Elements.Get_Attribute
                   (Elem => PT_Node,
                    Name => "physicalAddress"));
            Memory      : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Cur_Dom,
                 XPath => "memory/memory");
            Mem_Layout  : Paging.Layouts.Memory_Layout_Type (Levels => 3);
            File        : Ada.Streams.Stream_IO.File_Type;
         begin
            Mulog.Log (Msg => "Writing VT-d pagetable of device domain '"
                       & Dom_Name & "' to '" & Output_Dir & "/"
                       & Filename & "'");
            Paging.Layouts.Set_Large_Page_Support
              (Mem_Layout => Mem_Layout,
               State      => False);
            Paging.Layouts.Set_Address
              (Mem_Layout => Mem_Layout,
               Address    => Tables_Addr);

            for J in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
               declare
                  Mem_Node   : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item (List  => Memory,
                                            Index => J);
                  Log_Name   : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "logical");
                  Phys_Name  : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Mem_Node,
                       Name => "physical");
                  Phys_Node  : constant DOM.Core.Node
                    := Muxml.Utils.Get_Element
                      (Doc   => Policy.Doc,
                       XPath => "/system/memory/memory[@name='" & Phys_Name
                       & "']");
                  Size       : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "size"));
                  PMA        : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "physicalAddress"));
                  Mem_Type   : constant Paging.Caching_Type
                    := Paging.Caching_Type'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Phys_Node,
                          Name => "caching"));
                  VMA        : constant Interfaces.Unsigned_64
                    := Interfaces.Unsigned_64'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "virtualAddress"));
                  Executable : constant Boolean
                    := Boolean'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "executable"));
                  Writable   : constant Boolean
                    := Boolean'Value
                      (DOM.Core.Elements.Get_Attribute
                         (Elem => Mem_Node,
                          Name => "writable"));
               begin
                  Mulog.Log (Msg => "Adding region " & Log_Name
                             & "[" & Phys_Name & "] to domain '"
                             & Dom_Name & "'");
                  Paging.Layouts.Add_Memory_Region
                    (Mem_Layout       => Mem_Layout,
                     Physical_Address => PMA,
                     Virtual_Address  => VMA,
                     Size             => Size,
                     Caching          => Mem_Type,
                     Writable         => Writable,
                     Executable       => Executable);
               end;
            end loop;

            Paging.Layouts.Update_References (Mem_Layout => Mem_Layout);

            Mutools.Files.Open (Filename => Output_Dir & "/" & Filename,
                                File     => File);
            Paging.Layouts.Serialize
              (Stream      => Ada.Streams.Stream_IO.Stream (File),
               Mem_Layout  => Mem_Layout,
               Serializers =>
                 (1 => Paging.EPT.Serialize_PDPT'Access,
                  2 => Paging.EPT.Serialize_PD'Access,
                  3 => Paging.EPT.Serialize_PT'Access));
            Ada.Streams.Stream_IO.Close (File => File);
         end;
      end loop;
   end Write_Domain_Pagetables;

end VTd.Generator;
