--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Files;
with Mutools.Utils;

with Ukvm.Types;

package body Ukvm.Generator
is

   --  Return virtual address and size of memory region specified by logical
   --  name.
   procedure Get_Region_Dimension
     (Physical_Mem    :     DOM.Core.Node_List;
      Logical_Mem     :     DOM.Core.Node_List;
      Region_Name     :     String;
      Virtual_Address : out Interfaces.Unsigned_64;
      Size            : out Interfaces.Unsigned_64);

   --  Write Solo5/UKVM boot info structure with given command line to file
   --  specified by filename.
   procedure Write_BI_File
     (Filename  : String;
      Boot_Info : Types.UKVM_Boot_Info_Type;
      Cmdline   : String);

   -------------------------------------------------------------------------

   procedure Get_Region_Dimension
     (Physical_Mem    :     DOM.Core.Node_List;
      Logical_Mem     :     DOM.Core.Node_List;
      Region_Name     :     String;
      Virtual_Address : out Interfaces.Unsigned_64;
      Size            : out Interfaces.Unsigned_64)
   is
      Log_Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Nodes     => Logical_Mem,
           Ref_Attr  => "logical",
           Ref_Value => Region_Name);
      Addr_Str : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Log_Node,
           Name => "virtualAddress");
      Phys_Name : constant String
        := DOM.Core.Elements.Get_Attribute
          (Elem => Log_Node,
           Name => "physical");
      Size_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Nodes     => Physical_Mem,
           Ref_Attr  => "name",
           Ref_Value => Phys_Name,
           Attr_Name => "size");
   begin
      Virtual_Address := Interfaces.Unsigned_64'Value (Addr_Str);
      Size := Interfaces.Unsigned_64'Value (Size_Str);
   end Get_Region_Dimension;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Subj_Mem : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject/memory/memory");
      Ukvm_Infos : constant DOM.Core.Node_List
        := Muxml.Utils.Get_Elements (Nodes     => Phys_Mem,
                                     Ref_Attr  => "type",
                                     Ref_Value => "subject_ukvm_boot_info");
   begin
      Mulog.Log (Msg => "Found" & DOM.Core.Nodes.Length
                 (List => Ukvm_Infos)'Img & " UKVM boot info region(s)");

      for I in 1 .. DOM.Core.Nodes.Length (List => Ukvm_Infos) loop
         declare
            use type Interfaces.Unsigned_64;

            Info_Mem_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Ukvm_Infos,
                 Index => I - 1);
            Filename : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Info_Mem_Node,
                 XPath => "file",
                 Name  => "filename");
            Info_Mem_Phys_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Info_Mem_Node,
                 Name => "name");
            Info_Mem_Log_Mem : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Subj_Mem,
                 Ref_Attr  => "physical",
                 Ref_Value => Info_Mem_Phys_Name);
            Info_Log_Addr : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Info_Mem_Log_Mem,
                 Name => "virtualAddress");
            Subj_Node : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node
                (Node  => Info_Mem_Log_Mem,
                 Level => 2);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Subj_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
            Bootparams : constant String
              := Muxml.Utils.Get_Element_Value
                (Doc   => Subj_Node,
                 XPath => "bootparams");

            Boot_Info     : Types.UKVM_Boot_Info_Type := (others => 0);
            Address, Size : Interfaces.Unsigned_64;
         begin
            Get_Region_Dimension (Physical_Mem    => Phys_Mem,
                                  Logical_Mem     => Subj_Memory,
                                  Region_Name     => "ram",
                                  Virtual_Address => Address,
                                  Size            => Size);
            Boot_Info.Mem_Size := Address + Size;

            Get_Region_Dimension (Physical_Mem    => Phys_Mem,
                                  Logical_Mem     => Subj_Memory,
                                  Region_Name     => "binary",
                                  Virtual_Address => Address,
                                  Size            => Size);
            Boot_Info.Kernel_End := Address + Size - 1;
            Boot_Info.Cmdline    := Interfaces.Unsigned_64'Value
              (Info_Log_Addr) + Types.UKVM_Boot_Info_Type'Size / 8;

            Mulog.Log (Msg => "Writing UKVM boot info for subject '"
                       & Subj_Name & "' to '" & Filename & "': memory size "
                       & Mutools.Utils.To_Hex (Number => Boot_Info.Mem_Size)
                       & ", kernel end @ "
                       & Mutools.Utils.To_Hex (Number => Boot_Info.Kernel_End)
                       & ", cmdline @ "
                       & Mutools.Utils.To_Hex (Number => Boot_Info.Cmdline));
            Write_BI_File (Filename  => Output_Dir & "/" & Filename,
                           Boot_Info => Boot_Info,
                           Cmdline   => Bootparams);
         end;
      end loop;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_BI_File
     (Filename  : String;
      Boot_Info : Types.UKVM_Boot_Info_Type;
      Cmdline   : String)
   is
      use Ada.Streams.Stream_IO;

      File : Ada.Streams.Stream_IO.File_Type;
   begin
      Mulog.Log (Msg   => "Generating '" & Filename & "'"
                 & (if Cmdline'Length > 0 then "with command line '"
                   & Cmdline & "'" else ""));

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Types.UKVM_Boot_Info_Type'Write (Stream (File => File), Boot_Info);

      --  Write command line

      String'Write (Stream (File => File), Cmdline);
      Character'Write (Stream (File => File), Character'Val (0));

      Close (File => File);
   end Write_BI_File;

end Ukvm.Generator;
