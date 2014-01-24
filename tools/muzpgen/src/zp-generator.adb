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

with System;

with Interfaces.C;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Files;
with Mutools.Utils;

with bootparam_h;

package body Zp.Generator
is

   --  Memory size in bytes.
   Memory_Size      : constant := 256 * 1024 * 1024;
   Memory_Size_High : constant := Memory_Size - 16#400000#;

   --  arch/x86/include/uapi/asm/e820.h.
   E820_RAM      : constant := 1;
   E820_RESERVED : constant := 2;

   procedure C_Memset
     (S : System.Address;
      C : Interfaces.C.int;
      N : Interfaces.C.size_t);
   pragma Import (C, C_Memset, "memset");

   --  Return bootparams of subject associated with memory element given by
   --  name.
   function Get_Bootparams
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String;

   --  Return zero-page guest-physical address extracted from mapping of
   --  physical memory with given name.
   function Get_Guest_Physical
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String;

   --  Write Linux bootparams structure and specified command line to file
   --  given by filename. The size of the generated file is 4k + length (cmdl).
   --  The physical address argument designates the physical address of the
   --  zero-page in guest memory.
   procedure Write_ZP_File
     (Filename         : String;
      Cmdline          : String;
      Physical_Address : Natural);

   -------------------------------------------------------------------------

   function Get_Bootparams
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => XML_Data.Doc,
            XPath => "/system/subjects/subject[@name='"
            & Mutools.Utils.Decode_Entity_Name (Encoded_Str => Memory_Name)
            & "']/bootparams/text()"),
         Index => 0);
   begin
      if Node /= null then
         return DOM.Core.Nodes.Node_Value (N => Node);
      else
         return "";
      end if;
   end Get_Bootparams;

   -------------------------------------------------------------------------

   function Get_Guest_Physical
     (XML_Data    : Muxml.XML_Data_Type;
      Memory_Name : String)
      return String
   is
      use type DOM.Core.Node;

      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => XML_Data.Doc,
            XPath => "/system/subjects/subject/memory/memory/physical[@name='"
            & Memory_Name & "']/../@virtualAddress"),
         Index => 0);
   begin
      return DOM.Core.Nodes.Node_Value (N => Node);
   end Get_Guest_Physical;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Zps : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory/file[@format='zp']");
   begin
      Mulog.Log (Msg => "Found" & DOM.Core.Nodes.Length (List => Zps)'Img
                 & " zero-page file(s)");

      for I in 1 .. DOM.Core.Nodes.Length (List => Zps) loop
         declare
            Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Zps,
                                      Index => I - 1);
            Filename : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Node,
                 Name => "filename");
            Memname  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Node),
                 Name => "name");
            Physaddr : constant String
              := Get_Guest_Physical
                (XML_Data    => Policy,
                 Memory_Name => Memname);
         begin
            Mulog.Log (Msg => "Guest-physical address of " & Memname
                       & " zero-page is " & Physaddr);
            Write_ZP_File
              (Filename         => Output_Dir & "/" & Filename,
               Cmdline          => Get_Bootparams
                 (XML_Data    => Policy,
                  Memory_Name => Memname),
               Physical_Address => Natural'Value (Physaddr));
         end;
      end loop;

   end Write;

   -------------------------------------------------------------------------

   procedure Write_ZP_File
     (Filename         : String;
      Cmdline          : String;
      Physical_Address : Natural)
   is
      use Ada.Streams.Stream_IO;
      use type Interfaces.C.size_t;
      use type Interfaces.C.unsigned;

      File   : Ada.Streams.Stream_IO.File_Type;
      Params : bootparam_h.boot_params;
   begin
      Mulog.Log (Msg   => "Generating '" & Filename & "' with command line '"
                 & Cmdline & "'");

      C_Memset (S => Params'Address,
                C => 0,
                N => bootparam_h.boot_params'Object_Size / 8);

      Params.e820_entries := 4;

      --  Zero page incl. cmdl (8k), Time page, HVC and kbd channels

      Params.e820_map (0) := (addr   => 16#000000#,
                              size   => 16#005000#,
                              c_type => E820_RESERVED);

      --  Usable lower memory

      Params.e820_map (1) := (addr   => 16#005000#,
                              size   => 16#09e000#,
                              c_type => E820_RAM);

      --  VGA memory, OPROMs, BIOS extension (ACPI tables), System BIOS

      Params.e820_map (2) := (addr   => 16#0b8000#,
                              size   => 16#048000#,
                              c_type => E820_RESERVED);

      --  High memory

      Params.e820_map (3) := (addr   => 16#400000#,
                              size   => Memory_Size_High,
                              c_type => E820_RAM);

      Params.hdr.cmdline_size     := 16#0000_0fff#;
      Params.hdr.kernel_alignment := 16#0100_0000#;
      Params.hdr.cmd_line_ptr     := Interfaces.C.unsigned
        (Physical_Address) + 16#1000#;

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      bootparam_h.boot_params'Write (Stream (File => File), Params);

      --  Write command line

      String'Write (Stream (File => File), Cmdline);
      Character'Write (Stream (File => File), Character'Val (0));

      Close (File => File);
   end Write_ZP_File;

end Zp.Generator;
