--
--  Copyright (C) 2014, 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces.C.Extensions;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Files;
with Mutools.Utils;
with Mutools.Match;
with Mutools.XML_Utils;

with bootparam_h;

with Zp.Utils;

package body Zp.Generator
is

   use type Interfaces.Unsigned_64;

   --  Address where the Linux kernel is loaded after extraction.
   Kernel_Load_Addr   : constant Interfaces.Unsigned_64 := 16#0100_0000#;

   --  Initial boot pagetable size, see arc/x86/include/asm/boot.h
   Boot_Init_Pgt_Size : constant Interfaces.Unsigned_64 := 16#6000#;

   --  Mask to clear all sub-4K bits.
   Sub_Page_Size_Bits_Mask : constant Interfaces.Unsigned_64 := 16#ffff_f000#;

   procedure C_Memset
     (S : System.Address;
      C : Interfaces.C.int;
      N : Interfaces.C.size_t);
   pragma Import (C, C_Memset, "memset");

   --  Write Linux bootparams structure with ramdisk information and specified
   --  command line to file given by filename. The size of the generated file
   --  is 4k + length (cmdl). The init size parameter specifies the linear
   --  memory required during initialization of the Linux kernel.
   --  The physical address argument designates the logical address of the
   --  zero - page in guest memory. The subject memory nodes describe the
   --  virtual address space and are used to generate the e820 map.
   procedure Write_ZP_File
     (Filename         : String;
      Cmdline          : String;
      Init_Size        : Interfaces.Unsigned_64;
      Physical_Address : Interfaces.Unsigned_64;
      Ramdisk_Address  : Interfaces.Unsigned_64;
      Ramdisk_Size     : Interfaces.Unsigned_64;
      Subject_Memory   : DOM.Core.Node_List);

   --  Find initramfs start address and total size in given subject memory
   --  mappings. If no initramfs region is found, zero values are returned.
   procedure Get_Initramfs_Addr_And_Size
     (Subj_Mappings  :     DOM.Core.Node_List;
      Phys_Initramfs :     DOM.Core.Node_List;
      Virt_Addr      : out Interfaces.Unsigned_64;
      Size           : out Interfaces.Unsigned_64);

   --  Returns the size of memory from the kernel load address to the end of
   --  the enclosing memory region. If the kernel load memory region is larger
   --  than 32-bit it is clamped down to Unsigned_32'Last minus kernel load
   --  address and 6 * 4K (boot pagetables), rounded down to page size.
   function Get_Kernel_Load_Region_Size
     (Subject_Name : String;
      Physical_Mem : DOM.Core.Node_List;
      Logical_Mem  : DOM.Core.Node_List)
      return Interfaces.Unsigned_64
   with
      Post =>
         Get_Kernel_Load_Region_Size'Result + Kernel_Load_Addr
           + Boot_Init_Pgt_Size
           <= Interfaces.Unsigned_64 (Interfaces.Unsigned_32'Last);

   -------------------------------------------------------------------------

   procedure Get_Initramfs_Addr_And_Size
     (Subj_Mappings  :     DOM.Core.Node_List;
      Phys_Initramfs :     DOM.Core.Node_List;
      Virt_Addr      : out Interfaces.Unsigned_64;
      Size           : out Interfaces.Unsigned_64)
   is
      --  Get size value of given node.
      function Get_Size (Node : DOM.Core.Node) return String;

      ----------------------------------------------------------------------

      function Get_Size (Node : DOM.Core.Node) return String
      is
      begin
         return DOM.Core.Elements.Get_Attribute
           (Elem => Node,
            Name => "size");
      end Get_Size;

      Pairs       : Muxml.Utils.Matching_Pairs_Type;
      Unused_Addr : Interfaces.Unsigned_64;
   begin
      Virt_Addr := 0;
      Size      := 0;

      Pairs := Muxml.Utils.Get_Matching
        (Left_Nodes     => Subj_Mappings,
         Right_Nodes    => Phys_Initramfs,
         Match_Multiple => True,
         Match          => Mutools.Match.Is_Valid_Reference'Access);

      if DOM.Core.Nodes.Length (List => Pairs.Left) > 0 then
         Muxml.Utils.Get_Bounds (Nodes     => Pairs.Left,
                                 Attr_Name => "virtualAddress",
                                 Lower     => Virt_Addr,
                                 Upper     => Unused_Addr);
         Mutools.XML_Utils.Set_Memory_Size
           (Virtual_Mem_Nodes => Pairs.Left,
            Ref_Nodes         => Pairs.Right);
         Size := Muxml.Utils.Sum (Nodes  => Pairs.Left,
                                  Getter => Get_Size'Access);
      end if;
   end Get_Initramfs_Addr_And_Size;

   -------------------------------------------------------------------------

   function Get_Kernel_Load_Region_Size
     (Subject_Name : String;
      Physical_Mem : DOM.Core.Node_List;
      Logical_Mem  : DOM.Core.Node_List)
      return Interfaces.Unsigned_64
   is
      use type DOM.Core.Node;

      Load_Region : constant DOM.Core.Node
        :=  Mutools.XML_Utils.Get_Enclosing_Virtual_Region
          (Virtual_Address => Kernel_Load_Addr,
           Physical_Memory => Physical_Mem,
           Logical_Memory  => Logical_Mem);
   begin
      if Load_Region = null then
         raise Missing_Kernel_Load_Region with "Linux subject '" & Subject_Name
           & "' has no memory mapping at kernel load address "
           & Mutools.Utils.To_Hex (Number => Kernel_Load_Addr);
      end if;

      declare
         Base_Addr   : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Load_Region,
              Name => "virtualAddress");
         Phys_Name   : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Load_Region,
              Name => "physical");
         Phys_Size   : constant String
           := Muxml.Utils.Get_Attribute
             (Nodes     => Physical_Mem,
              Ref_Attr  => "name",
              Ref_Value => Phys_Name,
              Attr_Name => "size");
         Load_Size   : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (Base_Addr) + Interfaces.Unsigned_64'Value (Phys_Size)
           - Kernel_Load_Addr;
         Max_Size    : constant Interfaces.Unsigned_64
           := (Interfaces.Unsigned_64 (Interfaces.Unsigned_32'Last)
               - Kernel_Load_Addr - Boot_Init_Pgt_Size)
           and Sub_Page_Size_Bits_Mask;
      begin
         return Interfaces.Unsigned_64'Min
           (Load_Size,
            Max_Size);
      end;
   end Get_Kernel_Load_Region_Size;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects        : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Phys_Mem        : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Phys_Initrd_Mem : constant DOM.Core.Node_List
        := Muxml.Utils.Get_Elements
          (Nodes     => Phys_Mem,
           Ref_Attr  => "type",
           Ref_Value => "subject_initrd");
      Zps             : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory[@type='subject_zeropage']/file");
   begin
      Mulog.Log (Msg => "Found" & DOM.Core.Nodes.Length (List => Zps)'Img
                 & " zero-page file(s)");

      for I in 1 .. DOM.Core.Nodes.Length (List => Zps) loop
         declare
            Zp_Node     : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Zps,
                 Index => I - 1);
            Filename    : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Zp_Node,
                 Name => "filename");
            Memname     : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => DOM.Core.Nodes.Parent_Node (N => Zp_Node),
                 Name => "name");
            Subj_Name   : constant String
              := Mutools.Utils.Decode_Entity_Name (Encoded_Str => Memname);
            Subj_Node   : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Nodes     => Subjects,
                 Ref_Attr  => "name",
                 Ref_Value => Subj_Name);
            Subj_Memory : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subj_Node,
                 XPath => "memory/memory");
            Physaddr    : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Subj_Node,
                 XPath => "memory/memory[@logical='zero_page']",
                 Name  => "virtualAddress");
            Bootparams  : constant String
              := Muxml.Utils.Get_Element_Value
                (Doc   => Subj_Node,
                 XPath => "bootparams");
            Init_Size   : constant Interfaces.Unsigned_64
              := Get_Kernel_Load_Region_Size
                (Subject_Name => Subj_Name,
                 Physical_Mem => Phys_Mem,
                 Logical_Mem  => Subj_Memory);

            Initramfs_Address, Initramfs_Size : Interfaces.Unsigned_64 := 0;
         begin
            Mulog.Log (Msg => "Guest-physical address of '" & Memname
                       & "' zero-page is " & Physaddr);

            Mulog.Log (Msg => "Init size "
                       & Mutools.Utils.To_Hex (Number => Init_Size));

            Get_Initramfs_Addr_And_Size
              (Subj_Mappings  => Subj_Memory,
               Phys_Initramfs => Phys_Initrd_Mem,
               Virt_Addr      => Initramfs_Address,
               Size           => Initramfs_Size);
            if Initramfs_Address > 0 then
               Mulog.Log (Msg => "Declaring ramdisk of size "
                          & Mutools.Utils.To_Hex (Number => Initramfs_Size)
                          & " at address "
                          & Mutools.Utils.To_Hex (Number => Initramfs_Address)
                          & " in zero-page");
            end if;

            Write_ZP_File
              (Filename         => Output_Dir & "/" & Filename,
               Cmdline          => Bootparams,
               Init_Size        => Init_Size,
               Physical_Address => Interfaces.Unsigned_64'Value (Physaddr),
               Ramdisk_Address  => Initramfs_Address,
               Ramdisk_Size     => Initramfs_Size,
               Subject_Memory   => Subj_Memory);
         end;
      end loop;

   end Write;

   -------------------------------------------------------------------------

   procedure Write_ZP_File
     (Filename         : String;
      Cmdline          : String;
      Init_Size        : Interfaces.Unsigned_64;
      Physical_Address : Interfaces.Unsigned_64;
      Ramdisk_Address  : Interfaces.Unsigned_64;
      Ramdisk_Size     : Interfaces.Unsigned_64;
      Subject_Memory   : DOM.Core.Node_List)
   is
      use Ada.Streams.Stream_IO;
      use type Interfaces.C.size_t;
      use type Interfaces.C.unsigned;
      use type Interfaces.C.unsigned_char;

      File   : Ada.Streams.Stream_IO.File_Type;
      Params : bootparam_h.boot_params;
   begin
      Mulog.Log (Msg   => "Generating '" & Filename & "' with command line '"
                 & Cmdline & "'");

      C_Memset (S => Params'Address,
                C => 0,
                N => bootparam_h.boot_params'Object_Size / 8);

      Params.hdr.setup_sects  := 7;             --  Setup code size in 512-byte
      Params.hdr.boot_flag    := 16#aa55#;      --  Magic number
      Params.hdr.header       := 16#53726448#;  --  Magic signature "HdrS"
      Params.hdr.version      := 16#020d#;      --  Boot protocol version 2.13
      Params.hdr.loadflags    := 1;             --  LOADED_HIGH
      Params.hdr.pref_address                   --  Kernel load address
        := Interfaces.C.Extensions.unsigned_long_long (Kernel_Load_Addr);

      Params.hdr.type_of_loader := 16#ff#;
      Params.hdr.xloadflags     := 16#2#;       --  XLF_CAN_BE_LOADED_ABOVE_4G

      Params.e820_entries := Interfaces.C.unsigned_char
        (DOM.Core.Nodes.Length (List => Subject_Memory));
      Params.e820_map := Utils.Create_e820_Map (Memory => Subject_Memory);

      --  Initramfs

      Params.hdr.ramdisk_image
        := Interfaces.C.unsigned (Ramdisk_Address and 16#ffff_ffff#);
      Params.hdr.ramdisk_size
        := Interfaces.C.unsigned (Ramdisk_Size and 16#ffff_ffff#);

      Params.ext_ramdisk_image := Interfaces.C.unsigned
        (Ramdisk_Address / 2 ** 32);
      Params.ext_ramdisk_size  := Interfaces.C.unsigned
        (Ramdisk_Size / 2 ** 32);

      Params.hdr.init_size        := Interfaces.C.unsigned (Init_Size);
      Params.hdr.cmdline_size     := 16#0000_0fff#;
      Params.hdr.kernel_alignment := Interfaces.C.unsigned (Kernel_Load_Addr);
      Params.hdr.cmd_line_ptr     := Interfaces.C.unsigned
        (Physical_Address) + 16#1000#;

      --  Make vgacon.c work, values taken from 'arch/x86/xen/vga.c'.

      Params.the_screen_info.orig_video_mode   := 3;
      Params.the_screen_info.orig_video_isVGA  := 1;
      Params.the_screen_info.orig_video_lines  := 25;
      Params.the_screen_info.orig_video_cols   := 80;
      Params.the_screen_info.orig_video_ega_bx := 3;
      Params.the_screen_info.orig_video_points := 16;
      Params.the_screen_info.orig_y
        := Params.the_screen_info.orig_video_lines - 1;

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      bootparam_h.boot_params'Write (Stream (File => File), Params);

      --  Write command line

      String'Write (Stream (File => File), Cmdline);
      Character'Write (Stream (File => File), Character'Val (0));

      Close (File => File);
   end Write_ZP_File;

end Zp.Generator;
