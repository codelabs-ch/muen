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

with Ada.Strings.Fixed;

with Interfaces;

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.Constants;
with Mutools.XML_Utils;

with Expanders.XML_Utils;

package body Expanders.Memory
is

   --  Physical start address of VMX-related memory regions.
   VMX_Start_Address : constant Interfaces.Unsigned_64 := 16#1000#;

   -------------------------------------------------------------------------

   procedure Add_Alignment (Data : in out Muxml.XML_Data_Type)
   is
      Align : constant String := "16#1000#";
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/memory/memory[not(@alignment)]");
   begin
      Mulog.Log (Msg => "Adding alignment to"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img
                 & " memory region(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Mem : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
         begin
            DOM.Core.Elements.Set_Attribute
              (Elem  => Mem,
               Name  => "alignment",
               Value => Align);
         end;
      end loop;
   end Add_Alignment;

   -------------------------------------------------------------------------

   procedure Add_AP_Trampoline (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Mulog.Log (Msg => "Adding AP trampoline memory region");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "trampoline",
         Address     => "16#0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "system");
   end Add_AP_Trampoline;

   -------------------------------------------------------------------------

   procedure Add_Kernel_Binary (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Mulog.Log (Msg => "Adding kernel binary memory regions");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_text",
         Address     => "16#0010_0000#",
         Size        => "16#0001_0000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "kernel",
         File_Format => "bin_raw",
         File_Offset => "16#0000#",
         Memory_Type => "kernel_binary");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_data",
         Address     => "16#0011_0000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "kernel",
         File_Format => "bin_raw",
         File_Offset => "16#0001_0000#",
         Memory_Type => "kernel_binary");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_bss",
         Address     => "16#0011_1000#",
         Size        => "16#1000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         Memory_Type => "kernel_binary");
      Mutools.XML_Utils.Add_Memory_Region
        (Policy      => Data,
         Name        => "kernel_ro",
         Address     => "16#0011_f000#",
         Size        => "16#4000#",
         Caching     => "WB",
         Alignment   => "16#1000#",
         File_Name   => "kernel",
         File_Format => "bin_raw",
         File_Offset => "16#0001_f000#",
         Memory_Type => "kernel_binary");
   end Add_Kernel_Binary;

   -------------------------------------------------------------------------

   procedure Add_Kernel_PTs (Data : in out Muxml.XML_Data_Type)
   is
      Cur_Addr  : Interfaces.Unsigned_64 := 16#0012_9000#;
      CPU_Count : constant Positive      := Positive'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/processor",
            Name  => "logicalCpus"));

   begin
      for I in 0 .. CPU_Count - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            CPU_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
            Size : constant Interfaces.Unsigned_64
              := XML_Utils.Calculate_PT_Size
                (Policy             => Data,
                 Dev_Virt_Mem_XPath => "/system/kernel/devices/device/memory",
                 Virt_Mem_XPath     => "/system/kernel/memory/cpu[@id='" &
                   CPU_Str & "']/memory");
            Size_Str : constant String := Mutools.Utils.To_Hex
              (Number => Size);
         begin
            Mulog.Log (Msg => "Adding pagetable region with size " & Size_Str
                       & " at address "
                       & Mutools.Utils.To_Hex (Number => Cur_Addr)
                       & " for CPU " & CPU_Str);
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_" & CPU_Str & "|pt",
               Address     => Mutools.Utils.To_Hex (Number => Cur_Addr),
               Size        => Size_Str,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_pt",
               File_Name   => "kernel_pt_" & CPU_Str,
               File_Format => "pt",
               File_Offset => "none");

            Cur_Addr := Cur_Addr + Size;
         end;
      end loop;
   end Add_Kernel_PTs;

   -------------------------------------------------------------------------

   procedure Add_Stack_Store (Data : in out Muxml.XML_Data_Type)
   is
      CPU_Count : constant Positive := Positive'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/processor",
            Name  => "logicalCpus"));
   begin
      Mulog.Log (Msg => "Adding kernel stack and store memory regions for"
                 & CPU_Count'Img & " CPU(s)");

      for I in 0 .. CPU_Count - 1 loop
         declare
            CPU_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
         begin
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_stack_" & CPU_Str,
               Address     => "",
               Size        => "16#2000#",
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "kernel");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => "kernel_store_" & CPU_Str,
               Address     => "",
               Size        => "16#1000#",
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "kernel");
         end;
      end loop;
   end Add_Stack_Store;

   -------------------------------------------------------------------------

   procedure Add_Subject_Bitmaps (Data : in out Muxml.XML_Data_Type)
   is
      IOBM_Size  : constant := Mutools.Constants.Page_Size * 2;
      MSRBM_Size : constant := Mutools.Constants.Page_Size;
      Nodes      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
         begin
            Mulog.Log (Msg => "Adding I/O and MSR bitmap memory regions for"
                       & " subject '" & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|iobm",
               Address     => "",
               Size        => Mutools.Utils.To_Hex (Number => IOBM_Size),
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_iobm",
               File_Name   => Subj_Name & "_iobm",
               File_Format => "iobm",
               File_Offset => "none");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|msrbm",
               Address     => "",
               Size        => Mutools.Utils.To_Hex (Number => MSRBM_Size),
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_msrbm",
               File_Name   => Subj_Name & "_msrbm",
               File_Format => "msrbm",
               File_Offset => "none");
         end;
      end loop;
   end Add_Subject_Bitmaps;

   -------------------------------------------------------------------------

   procedure Add_Subject_PTs (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
            Size      : constant Interfaces.Unsigned_64
              := XML_Utils.Calculate_PT_Size
                (Policy             => Data,
                 Dev_Virt_Mem_XPath => "/system/subjects/subject[@name='"
                 & Subj_Name & "']/devices/device/memory",
                 Virt_Mem_XPath     => "/system/subjects/subject[@name='"
                 & Subj_Name & "']/memory/memory");
            Size_Str  : constant String := Mutools.Utils.To_Hex
              (Number => Size);
         begin
            Mulog.Log (Msg => "Adding pagetable region with size " & Size_Str
                       & " for subject '" & Subj_Name & "'");
            Mutools.XML_Utils.Add_Memory_Region
              (Policy      => Data,
               Name        => Subj_Name & "|pt",
               Address     => "",
               Size        => Size_Str,
               Caching     => "WB",
               Alignment   => "16#1000#",
               Memory_Type => "system_pt",
               File_Name   =>  Subj_Name & "_pt",
               File_Format => "pt",
               File_Offset => "none");
         end;
      end loop;
   end Add_Subject_PTs;

   -------------------------------------------------------------------------

   procedure Add_Subject_States (Data : in out Muxml.XML_Data_Type)
   is
      Nodes : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      Mulog.Log (Msg => "Adding state memory regions for"
                 & DOM.Core.Nodes.Length (List => Nodes)'Img & " subject(s)");

      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
         begin
            Mutools.XML_Utils.Add_Memory_Region
              (Policy    => Data,
               Name      => Subj_Name & "_state",
               Address   => "",
               Size      => "16#1000#",
               Caching   => "WB",
               Alignment => "16#1000#");
         end;
      end loop;
   end Add_Subject_States;

   -------------------------------------------------------------------------

   procedure Add_Tau0_Interface (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Mulog.Log (Msg => "Adding tau0 interface memory region");

      Mutools.XML_Utils.Add_Memory_Region
        (Policy    => Data,
         Name      => "sys_interface",
         Address   => "",
         Size      => "16#1000#",
         Caching   => "WB",
         Alignment => "16#1000#");
   end Add_Tau0_Interface;

   -------------------------------------------------------------------------

   procedure Add_VMCS_Regions (Data : in out Muxml.XML_Data_Type)
   is
      use type Interfaces.Unsigned_64;

      CPU_Count : constant Interfaces.Unsigned_64
        := Interfaces.Unsigned_64'Value
          (Muxml.Utils.Get_Attribute
             (Doc   => Data.Doc,
              XPath => "/system/platform/processor",
              Name  => "logicalCpus"));
      Curr_Addr : Interfaces.Unsigned_64 := VMX_Start_Address +
        CPU_Count * Mutools.Constants.Page_Size;
      Nodes     : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Data.Doc,
           XPath => "/system/subjects/subject");
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Nodes) - 1 loop
         declare
            Subj_Node : constant DOM.Core.Node
              := DOM.Core.Nodes.Item (List  => Nodes,
                                      Index => I);
            Subj_Name : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Subj_Node,
                 Name => "name");
         begin
            Mulog.Log (Msg => "Adding VMCS region for subject '"
                       & Subj_Name & "' at address "
                       & Mutools.Utils.To_Hex (Number => Curr_Addr));
            Mutools.XML_Utils.Add_Memory_Region
              (Policy    => Data,
               Name      => Subj_Name & "|vmcs",
               Address   => Mutools.Utils.To_Hex (Number => Curr_Addr),
               Size      => "16#1000#",
               Caching   => "WB",
               Alignment => "16#1000#");
            Curr_Addr := Curr_Addr + Mutools.Constants.Page_Size;
         end;
      end loop;
   end Add_VMCS_Regions;

   -------------------------------------------------------------------------

   procedure Add_VMXON_Regions (Data : in out Muxml.XML_Data_Type)
   is
      Curr_Addr : Interfaces.Unsigned_64 := VMX_Start_Address;
      CPU_Count : constant Positive      := Positive'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Data.Doc,
            XPath => "/system/platform/processor",
            Name  => "logicalCpus"));
   begin
      for I in 0 .. CPU_Count - 1 loop
         declare
            use type Interfaces.Unsigned_64;

            CPU_Str : constant String := Ada.Strings.Fixed.Trim
              (Source => I'Img,
               Side   => Ada.Strings.Left);
         begin
            Mulog.Log (Msg => "Adding VMXON region for CPU " & CPU_Str & " at "
                       & "address " & Mutools.Utils.To_Hex
                         (Number => Curr_Addr));
            Mutools.XML_Utils.Add_Memory_Region
              (Policy    => Data,
               Name      => "kernel_" & CPU_Str & "|vmxon",
               Address   => Mutools.Utils.To_Hex (Number => Curr_Addr),
               Size      => "16#1000#",
               Caching   => "WB",
               Alignment => "16#1000#");
            Curr_Addr := Curr_Addr + Mutools.Constants.Page_Size;
         end;
      end loop;
   end Add_VMXON_Regions;

end Expanders.Memory;
