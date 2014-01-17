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

with DOM.Core.Nodes;
with DOM.Core.Elements;

with McKae.XML.XPath.XIA;

with Mulog;
with Mutools.Files;
with Mutools.Constants;
with Muxml.Utils;

with Interfaces;

with Pt.Paging.EPT;

package body Pt.Generator
is

   type Paging_Type is (IA32e, EPT);

   --  Write pagetable files for each kernel/CPU as specified by the policy.
   procedure Write_Kernel_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write pagetable files for all subjects as specified by the policy.
   procedure Write_Subject_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Create paging structures from given memory regions and write them to the
   --  specified file. The PML4 address parameter specifies the physical start
   --  adddress of the PML4 paging structure. Depending on the given pagetable
   --  type IA-32e or EPT pagetables will be generated.
   procedure Write_Pagetable
     (Policy       : Muxml.XML_Data_Type;
      Memory       : DOM.Core.Node_List;
      Devices      : DOM.Core.Node_List;
      Pml4_Address : Interfaces.Unsigned_64;
      Filename     : String;
      PT_Type      : Paging_Type := IA32e);

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Write_Kernel_Pagetable (Output_Dir => Output_Dir,
                              Policy     => Policy);
      Write_Subject_Pagetable (Output_Dir => Output_Dir,
                               Policy     => Policy);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Kernel_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      CPUs : DOM.Core.Node_List;
   begin
      CPUs := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/kernel/memory/cpu");

      for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
         declare
            ID_Str : constant String := I'Img (I'Img'First + 1 .. I'Img'Last);

            Filename  : constant String := Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='kernel_" & ID_Str
               & "|pt']/file[@format='pt']",
               Name  => "filename");
            PML4_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                     (Doc   => Policy.Doc,
                      XPath => "/system/memory/memory[@name='kernel_" & ID_Str
                      & "|pt']",
                      Name  => "physicalAddress"));
            Nodes     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/kernel/memory/cpu[@id='" & ID_Str
                 & "']/memory");
            Devices   : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Policy.Doc,
                 XPath => "/system/kernel/devices/device[count(memory)>0]");
         begin
            Mulog.Log (Msg => "Writing kernel pagetable of CPU" & I'Img
                       & " to '" & Output_Dir & "/" & Filename & "'");
            Write_Pagetable
              (Policy       => Policy,
               Memory       => Nodes,
               Devices      => Devices,
               Pml4_Address => PML4_Addr,
               Filename     => Output_Dir & "/" & Filename,
               PT_Type      => IA32e);
         end;
      end loop;
   end Write_Kernel_Pagetable;

   -------------------------------------------------------------------------

   procedure Write_Pagetable
     (Policy       : Muxml.XML_Data_Type;
      Memory       : DOM.Core.Node_List;
      Devices      : DOM.Core.Node_List;
      Pml4_Address : Interfaces.Unsigned_64;
      Filename     : String;
      PT_Type      : Paging_Type := IA32e)
   is
      use Ada.Streams.Stream_IO;

      File : File_Type;
      PML4 : Paging.PML4_Table_Type := Paging.Null_PML4_Table;
      PDPT : Paging.PDP_Table_Type  := Paging.Null_PDP_Table;
      PD   : Paging.PD_Table_Type   := Paging.Null_PD_Table;
      PT   : Paging.Page_Table_Type := Paging.Null_Page_Table;

      --  Add mapping of given logical to physical memory.
      procedure Add_Mapping
        (Physical : DOM.Core.Node;
         Logical  : DOM.Core.Node);

      --  Add memory region with given attributes to pagetables.
      procedure Add_Memory_Region
        (Physical_Address : Interfaces.Unsigned_64;
         Virtual_Address  : Interfaces.Unsigned_64;
         Size             : Interfaces.Unsigned_64;
         Caching          : Paging.Caching_Type;
         Writable         : Boolean;
         Executable       : Boolean);

      ----------------------------------------------------------------------

      procedure Add_Mapping
        (Physical : DOM.Core.Node;
         Logical  : DOM.Core.Node)
      is
         Size    : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                  (Elem => Physical,
                   Name => "size"));
         PMA     : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                  (Elem => Physical,
                   Name => "physicalAddress"));
         VMA     : constant Interfaces.Unsigned_64
           := Interfaces.Unsigned_64'Value
             (DOM.Core.Elements.Get_Attribute
                  (Elem => Logical,
                   Name => "virtualAddress"));
         Write   : constant Boolean
           := Boolean'Value
             (DOM.Core.Elements.Get_Attribute
                  (Elem => Logical,
                   Name => "writable"));
         Exec    : constant Boolean
           := Boolean'Value
             (DOM.Core.Elements.Get_Attribute
                  (Elem => Logical,
                   Name => "executable"));
         Caching : constant Paging.Caching_Type
           := Paging.Caching_Type'Value
             (DOM.Core.Elements.Get_Attribute
                  (Elem => Physical,
                   Name => "caching"));
      begin
         Add_Memory_Region (Physical_Address => PMA,
                            Virtual_Address  => VMA,
                            Size             => Size,
                            Caching          => Caching,
                            Writable         => Write,
                            Executable       => Exec);
      end Add_Mapping;

      ----------------------------------------------------------------------

      procedure Add_Memory_Region
        (Physical_Address : Interfaces.Unsigned_64;
         Virtual_Address  : Interfaces.Unsigned_64;
         Size             : Interfaces.Unsigned_64;
         Caching          : Paging.Caching_Type;
         Writable         : Boolean;
         Executable       : Boolean)
      is
         use type Interfaces.Unsigned_64;
         use type Paging.PML4_Entry_Type;
         use type Paging.PDPT_Entry_Type;
         use type Paging.PD_Entry_Type;
         use type Paging.PT_Entry_Type;
         use type Paging.Table_Range;

         Is_PDPT_Page : constant Boolean := Size mod Paging.PDPT_Page_Size = 0;
         Is_PD_Page   : constant Boolean := Size mod Paging.PD_Page_Size = 0;
         Virt_End     : constant Interfaces.Unsigned_64
           := Virtual_Address + Size - 1;

         PML4_Idx_Start, PML4_Idx_End : Paging.Table_Range;
         PDPT_Idx_Start, PDPT_Idx_End : Paging.Table_Range;
         PD_Idx_Start, PD_Idx_End     : Paging.Table_Range;
         PT_Idx_Start, PT_Idx_End     : Paging.Table_Range;

         --  Physical start address of PDPT paging structure(s).
         PDPT_Addr : Interfaces.Unsigned_64;
         --  Physical start address of PD paging structure(s).
         PD_Addr   : Interfaces.Unsigned_64;
         --  Physical start address of PT paging structure(s).
         PT_Addr   : Interfaces.Unsigned_64;

         Physical_Addr : Interfaces.Unsigned_64 := Physical_Address;
      begin
         Paging.Get_Indexes (Address    => Virtual_Address,
                             PML4_Index => PML4_Idx_Start,
                             PDPT_Index => PDPT_Idx_Start,
                             PD_Index   => PD_Idx_Start,
                             PT_Index   => PT_Idx_Start);
         Paging.Get_Indexes (Address    => Virt_End,
                             PML4_Index => PML4_Idx_End,
                             PDPT_Index => PDPT_Idx_End,
                             PD_Index   => PD_Idx_End,
                             PT_Index   => PT_Idx_End);

         PDPT_Addr := Pml4_Address +
           (Interfaces.Unsigned_64
              (PML4_Idx_End) + 1) * Mutools.Constants.Page_Size;
         PD_Addr   := PDPT_Addr +
           (Interfaces.Unsigned_64
              (PDPT_Idx_End) + 1) * Mutools.Constants.Page_Size;
         PT_Addr   := PD_Addr +
           (Interfaces.Unsigned_64
              (PD_Idx_End) + 1) * Mutools.Constants.Page_Size;

         for Idx in Paging.Table_Range range PML4_Idx_Start .. PML4_Idx_End
         loop
            if PML4 (Idx) = Paging.PML4_Null_Entry then
               case PT_Type is
                  when IA32e =>
                     PML4 (Idx) := Paging.Create_PML4_Entry
                       (Address       => PDPT_Addr +
                          Interfaces.Unsigned_64
                            (Idx) * Mutools.Constants.Page_Size,
                        Writable      => True,
                        User_Access   => True,
                        Writethrough  => True,
                        Cache_Disable => False,
                        Exec_Disable  => False);
                  when EPT =>
                     PML4 (Idx) := Paging.EPT.Create_PML4_Entry
                       (Address    => PDPT_Addr +
                          Interfaces.Unsigned_64
                            (Idx) * Mutools.Constants.Page_Size,
                        Readable   => True,
                        Writable   => True,
                        Executable => True);
               end case;
            end if;
         end loop;

         for Idx in Paging.Table_Range range PDPT_Idx_Start .. PDPT_Idx_End
         loop
            if Is_PDPT_Page then
               PD_Addr := Physical_Addr +
                 Interfaces.Unsigned_64
                   (Idx - PDPT_Idx_Start) * Paging.PDPT_Page_Size;
            else
               PD_Addr := PD_Addr +
                 Interfaces.Unsigned_64
                   (Idx - PDPT_Idx_Start) * Mutools.Constants.Page_Size;
            end if;

            if PDPT (Idx) = Paging.PDPT_Null_Entry then
               case PT_Type is
                  when IA32e =>
                     PDPT (Idx) := Paging.Create_PDPT_Entry
                       (Address      => PD_Addr,
                        Writable     => not Is_PDPT_Page or Writable,
                        User_Access  => True,
                        Map_Page     => Is_PDPT_Page,
                        Global       => False,
                        Memory_Type  => Caching,
                        Exec_Disable => Is_PDPT_Page and not Executable);
                  when EPT =>
                     PDPT (Idx) := Paging.EPT.Create_PDPT_Entry
                       (Address     => PD_Addr,
                        Readable    => True,
                        Writable    => not Is_PDPT_Page or Writable,
                        Executable  => not Is_PDPT_Page or Executable,
                        Map_Page    => Is_PDPT_Page,
                        Ignore_PAT  => True,
                        Memory_Type => Caching);
               end case;
            end if;
         end loop;

         if Is_PDPT_Page then
            return;
         end if;

         for Idx in Paging.Table_Range range PD_Idx_Start .. PD_Idx_End loop
            if Is_PD_Page then
               PT_Addr := Physical_Addr +
                 Interfaces.Unsigned_64
                   (Idx - PD_Idx_Start) * Paging.PD_Page_Size;
            else
               PT_Addr := PT_Addr +
                 Interfaces.Unsigned_64
                   (Idx - PD_Idx_Start) * Mutools.Constants.Page_Size;
            end if;

            if PD (Idx) = Paging.PD_Null_Entry then
               case PT_Type is
                  when IA32e =>
                     PD (Idx) := Paging.Create_PD_Entry
                       (Address      => PT_Addr,
                        Writable     => not Is_PD_Page or Writable,
                        User_Access  => True,
                        Map_Page     => Is_PD_Page,
                        Global       => False,
                        Memory_Type  => Caching,
                        Exec_Disable => Is_PD_Page and not Executable);
                  when EPT =>
                     PD (Idx) := Paging.EPT.Create_PD_Entry
                       (Address     => PT_Addr,
                        Readable    => True,
                        Writable    => not Is_PD_Page or Writable,
                        Executable  => not Is_PD_Page or Executable,
                        Map_Page    => Is_PD_Page,
                        Ignore_PAT  => True,
                        Memory_Type => Caching);
               end case;
            end if;
         end loop;

         if Is_PD_Page then
            return;
         end if;

         for Idx in Paging.Table_Range range PT_Idx_Start .. PT_Idx_End loop
            if PT (Idx) = Paging.PT_Null_Entry then
               case PT_Type is
                  when IA32e =>
                     PT (Idx) := Paging.Create_PT_Entry
                       (Address      => Physical_Addr,
                        Writable     => Writable,
                        User_Access  => True,
                        Global       => False,
                        Memory_Type  => Caching,
                        Exec_Disable => not Executable);
                  when EPT =>
                     PT (Idx) := Paging.EPT.Create_PT_Entry
                       (Address     => Physical_Addr,
                        Readable    => True,
                        Writable    => Writable,
                        Executable  => Executable,
                        Map_Page    => True,
                        Ignore_PAT  => True,
                        Memory_Type => Caching);
               end case;
            end if;

            Physical_Addr := Physical_Addr + Mutools.Constants.Page_Size;
         end loop;
      end Add_Memory_Region;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Memory) - 1 loop
         declare
            Logical_Mem   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Memory,
                 Index => I);
            Logical_Name  : constant String
              := DOM.Core.Elements.Get_Attribute
                (Elem => Logical_Mem,
                 Name => "logical");
            Physical_Name : constant String
              := Muxml.Utils.Get_Attribute
                (Doc   => Logical_Mem,
                 XPath => "physical",
                 Name  => "name");
            Physical_Mem  : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => McKae.XML.XPath.XIA.XPath_Query
                     (N     => Policy.Doc,
                      XPath => "/system/memory/memory[@name='" & Physical_Name
                      & "']"),
                 Index => 0);
         begin
            Mulog.Log (Msg => "Adding region " & Logical_Name
                       & "[" & Physical_Name & "]");
            Add_Mapping (Physical => Physical_Mem,
                               Logical  => Logical_Mem);
         end;
      end loop;

      for I in 0 .. DOM.Core.Nodes.Length (List => Devices) - 1 loop
         declare
            Device   : constant DOM.Core.Node
              := DOM.Core.Nodes.Item
                (List  => Devices,
                 Index => I);
            Dev_Name : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Device,
               Name => "physical");
            Dev_Mem  : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Device,
                 XPath => "memory");
         begin
            for D in 0 .. DOM.Core.Nodes.Length (List => Dev_Mem) - 1 loop
               declare
                  Logical_Mem   : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => Dev_Mem,
                       Index => I);
                  Logical_Name  : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Logical_Mem,
                       Name => "logical");
                  Physical_Name : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => Logical_Mem,
                       Name => "physical");
                  Physical_Mem  : constant DOM.Core.Node
                    := DOM.Core.Nodes.Item
                      (List  => McKae.XML.XPath.XIA.XPath_Query
                           (N     => Policy.Doc,
                            XPath => "/system/platform/device[@name='"
                            & Dev_Name & "']/memory[@name='" & Physical_Name
                            & "']"),
                       Index => 0);
               begin
                  Mulog.Log (Msg => "Adding region " & Logical_Name
                             & "[" & Physical_Name & "] of device "
                             & Dev_Name);
                  Add_Mapping (Physical => Physical_Mem,
                               Logical  => Logical_Mem);
               end;
            end loop;
         end;
      end loop;

      Mutools.Files.Open (Filename => Filename,
                          File     => File);
      Paging.PML4_Table_Type'Write (Stream (File => File), PML4);
      Paging.PDP_Table_Type'Write  (Stream (File => File), PDPT);
      Paging.PD_Table_Type'Write   (Stream (File => File), PD);
      Paging.Page_Table_Type'Write (Stream (File => File), PT);
      Close (File => File);
   end Write_Pagetable;

   -------------------------------------------------------------------------

   procedure Write_Subject_Pagetable
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects : DOM.Core.Node_List;
   begin
      Subjects := McKae.XML.XPath.XIA.XPath_Query
        (N     => Policy.Doc,
         XPath => "/system/subjects/subject");

      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Name      : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => DOM.Core.Nodes.Item (List  => Subjects,
                                            Index => I),
               Name => "name");
            Filename  : constant String := Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='" & Name & "|pt']/"
               & "file[@format='pt']",
               Name  => "filename");
            PML4_Addr : constant Interfaces.Unsigned_64
              := Interfaces.Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                     (Doc   => Policy.Doc,
                      XPath => "/system/memory/memory[@name='" & Name
                      & "|pt']",
                      Name  => "physicalAddress"));
            Nodes     : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => DOM.Core.Nodes.Item
                     (List  => Subjects,
                      Index => I),
                 XPath => "memory/memory");
            Devices   : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => DOM.Core.Nodes.Item
                     (List  => Subjects,
                      Index => I),
                 XPath => "devices/device[count(memory)>0]");

            Paging : Paging_Type;
         begin
            if Muxml.Utils.Get_Element_Value
              (Doc   => DOM.Core.Nodes.Item (List  => Subjects,
                                             Index => I),
               XPath => "vcpu/vmx/controls/proc2/EnableEPT") = "1"
            then
               Paging := EPT;
            else
               Paging := IA32e;
            end if;

            Mulog.Log (Msg => "Writing " & Paging'Img & " pagetable of "
                       & Name & " to '" & Output_Dir & "/" & Filename & "'");
            Write_Pagetable
              (Policy       => Policy,
               Memory       => Nodes,
               Devices      => Devices,
               Pml4_Address => PML4_Addr,
               Filename     => Output_Dir & "/" & Filename,
               PT_Type      => Paging);
         end;
      end loop;
   end Write_Subject_Pagetable;

end Pt.Generator;
