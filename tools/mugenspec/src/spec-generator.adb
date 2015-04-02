--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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
with Ada.Strings.Unbounded;

with Interfaces;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;
with Muxml.Utils;
with Mutools.Utils;
with Mutools.XML_Utils;
with Mutools.Templates;
with Mutools.Constants;
with Mutools.Match;

with Spec.Utils;
with Spec.Kernel;
with Spec.Scheduling;

with String_Templates;

pragma Elaborate_All (Spec.Utils);

package body Spec.Generator
is

   use Ada.Strings.Unbounded;
   use Interfaces;

   function U
     (Source : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   type Pin_Ctrl_Type is
     (ExternalInterruptExiting,
      NMIExiting,
      VirtualNMIs,
      ActivateVMXTimer,
      ProcessPostedInterrupts);

   type Pin_Ctrl_Map_Type is array (Pin_Ctrl_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  Pin control bit positions as specified by Intel SDM Vol. 3C, table 24-5.
   function Get_Pin_Controls is new Utils.To_Number
     (Bitfield_Type => Pin_Ctrl_Type,
      Mapping_Type  => Pin_Ctrl_Map_Type,
      Map           =>
        (ExternalInterruptExiting => 0,
         NMIExiting               => 3,
         VirtualNMIs              => 5,
         ActivateVMXTimer         => 6,
         ProcessPostedInterrupts  => 7));

   type Proc_Ctrl_Type is
     (InterruptWindowExiting,
      UseTSCOffsetting,
      HLTExiting,
      INVLPGExiting,
      MWAITExiting,
      RDPMCExiting,
      RDTSCExiting,
      CR3LoadExiting,
      CR3StoreExiting,
      CR8LoadExiting,
      CR8StoreExiting,
      UseTPRShadow,
      NMIWindowExiting,
      MOVDRExiting,
      UnconditionalIOExiting,
      UseIOBitmaps,
      MonitorTrapFlag,
      UseMSRBitmaps,
      MONITORExiting,
      PAUSEExiting,
      Activate2ndaryControls);

   type Proc_Ctrl_Map_Type is array (Proc_Ctrl_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  Proc control bit positions as specified by Intel SDM Vol. 3C, table
   --  24-6.
   function Get_Proc_Controls is new Utils.To_Number
     (Bitfield_Type => Proc_Ctrl_Type,
      Mapping_Type  => Proc_Ctrl_Map_Type,
      Map           =>
        (InterruptWindowExiting => 2,
         UseTSCOffsetting       => 3,
         HLTExiting             => 7,
         INVLPGExiting          => 9,
         MWAITExiting           => 10,
         RDPMCExiting           => 11,
         RDTSCExiting           => 12,
         CR3LoadExiting         => 15,
         CR3StoreExiting        => 16,
         CR8LoadExiting         => 19,
         CR8StoreExiting        => 20,
         UseTPRShadow           => 21,
         NMIWindowExiting       => 22,
         MOVDRExiting           => 23,
         UnconditionalIOExiting => 24,
         UseIOBitmaps           => 25,
         MonitorTrapFlag        => 27,
         UseMSRBitmaps          => 28,
         MONITORExiting         => 29,
         PAUSEExiting           => 30,
         Activate2ndaryControls => 31));

   type Proc2_Ctrl_Type is
     (VirtualAPICAccesses,
      EnableEPT,
      DescriptorTableExiting,
      EnableRDTSCP,
      Virtualizex2APICMode,
      EnableVPID,
      WBINVDExiting,
      UnrestrictedGuest,
      APICRegisterVirtualization,
      VirtualInterruptDelivery,
      PAUSELoopExiting,
      RDRANDExiting,
      EnableINVPCID,
      EnableVMFunctions);

   type Proc2_Ctrl_Map_Type is array (Proc2_Ctrl_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  Secondary proc control bit positions as specified by Intel SDM Vol. 3C,
   --  table 24-7.
   function Get_Proc2_Controls is new Utils.To_Number
     (Bitfield_Type => Proc2_Ctrl_Type,
      Mapping_Type  => Proc2_Ctrl_Map_Type,
      Map           =>
        (VirtualAPICAccesses        => 0,
         EnableEPT                  => 1,
         DescriptorTableExiting     => 2,
         EnableRDTSCP               => 3,
         Virtualizex2APICMode       => 4,
         EnableVPID                 => 5,
         WBINVDExiting              => 6,
         UnrestrictedGuest          => 7,
         APICRegisterVirtualization => 8,
         VirtualInterruptDelivery   => 9,
         PAUSELoopExiting           => 10,
         RDRANDExiting              => 11,
         EnableINVPCID              => 12,
         EnableVMFunctions          => 13));

   type Entry_Ctrl_Type is
     (LoadDebugControls,
      IA32eModeGuest,
      EntryToSMM,
      DeactiveDualMonitorTreatment,
      LoadIA32PERFGLOBALCTRL,
      LoadIA32PAT,
      LoadIA32EFER);

   type Entry_Ctrl_Map_Type is array (Entry_Ctrl_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  VM-Entry control bit positions as specified by Intel SDM Vol. 3C,
   --  table 24-12.
   function Get_Entry_Controls is new Utils.To_Number
     (Bitfield_Type => Entry_Ctrl_Type,
      Mapping_Type  => Entry_Ctrl_Map_Type,
      Map           =>
        (LoadDebugControls            => 2,
         IA32eModeGuest               => 9,
         EntryToSMM                   => 10,
         DeactiveDualMonitorTreatment => 11,
         LoadIA32PERFGLOBALCTRL       => 13,
         LoadIA32PAT                  => 14,
         LoadIA32EFER                 => 15));

   type Exit_Ctrl_Type is
     (SaveDebugControls,
      HostAddressspaceSize,
      LoadIA32PERFGLOBALCTRL,
      AckInterruptOnExit,
      SaveIA32PAT,
      LoadIA32PAT,
      SaveIA32EFER,
      LoadIA32EFER,
      SaveVMXTimerValue);

   type Exit_Ctrl_Map_Type is array (Exit_Ctrl_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  VM-Exit control bit positions as specified by Intel SDM Vol. 3C,
   --  table 24-10.
   function Get_Exit_Controls is new Utils.To_Number
     (Bitfield_Type => Exit_Ctrl_Type,
      Mapping_Type  => Exit_Ctrl_Map_Type,
      Map           =>
        (SaveDebugControls      => 2,
         HostAddressspaceSize   => 9,
         LoadIA32PERFGLOBALCTRL => 12,
         AckInterruptOnExit     => 15,
         SaveIA32PAT            => 18,
         LoadIA32PAT            => 19,
         SaveIA32EFER           => 20,
         LoadIA32EFER           => 21,
         SaveVMXTimerValue      => 22));

   type CR0_Flags_Type is
     (ProtectionEnable,
      MonitorCoprocessor,
      Emulation,
      TaskSwitched,
      ExtensionType,
      NumericError,
      WriteProtect,
      AlignmentMask,
      NotWritethrough,
      CacheDisable,
      Paging);

   type CR0_Flags_Map_Type is array (CR0_Flags_Type)
     of Mutools.Utils.Unsigned_64_Pos;
   --  CR0 flag bit positions as specified by Intel SDM Vol. 3A, section 2.5.
   function Get_CR0 is new Utils.To_Number
     (Bitfield_Type => CR0_Flags_Type,
      Mapping_Type  => CR0_Flags_Map_Type,
      Map           =>
        (ProtectionEnable   => 0,
         MonitorCoprocessor => 1,
         Emulation          => 2,
         TaskSwitched       => 3,
         ExtensionType      => 4,
         NumericError       => 5,
         WriteProtect       => 16,
         AlignmentMask      => 18,
         NotWritethrough    => 29,
         CacheDisable       => 30,
         Paging             => 31));

   type CR4_Flags_Type is
     (Virtual8086,
      ProtectedVirtualInts,
      TimeStampDisable,
      DebuggingExtensions,
      PageSizeExtensions,
      PhysicalAddressExtension,
      MachineCheckEnable,
      PageGlobalEnable,
      PerfCounterEnable,
      OSSupportFXSAVE,
      OSSupportSIMDExceptions,
      VMXEnable,
      SMXEnable,
      FSGSBASEEnable,
      PCIDEnable,
      XSAVEEnable,
      SMEPEnable);

   type CR4_Flags_Map_Type is array (CR4_Flags_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  CR4 flag bit positions as specified by Intel SDM Vol. 3A, section 2.5.
   function Get_CR4 is new Utils.To_Number
     (Bitfield_Type => CR4_Flags_Type,
      Mapping_Type  => CR4_Flags_Map_Type,
      Map           =>
        (Virtual8086              => 0,
         ProtectedVirtualInts     => 1,
         TimeStampDisable         => 2,
         DebuggingExtensions      => 3,
         PageSizeExtensions       => 4,
         PhysicalAddressExtension => 5,
         MachineCheckEnable       => 6,
         PageGlobalEnable         => 7,
         PerfCounterEnable        => 8,
         OSSupportFXSAVE          => 9,
         OSSupportSIMDExceptions  => 10,
         VMXEnable                => 13,
         SMXEnable                => 14,
         FSGSBASEEnable           => 16,
         PCIDEnable               => 17,
         XSAVEEnable              => 18,
         SMEPEnable               => 20));

   type Exceptions_Type is
     (DivideError,
      NMI,
      Breakpoint,
      Overflow,
      BOUNDRangeExceeded,
      InvalidOpcode,
      DeviceNotAvailable,
      DoubleFault,
      CoprocessorSegmentOverrun,
      InvalidTSS,
      SegmentNotPresent,
      StackSegmentFault,
      GeneralProtection,
      PageFault,
      x87FPUFloatingPointError,
      AlignmentCheck,
      MachineCheck,
      SIMDFloatingPointException);

   type Exceptions_Map_Type is array (Exceptions_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  Exceptions bit positions as specified by Intel SDM Vol. 3A, table 6.3.1.
   function Get_Exceptions is new Utils.To_Number
     (Bitfield_Type => Exceptions_Type,
      Mapping_Type  => Exceptions_Map_Type,
      Map           =>
        (DivideError                => 0,
         NMI                        => 2,
         Breakpoint                 => 3,
         Overflow                   => 4,
         BOUNDRangeExceeded         => 5,
         InvalidOpcode              => 6,
         DeviceNotAvailable         => 7,
         DoubleFault                => 8,
         CoprocessorSegmentOverrun  => 9,
         InvalidTSS                 => 10,
         SegmentNotPresent          => 11,
         StackSegmentFault          => 12,
         GeneralProtection          => 13,
         PageFault                  => 14,
         x87FPUFloatingPointError   => 16,
         AlignmentCheck             => 17,
         MachineCheck               => 18,
         SIMDFloatingPointException => 19));

   --  Returns the kernel PML4 addresses as string for inclusion in policy.h.
   function Get_Kernel_PML4_Addrs
     (Physical_Memory : DOM.Core.Node_List;
      CPU_Count       : Positive)
      return String;

   --  Write hardware-related policy file to specified output directory.
   procedure Write_Hardware
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write interrupt policy file to specified output directory.
   procedure Write_Interrupts
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write IOMMU-related policy file to specified output directory.
   procedure Write_IOMMU
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write kernel-related policy files to specified output directory.
   procedure Write_Kernel
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write subject-related policy file to specified output directory.
   procedure Write_Subject
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write toplevel system-related policy file to specified output directory.
   procedure Write_System
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   function Get_Kernel_PML4_Addrs
     (Physical_Memory : DOM.Core.Node_List;
      CPU_Count       : Positive)
      return String
   is
      Buffer : Unbounded_String;
   begin
      for I in Natural range 0 .. CPU_Count - 1 loop
         declare
            CPU_ID    : constant String
              := Ada.Strings.Fixed.Trim
                (Source  => I'Img,
                 Side    => Ada.Strings.Left);
            PML4_Addr : constant Unsigned_64
              := Unsigned_64'Value
                (Muxml.Utils.Get_Attribute
                   (Nodes     => Physical_Memory,
                    Refs      => ((Name  => U ("type"),
                                   Value => U ("system_pt")),
                                  (Name  => U ("name"),
                                   Value => U ("kernel_" & CPU_ID & "|pt"))),
                    Attr_Name => "physicalAddress"));
         begin
            Buffer := Buffer & ASCII.LF & Indent
              (N         => 1,
               Unit_Size => 4);
            Buffer := Buffer  & ".long 0x" & Mutools.Utils.To_Hex
              (Number     => PML4_Addr,
               Normalize  => False);
         end;
      end loop;
      return To_String (Buffer);
   end Get_Kernel_PML4_Addrs;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Scheduling.Write_Spec_File
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Write_Interrupts (Output_Dir => Output_Dir,
                        Policy     => Policy);
      Write_Kernel (Output_Dir => Output_Dir,
                    Policy     => Policy);
      Write_Subject (Output_Dir => Output_Dir,
                     Policy     => Policy);
      Write_System (Output_Dir => Output_Dir,
                    Policy     => Policy);
      Write_Hardware (Output_Dir => Output_Dir,
                      Policy     => Policy);
      Kernel.Write_Project_File
        (Output_Dir => Output_Dir,
         Policy     => Policy);

      --  IOMMU feature.

      if Mutools.XML_Utils.Has_Feature_Enabled
        (Data => Policy,
         F    => Mutools.XML_Utils.Feature_IOMMU)
      then
         Write_IOMMU (Output_Dir => Output_Dir,
                      Policy     => Policy);
      end if;
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Hardware
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Tmpl : Mutools.Templates.Template_Type;

      MMConf_Base_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/platform/devices",
           Name  => "pciConfigAddress");

      --  Write I/O port constant for debug console.
      procedure Write_Debugconsole;

      ----------------------------------------------------------------------

      procedure Write_Debugconsole
      is
         Logical_Dev    : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/kernel/devices/device"
              & "[@logical='debugconsole']");
         Phys_Dev_Name  : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Logical_Dev,
              Name => "physical");
         Phys_Port_Name : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Logical_Dev,
              XPath => "ioPort",
              Name  => "physical");
         Phys_Address   : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Policy.Doc,
              XPath => "/system/platform/devices/device"
              & "[@name='" & Phys_Dev_Name & "']/ioPort[@name='"
              & Phys_Port_Name & "']",
              Name  => "start");
      begin
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__debug_console_port__",
            Content  => Phys_Address);
      end Write_Debugconsole;
   begin
      Mulog.Log (Msg => "Writing hardware spec to '"
                 & Output_Dir & "/skp-hardware.ads'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_hardware_ads);

      Write_Debugconsole;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__mmconf_base_addr__",
         Content  =>
           (if MMConf_Base_Addr_Str'Length > 0 then MMConf_Base_Addr_Str
            else Mutools.Utils.To_Hex (Number => Unsigned_64'Last)));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-hardware.ads");
   end Write_Hardware;

   -------------------------------------------------------------------------

   procedure Write_Interrupts
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      IRQs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data       => Policy,
           Left_XPath     => "/system/subjects/subject/devices/device/irq",
           Right_XPath    => "/system/platform/devices/device/irq",
           Match_Multiple => False,
           Match          => Mutools.Match.Is_Valid_Resource_Ref'Access);
      IRQ_Count   : constant Natural := DOM.Core.Nodes.Length
        (List => IRQs.Right);
      Route_Count : constant Natural
        := IRQ_Count - Utils.Get_IRQ_Count
          (IRQs     => IRQs.Right,
           IRQ_Kind => Mutools.XML_Utils.IRQ_PCI_MSI);

      Cur_IRQ   : Positive := 1;
      Cur_Route : Positive := 1;

      IRQ_Buffer, Vector_Buffer : Unbounded_String;

      --  Write IRQ information to interrupts spec.
      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Owner : DOM.Core.Node;
         Index : Natural);

      ----------------------------------------------------------------------

      procedure Write_Interrupt
        (IRQ   : DOM.Core.Node;
         Owner : DOM.Core.Node;
         Index : Natural)
      is
         use type DOM.Core.Node;
         use type Mutools.XML_Utils.IRQ_Kind;

         Phys_IRQ_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => IRQ,
              Name => "physical");
         Dev_Name : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => DOM.Core.Nodes.Parent_Node (N => IRQ),
              Name => "physical");
         Dev_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Policy.Doc,
              XPath => "/system/platform/devices/device[@name='"
              & Dev_Name & "']");
         Physical_IRQ : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Doc   => Dev_Node,
              XPath => "irq[@name='" & Phys_IRQ_Name & "']");
         IRQ_Nr : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Physical_IRQ,
               Name => "number"));
         Host_Vector : constant Natural := IRQ_Nr
           + Mutools.Constants.Host_IRQ_Remap_Offset;
         CPU : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Owner,
               Name => "cpu"));
         Subject_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Owner,
              Name => "id");
         Subject_Vector : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => IRQ,
              Name => "vector");
         IRQ_Kind : constant Mutools.XML_Utils.IRQ_Kind
           := Mutools.XML_Utils.Get_IRQ_Kind (Dev => Dev_Node);
         Is_PCI_Dev : constant Boolean
           := IRQ_Kind /= Mutools.XML_Utils.IRQ_ISA;
      begin
         case IRQ_Kind is
            when Mutools.XML_Utils.IRQ_ISA |
                 Mutools.XML_Utils.IRQ_PCI_LSI =>

               --  IRQ routing table.

               IRQ_Buffer := IRQ_Buffer & Indent (N => 2)
                 & Index'Img & " => IRQ_Route_Type'("
                 & ASCII.LF
                 & Indent (N => 3) & "CPU       =>" & CPU'Img
                 & "," & ASCII.LF
                 & Indent (N => 3) & "IRQ       =>" & IRQ_Nr'Img
                 & "," & ASCII.LF
                 & Indent (N => 3)
                 & "IRQ_Mode  => " & (if Is_PCI_Dev then "Level" else "Edge")
                 & "," & ASCII.LF
                 & Indent (N => 3)
                 & "IRQ_Level => " & (if Is_PCI_Dev then "Low" else "High")
                 & "," & ASCII.LF
                 & Indent (N => 3) & "Vector    =>" & Host_Vector'Img & ")";

               if Cur_Route /= Route_Count then
                  IRQ_Buffer := IRQ_Buffer & "," & ASCII.LF;
               end if;

               Cur_Route := Cur_Route + 1;
            when Mutools.XML_Utils.IRQ_PCI_MSI =>

               --  MSI interrupts are not routed through the I/O APIC.

               null;
         end case;

         --  Vector -> subject routing table.

         Vector_Buffer := Vector_Buffer & Indent (N => 2)
           & Host_Vector'Img & " => Vector_Route_Type'("
           & ASCII.LF
           & Indent (N => 3) & "Subject => " & Subject_Id & ","
           & ASCII.LF
           & Indent (N => 3) & "Vector  => " & Subject_Vector & ")";
      end Write_Interrupt;

      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing interrupt routing spec to '"
                 & Output_Dir & "/skp-interrupts.ads'");

      for I in 0 .. DOM.Core.Nodes.Length (List => IRQs.Left) - 1 loop
         declare
            IRQ     : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => IRQs.Left,
               Index => I);
            Subject : constant DOM.Core.Node
              := Muxml.Utils.Ancestor_Node (Node  => IRQ,
                                            Level => 3);
         begin
            Write_Interrupt
              (IRQ   => IRQ,
               Owner => Subject,
               Index => Cur_Route);

            if Cur_IRQ /= IRQ_Count then
               Vector_Buffer := Vector_Buffer & "," & ASCII.LF;
            end if;

            Cur_IRQ := Cur_IRQ + 1;
         end;
      end loop;

      if Route_Count = 0 then
         IRQ_Buffer := IRQ_Buffer & Indent (N => 2)
           & " others => Null_IRQ_Route";
      end if;

      if IRQ_Count > 0 then
         Vector_Buffer := Vector_Buffer & "," & ASCII.LF;
      end if;

      Vector_Buffer := Vector_Buffer & Indent (N => 2)
        & " others => Null_Vector_Route";

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_interrupts_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__remap_offset__",
         Content  => Mutools.Constants.Host_IRQ_Remap_Offset'Img);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__routing_range__",
         Content  => "1 .." & Natural'Max (1, Route_Count)'Img);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__irq_routing_table__",
         Content  => To_String (IRQ_Buffer));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__vector_routing_table__",
         Content  => To_String (Vector_Buffer));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-interrupts.ads");
   end Write_Interrupts;

   -------------------------------------------------------------------------

   procedure Write_IOMMU
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      use type Mutools.XML_Utils.IOMMU_Paging_Level;

      --  Return the lowest virtualAddress value string of the memory regions
      --  given as node list. Returns zero if node list is empty.
      function Get_Base_Addr
        (Nodes : DOM.Core.Node_List)
         return String;

      ----------------------------------------------------------------------

      function Get_Base_Addr
        (Nodes : DOM.Core.Node_List)
         return String
      is
         Result : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
         Count  : constant Natural       := DOM.Core.Nodes.Length
           (List => Nodes);
      begin
         if Count = 0 then
            return "0";
         end if;

         for I in 0 .. Count - 1 loop
            declare
               Node : constant DOM.Core.Node
                 := DOM.Core.Nodes.Item
                   (List  => Nodes,
                    Index => I);
               virtualAddr : constant Interfaces.Unsigned_64
                 := Interfaces.Unsigned_64'Value
                   (DOM.Core.Elements.Get_Attribute
                      (Elem => Node,
                       Name => "virtualAddress"));
            begin
               if virtualAddr < Result then
                  Result := virtualAddr;
               end if;
            end;
         end loop;

         return Mutools.Utils.To_Hex (Number => Result);
      end Get_Base_Addr;

      Filename  : constant String := Output_Dir & "/skp-iommu.ads";
      Root_Addr : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/memory/memory[@type='system_vtd_root']",
           Name  => "physicalAddress");
      IRT_Phys_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/memory/memory[@type='kernel_vtd_ir']",
           Name  => "physicalAddress");
      IRT_Virt_Addr_Str : constant String
        := Muxml.Utils.Get_Attribute
          (Doc   => Policy.Doc,
           XPath => "/system/kernel/memory/cpu[@id='0']/memory"
           & "[@physical='vtd_ir']",
           Name  => "virtualAddress");
      IRT_Phys_Addr : Interfaces.Unsigned_64
        := (if IRT_Phys_Addr_Str'Length > 0
            then Interfaces.Unsigned_64'Value (IRT_Phys_Addr_Str) else 0);
      IRT_Virt_Addr : constant Interfaces.Unsigned_64
        := (if IRT_Virt_Addr_Str'Length > 0
            then Interfaces.Unsigned_64'Value (IRT_Virt_Addr_Str) else 0);
      IOMMUs : constant Muxml.Utils.Matching_Pairs_Type
        := Muxml.Utils.Get_Matching
          (XML_Data    => Policy,
           Left_XPath  => "/system/kernel/devices/device/"
           & "memory[@logical='mmio']",
           Right_XPath => "/system/platform/devices/device[capabilities/"
           & "capability/@name='iommu']",
           Match       => Mutools.Match.Is_Valid_Reference_Lparent'Access);
      IOMMU_Count : constant Natural := DOM.Core.Nodes.Length
        (List => IOMMUs.Right);
      IOMMU_PT_Levels : constant Mutools.XML_Utils.IOMMU_Paging_Level
        := Mutools.XML_Utils.Get_IOMMU_Paging_Levels (Data => Policy);
      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing IOMMU spec to '" & Filename & "'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_iommu_ads);

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__root_table_addr__",
         Content  => (if Root_Addr'Length > 0 then Root_Addr else "0"));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__base_addr__",
         Content  => Get_Base_Addr (Nodes => IOMMUs.Left));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__iommu_device_range__",
         Content  => "1 .." & IOMMU_Count'Img);

      --  Shifted, 4KB aligned IR table address (see Intel VT-d specification,
      --  section 10.4.29).

      IRT_Phys_Addr := IRT_Phys_Addr / 2 ** 12;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__ir_table_phys_addr__",
         Content  => Mutools.Utils.To_Hex (Number => IRT_Phys_Addr));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__ir_table_virt_addr__",
         Content  => Mutools.Utils.To_Hex (Number => IRT_Virt_Addr));

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__cap_agaw_bit__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => Positive'Image (IOMMU_PT_Levels - 1),
            Side   => Ada.Strings.Left));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Filename);
   end Write_IOMMU;

   -------------------------------------------------------------------------

   procedure Write_Kernel
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Phys_Memory : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Stack_Node  : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Policy.Doc,
           XPath => "/system/kernel/memory/cpu[@id='0']/"
           & "memory[@logical='stack']");
      Stack_Ref   : constant String := DOM.Core.Elements.Get_Attribute
        (Elem => Stack_Node,
         Name => "physical");
      Stack_Size  : constant Unsigned_64 := Unsigned_64'Value
        (Muxml.Utils.Get_Attribute
           (Nodes     => Phys_Memory,
            Ref_Attr  => "name",
            Ref_Value => Stack_Ref,
            Attr_Name => "size"));
      Stack_Addr  : constant Unsigned_64 := Unsigned_64'Value
        (DOM.Core.Elements.Get_Attribute
           (Elem => Stack_Node,
            Name => "virtualAddress")) + Stack_Size;

      CPU_Store_Addr : constant Unsigned_64 := Unsigned_64'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@logical='store']",
            Name  => "virtualAddress"));

      Tau0_Iface_Addr : constant Unsigned_64 := Unsigned_64'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@logical='tau0_interface']",
            Name  => "virtualAddress"));

      --  Write C header for kernel to specified output directory.
      procedure Write_Kernel_Header
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type);

      --  Write SPARK specification for kernel to specified output directory.
      procedure Write_Kernel_Spec
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type);

      ----------------------------------------------------------------------

      procedure Write_Kernel_Header
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type)
      is
         Subject_Count : constant Natural := DOM.Core.Nodes.Length
           (List => McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => "/system/subjects/subject"));
         CPU_Count     : constant Positive
           := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Policy);
         VMXON_Addr    : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_vmxon")),
                             (Name  => U ("name"),
                              Value => U ("kernel_0|vmxon"))),
               Attr_Name => "physicalAddress"));
         VMCS_Addr     : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_vmcs")),
                             (Name  => U ("name"),
                              Value => U ("tau0|vmcs"))),
               Attr_Name => "physicalAddress"));

         Tmpl : Mutools.Templates.Template_Type;
      begin
         Mulog.Log (Msg => "Writing kernel header file to '"
                    & Output_Dir & "/policy.h'");

         Tmpl := Mutools.Templates.Create
           (Content => String_Templates.policy_h);
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_count__",
            Content  => Ada.Strings.Fixed.Trim
              (Source => Subject_Count'Img,
               Side   => Ada.Strings.Left));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__stack_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number    => Stack_Addr,
               Normalize => False));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_store_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number    => CPU_Store_Addr,
               Normalize => False));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_count__",
            Content  => Ada.Strings.Fixed.Trim
              (Source => CPU_Count'Img,
               Side   => Ada.Strings.Left));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__vmxon_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number    => VMXON_Addr,
               Normalize => False));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__vmcs_addr__",
            Content  => Mutools.Utils.To_Hex
              (Number    => VMCS_Addr,
               Normalize => False));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__kernel_pml4_addrs__",
            Content  => Get_Kernel_PML4_Addrs
              (Physical_Memory => Phys_Memory,
               CPU_Count       => CPU_Count));

         Mutools. Templates.Write
           (Template => Tmpl,
            Filename => Output_Dir & "/policy.h");
      end Write_Kernel_Header;

      ----------------------------------------------------------------------

      procedure Write_Kernel_Spec
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type)
      is
         Subj_States_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='tau0|state']",
               Name  => "virtualAddress"));
         Subj_Timers_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/memory/cpu/"
               & "memory[@logical='tau0|timer']",
               Name  => "virtualAddress"));
         IO_Apic_Addr     : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/kernel/devices/device[@logical='ioapic']"
               & "/memory",
               Name  => "virtualAddress"));

         Tmpl : Mutools.Templates.Template_Type;
      begin
         Mulog.Log (Msg => "Writing kernel spec to '"
                    & Output_Dir & "/skp-kernel.ads'");

         Tmpl := Mutools.Templates.Create
           (Content => String_Templates.skp_kernel_ads);
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__stack_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Stack_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_store_addr__",
            Content  => Mutools.Utils.To_Hex (Number => CPU_Store_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__tau0_iface_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Tau0_Iface_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_states_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Subj_States_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_timers_addr__",
            Content  => Mutools.Utils.To_Hex (Number => Subj_Timers_Addr));
         Mutools.Templates.Replace
           (Template => Tmpl,
            Pattern  => "__ioapic_addr__",
            Content  => Mutools.Utils.To_Hex (Number => IO_Apic_Addr));

         Mutools.Templates.Write
           (Template => Tmpl,
            Filename => Output_Dir & "/skp-kernel.ads");
      end Write_Kernel_Spec;
   begin
      Write_Kernel_Spec (Output_Dir => Output_Dir,
                         Policy     => Policy);
      Write_Kernel_Header (Output_Dir => Output_Dir,
                           Policy     => Policy);
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Subject
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects      : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Phys_Memory   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/memory/memory");
      Subj_Count    : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);
      Events        : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/events/event");
      Event_Targets : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject/events/target/event");

      Buffer : Unbounded_String;
      Tmpl   : Mutools.Templates.Template_Type;

      --  Add event entry to template buffer.
      procedure Add_Event (Event : DOM.Core.Node);

      --  Add trap entry to template buffer.
      procedure Add_Trap (Trap : DOM.Core.Node);

      --  Append SPARK specification of given subject to template buffer.
      procedure Write_Subject_Spec (Subject : DOM.Core.Node);

      -------------------------------------------------------------------

      procedure Add_Event (Event : DOM.Core.Node)
      is
         Event_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event,
              Name => "id");
         Phys_Event_Ref : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Event,
              XPath => "notify",
              Name  => "physical");
         Event_Target : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Event_Targets,
              Ref_Attr  => "physical",
              Ref_Value => Phys_Event_Ref);
         Dst_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Muxml.Utils.Ancestor_Node
                (Node  => Event_Target,
                 Level => 3),
              Name => "id");
         Dst_Vector : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event_Target,
              Name => "vector");
         Notify_Mode : constant String
           := Muxml.Utils.Get_Attribute
             (Nodes     => Events,
              Ref_Attr  => "name",
              Ref_Value => Phys_Event_Ref,
              Attr_Name => "mode");
      begin
         Buffer := Buffer & Indent (N => 3)  & " "
           & Event_Id & " => Event_Entry_Type'("
           & ASCII.LF
           & Indent (N => 4) & "Dst_Subject => " & Dst_Id & ","
           & ASCII.LF
           & Indent (N => 4) & "Dst_Vector  => ";

         if Dst_Vector = "none" then
            Buffer := Buffer & "Skp.Invalid_Vector,";
         else
            Buffer := Buffer & Dst_Vector & ",";
         end if;

         Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Handover    => ";
         if Notify_Mode = "switch" then
            Buffer := Buffer & "True,";
         else
            Buffer := Buffer & "False,";
         end if;

         Buffer := Buffer & ASCII.LF & Indent (N => 4) & "Send_IPI    => ";
         if Notify_Mode = "ipi" then
            Buffer := Buffer & "True)";
         else
            Buffer := Buffer & "False)";
         end if;
      end Add_Event;

      -------------------------------------------------------------------

      procedure Add_Trap (Trap : DOM.Core.Node)
      is
         Trap_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Trap,
              Name => "id");
         Phys_Event_Ref : constant String
           := Muxml.Utils.Get_Attribute
             (Doc   => Trap,
              XPath => "notify",
              Name  => "physical");
         Event_Target : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes     => Event_Targets,
              Ref_Attr  => "physical",
              Ref_Value => Phys_Event_Ref);

         Dst_Id : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Muxml.Utils.Ancestor_Node
                (Node  => Event_Target,
                 Level => 3),
              Name => "id");
         Dst_Vector : constant String
           := DOM.Core.Elements.Get_Attribute
             (Elem => Event_Target,
              Name => "vector");
      begin
         Buffer := Buffer & Indent (N => 3) & " "
           & Trap_Id & " => Trap_Entry_Type'(Dst_Subject => " & Dst_Id
           & ", Dst_Vector => ";

         if Dst_Vector = "none" then
            Buffer := Buffer & "Skp.Invalid_Vector)";
         else
            Buffer := Buffer & Dst_Vector & ")";
         end if;
      end Add_Trap;

      ----------------------------------------------------------------------

      procedure Write_Subject_Spec (Subject : DOM.Core.Node)
      is
         use type DOM.Core.Node;

         --  EPT memory type WB, page-walk length 4
         EPT_Flags : constant := 16#1e#;

         Name    : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "name");
         Subj_Id : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "id");
         CPU_Id  : constant String := DOM.Core.Elements.Get_Attribute
           (Elem => Subject,
            Name => "cpu");

         PML4_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_pt")),
                             (Name  => U ("name"),
                              Value => U (Name & "|pt"))),
               Attr_Name => "physicalAddress"));
         Entry_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Element_Value
              (Doc   => Subject,
               XPath => "vcpu/registers/gpr/rip"));
         Stack_Addr : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Element_Value
              (Doc   => Subject,
               XPath => "vcpu/registers/gpr/rsp"));
         VMCS_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_vmcs")),
                             (Name  => U ("name"),
                              Value => U (Name & "|vmcs"))),
               Attr_Name => "physicalAddress"));
         IOBM_Addr  : constant Unsigned_64 := Unsigned_64'Value
           (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_iobm")),
                             (Name  => U ("name"),
                              Value => U (Name & "|iobm"))),
               Attr_Name => "physicalAddress"));
         MSRBM_Addr : constant Unsigned_64 := Unsigned_64'Value
                      (Muxml.Utils.Get_Attribute
              (Nodes     => Phys_Memory,
               Refs      => ((Name  => U ("type"),
                              Value => U ("system_msrbm")),
                             (Name  => U ("name"),
                              Value => U (Name & "|msrbm"))),
               Attr_Name => "physicalAddress"));

         MSR_Store_Node : constant DOM.Core.Node
           := Muxml.Utils.Get_Element
             (Nodes => Phys_Memory,
              Refs  => ((Name  => U ("type"),
                         Value => U ("system_msrstore")),
                        (Name  => U ("name"),
                         Value => U (Name & "|msrstore"))));
         MSR_Store_Addr : Unsigned_64 := 0;
         MSR_Count      : Natural     := 0;

         CS_Access : constant String := Muxml.Utils.Get_Attribute
           (Doc   => Subject,
            XPath => "vcpu/segments/cs",
            Name  => "access");

         Pin_Ctrls   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/pin/*");
         Proc_Ctrls  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/proc/*");
         Proc2_Ctrls : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/proc2/*");
         Entry_Ctrls : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/entry/*");
         Exit_Ctrls  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/controls/exit/*");
         CR0_Value   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr0/*");
         CR0_Mask    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/cr0/*");
         CR4_Value   : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/registers/cr4/*");
         CR4_Mask    : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/cr4/*");
         Exceptions  : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "vcpu/vmx/masks/exception/*");
         Traps       : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "events/source/group[@name='vmx_exit']/*");
         Trap_Count  : constant Natural := DOM.Core.Nodes.Length
           (List => Traps);
         Events      : constant DOM.Core.Node_List
           := McKae.XML.XPath.XIA.XPath_Query
             (N     => Subject,
              XPath => "events/source/group[@name='vmcall']/*");
         Event_Count : constant Natural := DOM.Core.Nodes.Length
           (List => Events);
      begin
         if MSR_Store_Node /= null then
            MSR_Store_Addr := Unsigned_64'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => MSR_Store_Node,
                  Name => "physicalAddress"));
            declare
               Ctrls_Node : constant DOM.Core.Node
                 := Muxml.Utils.Get_Element
                   (Doc   => Subject,
                    XPath => "vcpu/vmx/controls");
               Debug_Ctrl : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_DEBUGCTL
                   (Controls => Ctrls_Node);
               PERF_Ctrl  : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_PERFGLOBALCTRL
                   (Controls => Ctrls_Node);
               PAT_Ctrl   : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_PAT (Controls => Ctrls_Node);
               EFER_Ctrl  : constant Boolean
                 := Mutools.XML_Utils.Has_Managed_EFER
                   (Controls => Ctrls_Node);
            begin
               MSR_Count := Mutools.XML_Utils.Calculate_MSR_Count
                 (MSRs                   => McKae.XML.XPath.XIA.XPath_Query
                   (N     => Subject,
                    XPath => "vcpu/registers/msrs/msr"
                    & "[@mode='w' or @mode='rw']"),
                  DEBUGCTL_Control       => Debug_Ctrl,
                  PAT_Control            => PAT_Ctrl,
                  PERFGLOBALCTRL_Control => PERF_Ctrl,
                  EFER_Control           => EFER_Ctrl);
            end;
         end if;

         Buffer := Buffer & Indent (N => 2) & Subj_Id
           & " => Subject_Spec_Type'("
           & ASCII.LF
           & Indent & "    CPU_Id             => " & CPU_Id & ","
           & ASCII.LF;

         if Muxml.Utils.Get_Element_Value
           (Doc   => Subject,
            XPath => "vcpu/vmx/controls/proc2/EnableEPT") = "1"
         then
            Buffer := Buffer
              & Indent & "    PML4_Address       => 0,"
              & ASCII.LF
              & Indent & "    EPT_Pointer        => "
              & Mutools.Utils.To_Hex (Number => PML4_Addr + EPT_Flags)
              & ",";
         else
            Buffer := Buffer
              & Indent & "    PML4_Address       => "
              & Mutools.Utils.To_Hex (Number => PML4_Addr) & ","
              & ASCII.LF
              & Indent & "    EPT_Pointer        => 0,";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    VMCS_Address       => "
           & Mutools.Utils.To_Hex (Number => VMCS_Addr) & ","
           & ASCII.LF
           & Indent & "    IO_Bitmap_Address  => "
           & Mutools.Utils.To_Hex (Number => IOBM_Addr) & ","
           & ASCII.LF
           & Indent & "    MSR_Bitmap_Address => "
           & Mutools.Utils.To_Hex (Number => MSRBM_Addr) & ","
           & ASCII.LF
           & Indent & "    MSR_Store_Address  => "
           & Mutools.Utils.To_Hex (Number => MSR_Store_Addr) & ","
           & ASCII.LF
           & Indent & "    Stack_Address      => "
           & Mutools.Utils.To_Hex (Number => Stack_Addr) & ","
           & ASCII.LF
           & Indent & "    Entry_Point        => "
           & Mutools.Utils.To_Hex (Number => Entry_Addr) & ","
           & ASCII.LF
           & Indent & "    CR0_Value          => "
           & Mutools.Utils.To_Hex (Number => Get_CR0 (Fields => CR0_Value))
           & "," & ASCII.LF
           & Indent & "    CR0_Mask           => "
           & Mutools.Utils.To_Hex (Number => Get_CR0 (Fields => CR0_Mask))
           & "," & ASCII.LF
           & Indent & "    CR4_Value          => "
           & Mutools.Utils.To_Hex (Number => Get_CR4 (Fields => CR4_Value))
           & "," & ASCII.LF
           & Indent & "    CR4_Mask           => "
           & Mutools.Utils.To_Hex (Number => Get_CR4 (Fields => CR4_Mask))
           & "," & ASCII.LF
           & Indent & "    CS_Access          => " & CS_Access & ","
           & ASCII.LF
           & Indent & "    Exception_Bitmap   => "
           & Mutools.Utils.To_Hex
           (Number => Get_Exceptions (Fields => Exceptions,
                                      Default => 16#ffff_ffff#)) & ","
           & ASCII.LF
           & Indent & "    MSR_Count          =>" & MSR_Count'Img & ","
           & ASCII.LF
           & Indent & "    VMX_Controls       => VMX_Controls_Type'("
           & ASCII.LF
           & Indent (N => 3) & " Exec_Pin    =>"
           & Get_Pin_Controls (Fields => Pin_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc   =>"
           & Get_Proc_Controls (Fields => Proc_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exec_Proc2  =>"
           & Get_Proc2_Controls (Fields => Proc2_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Exit_Ctrls  =>"
           & Get_Exit_Controls (Fields => Exit_Ctrls)'Img  & ","
           & ASCII.LF
           & Indent (N => 3) & " Entry_Ctrls =>"
           & Get_Entry_Controls (Fields => Entry_Ctrls)'Img  & "),"
           & ASCII.LF
           & Indent & "    Trap_Table         => ";

         if Trap_Count = 0 then
            Buffer := Buffer & "Null_Trap_Table,";
         else
            Buffer := Buffer & "Trap_Table_Type'(" & ASCII.LF;
            for I in 0 .. Trap_Count - 1 loop
               Add_Trap (Trap   => DOM.Core.Nodes.Item
                         (List  => Traps,
                          Index => I));

               if I < Trap_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end loop;

            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & " others => Null_Trap),";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    Event_Table        => ";

         if Event_Count = 0 then
            Buffer := Buffer & "Null_Event_Table)";
         else
            Buffer := Buffer & "Event_Table_Type'(" & ASCII.LF;
            for I in 0 .. Event_Count - 1 loop
               Add_Event (Event => DOM.Core.Nodes.Item
                          (List  => Events,
                           Index => I));

               if I < Event_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end loop;

            if Event_Count /= 32 then
               Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
                 & " others => Null_Event";
            end if;
            Buffer := Buffer & "))";
         end if;
      end Write_Subject_Spec;
   begin
      Mulog.Log (Msg => "Writing subject spec to '"
                 & Output_Dir & "/skp-subjects.adb'");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_ads);
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.ads");

      Tmpl := Mutools.Templates.Create
        (Content => String_Templates.skp_subjects_adb);

      for I in 0 .. Subj_Count - 1 loop
         Write_Subject_Spec
           (Subject => DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I));

         if I < Subj_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end loop;

      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subjects__",
         Content  => To_String (Buffer));

      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp-subjects.adb");
   end Write_Subject;

   -------------------------------------------------------------------------

   procedure Write_System
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      S_Count    : constant Natural     := DOM.Core.Nodes.Length
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/subjects/subject"));
      CPU_Count  : constant Natural
        := Mutools.XML_Utils.Get_Active_CPU_Count (Data => Policy);
      VMXON_Addr : constant Unsigned_64 := Unsigned_64'Value
        (Muxml.Utils.Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/memory/memory[@type='system_vmxon' and "
            & "contains(string(@name),'kernel_0')]",
            Name  => "physicalAddress"));

      Tmpl : Mutools.Templates.Template_Type;
   begin
      Mulog.Log (Msg => "Writing system spec to '" & Output_Dir & "/skp.ads'");

      Tmpl := Mutools.Templates.Create (Content => String_Templates.skp_ads);
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__cpu_count__",
         Content  => Ada.Strings.Fixed.Trim
           (Source => CPU_Count'Img,
            Side   => Ada.Strings.Left));
      Mutools.Templates.Replace
        (Template => Tmpl,
         Pattern  => "__subj_range__",
         Content  => "0 .."  & Positive'Image (S_Count - 1));
      Mutools.Templates.Replace (Template => Tmpl,
                                 Pattern  => "__vmxon_addr__",
                                 Content  => Mutools.Utils.To_Hex
                                   (Number => VMXON_Addr));
      Mutools.Templates.Write
        (Template => Tmpl,
         Filename => Output_Dir & "/skp.ads");
   end Write_System;

end Spec.Generator;
