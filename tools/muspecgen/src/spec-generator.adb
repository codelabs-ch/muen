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
with Ada.Strings.Unbounded;
with Ada.Long_Long_Integer_Text_IO;

with DOM.Core.Elements;
with DOM.Core.Nodes;

with McKae.XML.XPath.XIA;

with Mulog;

with Spec.Templates;
with Spec.Utils;

pragma Elaborate_All (Spec.Utils);

package body Spec.Generator
is

   use Ada.Strings.Unbounded;

   --  Searches the element specified by an XPath in the given document and
   --  returns the attribute given by name as string. If no such attribute or
   --  element exists, an empty string is returned.
   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String;

   --  Searches the element specified by an XPath in the given document and
   --  returns its value as string. If no such element exists, an empty string
   --  is returned.
   function Get_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String)
      return String;

   --  Return N number of indentation spaces.
   function Indent (N : Positive := 1) return String;

   --  Return hexadecimal representation of given number. If prefix is True,
   --  the returned string includes the base (16#..#).
   function To_Hex
     (Number : Long_Long_Integer;
      Prefix : Boolean := True)
      return String;

   type Pin_Ctrl_Type is
     (ExternalInterruptExiting,
      NMIExiting,
      VirtualNMIs,
      ActivateVMXTimer,
      ProcessPostedInterrupts);

   type Pin_Ctrl_Map_Type is array (Pin_Ctrl_Type) of Natural;

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

   type Proc_Ctrl_Map_Type is array (Proc_Ctrl_Type) of Natural;

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

   type Proc2_Ctrl_Map_Type is array (Proc2_Ctrl_Type) of Natural;

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

   type Entry_Ctrl_Map_Type is array (Entry_Ctrl_Type) of Natural;

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

   type Exit_Ctrl_Map_Type is array (Exit_Ctrl_Type) of Natural;

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

   type CR0_Flags_Map_Type is array (CR0_Flags_Type) of Natural;

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

   type CR4_Flags_Map_Type is array (CR4_Flags_Type) of Natural;

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

   type Exceptions_Map_Type is array (Exceptions_Type) of Natural;

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

   --  Write interrupt policy file to specified output directory.
   procedure Write_Interrupts
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write kernel-related policy files to specified output directory.
   procedure Write_Kernel
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write scheduling-related policy file to specified output directory.
   procedure Write_Scheduling
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   --  Write subject-related policy file to specified output directory.
   procedure Write_Subject
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type);

   -------------------------------------------------------------------------

   function Get_Attribute
     (Doc   : DOM.Core.Node;
      XPath : String;
      Name  : String)
      return String
   is
      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => Doc,
            XPath => XPath),
         Index => 0);
   begin
      return DOM.Core.Elements.Get_Attribute
        (Elem => Node,
         Name => Name);
   end Get_Attribute;

   -------------------------------------------------------------------------

   function Get_Element_Value
     (Doc   : DOM.Core.Node;
      XPath : String)
      return String
   is
      Node : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List  => McKae.XML.XPath.XIA.XPath_Query
           (N     => Doc,
            XPath => XPath & "/text()"),
         Index => 0);
   begin
      return DOM.Core.Nodes.Node_Value (N => Node);
   end Get_Element_Value;

   -------------------------------------------------------------------------

   function Indent (N : Positive := 1) return String
   is
      Indent : constant String := "   ";
      Result : Unbounded_String;
   begin
      for I in Positive range 1 .. N loop
         Result := Result & Indent;
      end loop;

      return To_String (Result);
   end Indent;

   -------------------------------------------------------------------------

   function To_Hex
     (Number : Long_Long_Integer;
      Prefix : Boolean := True)
      return String
   is
      Num_Str : String (1 .. 20);
   begin
      Ada.Long_Long_Integer_Text_IO.Put
        (To   => Num_Str,
         Item => Number,
         Base => 16);

      declare
         Trimmed : constant String := Ada.Strings.Fixed.Trim
           (Source => Num_Str,
            Side   => Ada.Strings.Left);
      begin
         if Prefix then
            return Trimmed;
         else
            return Trimmed (Trimmed'First + 3 .. Trimmed'Last - 1);
         end if;
      end;
   end To_Hex;

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Write_Scheduling (Output_Dir => Output_Dir,
                        Policy     => Policy);
      Write_Interrupts (Output_Dir => Output_Dir,
                        Policy     => Policy);
      Write_Kernel (Output_Dir => Output_Dir,
                    Policy     => Policy);
      Write_Subject (Output_Dir => Output_Dir,
                     Policy     => Policy);
   end Write;

   -------------------------------------------------------------------------

   procedure Write_Interrupts
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is

      --  IRQ to host Vector offset.
      Vector_Offset : constant := 48;

      Subjects  : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject[count(devices/device/irq)>0]");
      IRQ_Count : constant Natural := DOM.Core.Nodes.Length
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/subjects/subject/devices/device/irq"));

      Cur_IRQ : Positive := 1;

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
         IRQ_Nr : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => IRQ,
               Name => "number"));
         CPU    : constant Natural := Natural'Value
           (DOM.Core.Elements.Get_Attribute
              (Elem => Owner,
               Name => "cpu"));
      begin

         --  IRQ routing table.

         IRQ_Buffer := IRQ_Buffer & Indent (N => 2)
           & Index'Img & " => IRQ_Route_Type'("
           & ASCII.LF
           & Indent (N => 3) & "CPU    =>" & CPU'Img
           & "," & ASCII.LF
           & Indent (N => 3) & "IRQ    =>" & IRQ_Nr'Img
           & "," & ASCII.LF
           & Indent (N => 3) & "Vector =>"
           & Positive'Image (Vector_Offset + IRQ_Nr) & ")";

         --  Vector -> subject routing table.

         Vector_Buffer := Vector_Buffer & Indent (N => 2)
           & Positive'Image (Vector_Offset + IRQ_Nr) & " => "
           & DOM.Core.Elements.Get_Attribute (Elem => Owner,
                                              Name => "id");
      end Write_Interrupt;

      Tmpl : Templates.Template_Type;
   begin
      for I in 0 .. DOM.Core.Nodes.Length (List => Subjects) - 1 loop
         declare
            Subject : constant DOM.Core.Node := DOM.Core.Nodes.Item
              (List  => Subjects,
               Index => I);
            IRQs    : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Subject,
                 XPath => "devices/device/irq");
         begin
            for IRQ in 1 .. DOM.Core.Nodes.Length (List => IRQs) loop
               Write_Interrupt
                 (IRQ   => DOM.Core.Nodes.Item (List  => IRQs,
                                                Index => IRQ - 1),
                  Owner => Subject,
                  Index => Cur_IRQ);

               if Cur_IRQ /= IRQ_Count then
                  IRQ_Buffer    := IRQ_Buffer    & "," & ASCII.LF;
                  Vector_Buffer := Vector_Buffer & "," & ASCII.LF;
               end if;

               Cur_IRQ := Cur_IRQ + 1;
            end loop;
         end;
      end loop;

      Vector_Buffer := Vector_Buffer & ","
        & ASCII.LF & Indent (N => 2) & " others => Skp.Invalid_Subject";

      Tmpl := Templates.Load (Filename => "skp-interrupts.ads");
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__routing_range__",
                         Content  => "1 .." & IRQ_Count'Img);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__irq_routing_table__",
                         Content  => To_String (IRQ_Buffer));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__vector_routing_table__",
                         Content  => To_String (Vector_Buffer));

      Mulog.Log (Msg => "Writing interrupt routing spec to '"
                 & Output_Dir & "/skp-interrupts.ads'");

      Templates.Write (Template => Tmpl,
                       Filename => Output_Dir & "/skp-interrupts.ads");
   end Write_Interrupts;

   -------------------------------------------------------------------------

   procedure Write_Kernel
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Stack_Size : constant Long_Long_Integer := 2 * 4096;
      Stack_Addr : constant Long_Long_Integer := Long_Long_Integer'Value
        (Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@logical='stack']",
            Name  => "virtualAddress")) + Stack_Size;

      CPU_Store_Addr : constant Long_Long_Integer := Long_Long_Integer'Value
        (Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/kernel/memory/cpu[@id='0']/"
            & "memory[@logical='store']",
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
         Filename : constant String := "policy.h";

         Subject_Count : constant Natural := DOM.Core.Nodes.Length
           (List => McKae.XML.XPath.XIA.XPath_Query
              (N     => Policy.Doc,
               XPath => "/system/subjects/subject"));
         CPU_Count     : constant String := Get_Attribute
           (Doc   => Policy.Doc,
            XPath => "/system/platform/processor",
            Name  => "logicalCpus");
         PML4_Addr     : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='kernel_0|pt']",
               Name  => "physicalAddress"));
         VMXON_Addr    : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='kernel_0|vmxon']",
               Name  => "physicalAddress"));
         VMCS_Addr     : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='tau0|vmcs']",
               Name  => "physicalAddress"));

         Tmpl : Templates.Template_Type;
      begin
         Tmpl := Templates.Load (Filename => Filename);
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__subj_count__",
            Content  => Ada.Strings.Fixed.Trim
              (Source => Subject_Count'Img,
               Side   => Ada.Strings.Left));
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__stack_addr__",
            Content  => To_Hex (Number => Stack_Addr,
                                Prefix => False));
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__kpml4_addr__",
            Content  => To_Hex (Number => PML4_Addr,
                                Prefix => False));
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_count__",
            Content  => CPU_Count);
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__vmxon_addr__",
            Content  => To_Hex (Number => VMXON_Addr,
                                Prefix => False));
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__vmcs_addr__",
            Content  => To_Hex (Number => VMCS_Addr,
                                Prefix => False));

         Mulog.Log (Msg => "Writing kernel header file to '"
                    & Output_Dir & "/" & Filename & "'");

         Templates.Write (Template => Tmpl,
                          Filename => Output_Dir & "/" & Filename);
      end Write_Kernel_Header;

      ----------------------------------------------------------------------

      procedure Write_Kernel_Spec
        (Output_Dir : String;
         Policy     : Muxml.XML_Data_Type)
      is
         pragma Unreferenced (Policy);

         Filename : constant String := "skp-kernel.ads";

         Tmpl : Templates.Template_Type;
      begin
         Tmpl := Templates.Load (Filename => Filename);
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__stack_addr__",
            Content  => To_Hex (Number => Stack_Addr));
         Templates.Replace
           (Template => Tmpl,
            Pattern  => "__cpu_store_addr__",
            Content  => To_Hex (Number => CPU_Store_Addr));

         Mulog.Log (Msg => "Writing kernel spec to '"
                    & Output_Dir & "/" & Filename & "'");

         Templates.Write (Template => Tmpl,
                          Filename => Output_Dir & "/" & Filename);
      end Write_Kernel_Spec;
   begin
      Write_Kernel_Spec (Output_Dir => Output_Dir,
                         Policy     => Policy);
      Write_Kernel_Header (Output_Dir => Output_Dir,
                         Policy     => Policy);
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Scheduling
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Scheduling   : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/scheduling"),
         Index => 0);
      Processor    : constant DOM.Core.Node := DOM.Core.Nodes.Item
        (List => McKae.XML.XPath.XIA.XPath_Query
           (N     => Policy.Doc,
            XPath => "/system/platform/processor"),
         Index => 0);
      CPU_Speed_Hz : constant Long_Integer :=  1_000_000 * Long_Integer'Value
        (DOM.Core.Elements.Get_Attribute (Elem => Processor,
                                          Name => "speed"));
      Timer_Rate   : constant Long_Integer := 2 ** Natural'Value
        (DOM.Core.Elements.Get_Attribute (Elem => Processor,
                                          Name => "vmxTimerRate"));
      Timer_Factor : constant Long_Integer := CPU_Speed_Hz /
        (Timer_Rate * Long_Integer'Value (DOM.Core.Elements.Get_Attribute
         (Elem => Scheduling,
          Name => "tickRate")));
      CPU_Count    : constant Natural      := Natural'Value
        (DOM.Core.Elements.Get_Attribute (Elem => Processor,
                                          Name => "logicalCpus"));

      Major_Count     : Positive;
      Max_Minor_Count : Positive;
      Majors          : DOM.Core.Node_List;
      Buffer          : Unbounded_String;
      Tmpl            : Templates.Template_Type;

      --  Returns the maximum count of minor frames per major frame.
      function Get_Max_Minor_Count (Schedule : DOM.Core.Node) return Positive;

      --  Write major frame with given index and minor frames to buffer.
      procedure Write_Major_Frame
        (Index  : Natural;
         Minors : DOM.Core.Node_List);

      ----------------------------------------------------------------------

      function Get_Max_Minor_Count (Schedule : DOM.Core.Node) return Positive
      is
         CPUs   : DOM.Core.Node_List;
         Minors : DOM.Core.Node_List;
         Count  : Positive := 1;
      begin
         CPUs := McKae.XML.XPath.XIA.XPath_Query
           (N     => Schedule,
            XPath => "majorFrame/cpu");

         for I in 0 .. DOM.Core.Nodes.Length (List => CPUs) - 1 loop
            Minors := McKae.XML.XPath.XIA.XPath_Query
              (N     => DOM.Core.Nodes.Item (List  => CPUs,
                                             Index => I),
               XPath => "minorFrame");

            if DOM.Core.Nodes.Length (List => Minors) > Count then
               Count := DOM.Core.Nodes.Length (List => Minors);
            end if;
         end loop;

         return Count;
      end Get_Max_Minor_Count;

      ----------------------------------------------------------------------

      procedure Write_Major_Frame
        (Index  : Natural;
         Minors : DOM.Core.Node_List)
      is
         Minor_Count : constant Positive := DOM.Core.Nodes.Length
           (List => Minors);

         --  Write minor frame with given index to buffer.
         procedure Write_Minor_Frame
           (Minor : DOM.Core.Node;
            Index : Natural);

         -------------------------------------------------------------------

         procedure Write_Minor_Frame
           (Minor : DOM.Core.Node;
            Index : Natural)
         is
            Ticks : constant Long_Integer := Timer_Factor * Long_Integer'Value
              (DOM.Core.Elements.Get_Attribute
                 (Elem => Minor,
                  Name => "ticks"));

            Subject    : constant String := DOM.Core.Elements.Get_Attribute
              (Elem => Minor,
               Name => "subject");
            Subject_Id : constant String := Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/subjects/subject[@name='" & Subject & "']",
               Name  => "id");
         begin
            Buffer := Buffer & Indent (N => 4) & Index'Img
              & " => Minor_Frame_Type'(Subject_Id => " & Subject_Id
              & ", Ticks =>" & Ticks'Img & ")";
         end Write_Minor_Frame;
      begin
         Buffer := Buffer & Indent (N => 2)
           & Index'Img & " => Major_Frame_Type'"
           & ASCII.LF & Indent (N => 3)
           & "(Length       =>" & Minor_Count'Img & ","
           & ASCII.LF & Indent (N => 3)
           & " Minor_Frames => Minor_Frame_Array'("
           & ASCII.LF;

         for I in 1 .. Minor_Count loop
            Write_Minor_Frame (Minor => DOM.Core.Nodes.Item
                               (List  => Minors,
                                Index => I - 1),
                               Index => I);

            if I < Minor_Count then
               Buffer := Buffer & "," & ASCII.LF;
            end if;
         end loop;

         if Minor_Count < Max_Minor_Count then
            Buffer := Buffer & "," & ASCII.LF & Indent (N => 3)
              & Indent & " others => Null_Minor_Frame";
         end if;

         Buffer := Buffer & "))";
      end Write_Major_Frame;
   begin
      Majors := McKae.XML.XPath.XIA.XPath_Query
        (N     => Scheduling,
         XPath => "majorFrame");

      Major_Count     := DOM.Core.Nodes.Length (List => Majors);
      Max_Minor_Count := Get_Max_Minor_Count (Schedule => Scheduling);

      for CPU in 0 .. CPU_Count - 1 loop
         Buffer    := Buffer & Indent
           & " " & CPU'Img & " => Major_Frame_Array'("
           & ASCII.LF;

         for I in 0 .. Major_Count - 1 loop
            declare
               Major      : constant DOM.Core.Node := DOM.Core.Nodes.Item
                 (List  => Majors,
                  Index => I);
               Major_CPUs : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => Major,
                    XPath => "cpu");
               Minors     : constant DOM.Core.Node_List
                 := McKae.XML.XPath.XIA.XPath_Query
                   (N     => DOM.Core.Nodes.Item
                        (List  => Major_CPUs,
                         Index => CPU),
                    XPath => "minorFrame");
            begin
               Write_Major_Frame (Minors => Minors,
                                  Index  => I);

               if I < Major_Count - 1 then
                  Buffer := Buffer & "," & ASCII.LF;
               end if;
            end;
         end loop;

         Buffer := Buffer & ")";

         if CPU < CPU_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end loop;

      Tmpl := Templates.Load (Filename  => "skp-scheduling.ads");
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__minor_range__",
                         Content  => "1 .." & Max_Minor_Count'Img);
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__major_range__",
                         Content  => "0 .." & Natural'Image (Major_Count - 1));
      Templates.Replace (Template => Tmpl,
                         Pattern  => "__scheduling_plans__",
                         Content  => To_String (Buffer));

      Mulog.Log (Msg => "Writing scheduling spec for" & CPU_Count'Img
                 & " CPUs to '" & Output_Dir & "/skp-scheduling.ads'");

      Templates.Write (Template => Tmpl,
                       Filename => Output_Dir & "/skp-scheduling.ads");
   end Write_Scheduling;

   -------------------------------------------------------------------------

   procedure Write_Subject
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
      Subjects   : constant DOM.Core.Node_List
        := McKae.XML.XPath.XIA.XPath_Query
          (N     => Policy.Doc,
           XPath => "/system/subjects/subject");
      Subj_Count : constant Natural
        := DOM.Core.Nodes.Length (List => Subjects);

      Buffer : Unbounded_String;
      Tmpl   : Templates.Template_Type;

      --  Append SPARK specification of given subject to template buffer.
      procedure Write_Subject_Spec
        (Subject : DOM.Core.Node;
         Policy  : Muxml.XML_Data_Type);

      ----------------------------------------------------------------------

      procedure Write_Subject_Spec
        (Subject : DOM.Core.Node;
         Policy  : Muxml.XML_Data_Type)
      is
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

         PML4_Addr  : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='" &  Name & "|pt']",
               Name  => "physicalAddress"));
         Entry_Addr : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Element_Value
              (Doc   => Subject,
               XPath => "vcpu/registers/gpr/rip"));
         Stack_Addr : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Element_Value
              (Doc   => Subject,
               XPath => "vcpu/registers/gpr/rsp"));
         VMCS_Addr  : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='" & Name & "|vmcs']",
               Name  => "physicalAddress"));
         IOBM_Addr  : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='" & Name & "|iobm']",
               Name  => "physicalAddress"));
         MSRBM_Addr : constant Long_Long_Integer := Long_Long_Integer'Value
           (Get_Attribute
              (Doc   => Policy.Doc,
               XPath => "/system/memory/memory[@name='" & Name & "|msrbm']",
               Name  => "physicalAddress"));

         CS_Access : constant String := Get_Attribute
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
      begin
         Buffer := Buffer & Indent (N => 2) & Subj_Id
           & " => Subject_Spec_Type'("
           & ASCII.LF
           & Indent & "    CPU_Id             => " & CPU_Id & ","
           & ASCII.LF
           & Indent & "    Profile            => ";

         if Get_Element_Value
           (Doc   => Subject,
            XPath => "vcpu/vmx/controls/proc2/UnrestrictedGuest") = "1"
         then
            Buffer := Buffer & "Vm," & ASCII.LF;
         else
            Buffer := Buffer & "Native," & ASCII.LF;
         end if;

         if Get_Element_Value
           (Doc   => Subject,
            XPath => "vcpu/vmx/controls/proc2/EnableEPT") = "1"
         then
            Buffer := Buffer
                 & Indent & "    PML4_Address       => 0,"
                 & ASCII.LF
                 & Indent & "    EPT_Pointer        => "
                 & To_Hex (Number => PML4_Addr + EPT_Flags) & ",";
         else
            Buffer := Buffer
              & Indent & "    PML4_Address       => "
              & To_Hex (Number => PML4_Addr) & ","
              & ASCII.LF
              & Indent & "    EPT_Pointer        => 0,";
         end if;

         Buffer := Buffer & ASCII.LF
           & Indent & "    VMCS_Address       => "
           & To_Hex (Number => VMCS_Addr) & ","
           & ASCII.LF
           & Indent & "    IO_Bitmap_Address  => "
           & To_Hex (Number => IOBM_Addr) & ","
           & ASCII.LF
           & Indent & "    MSR_Bitmap_Address => "
           & To_Hex (Number => MSRBM_Addr) & ","
           & ASCII.LF
           & Indent & "    Stack_Address      => "
           & To_Hex (Number => Stack_Addr) & ","
           & ASCII.LF
           & Indent & "    Entry_Point        => "
           & To_Hex (Number => Entry_Addr) & ","
           & ASCII.LF
           & Indent & "    CR0_Value          => "
           & To_Hex (Number => Get_CR0 (Fields => CR0_Value)) & ","
           & ASCII.LF
           & Indent & "    CR0_Mask           => "
           & To_Hex (Number => Get_CR0 (Fields => CR0_Mask)) & ","
           & ASCII.LF
           & Indent & "    CR4_Value          => "
           & To_Hex (Number => Get_CR4 (Fields => CR4_Value)) & ","
           & ASCII.LF
           & Indent & "    CR4_Mask           => "
           & To_Hex (Number => Get_CR4 (Fields => CR4_Mask)) & ","
           & ASCII.LF
           & Indent & "    CS_Access          => " & CS_Access & ","
           & ASCII.LF
           & Indent & "    Exception_Bitmap   => "
           & To_Hex (Number => Get_Exceptions (Fields => Exceptions)) & ","
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
           & Indent & "    Trap_Table         => Null_Trap_Table,"
           & ASCII.LF
           & Indent & "    Event_Table        => Null_Event_Table)";
      end Write_Subject_Spec;
   begin
      Tmpl := Templates.Load (Filename => "skp-subjects.ads");
      Templates.Write (Template => Tmpl,
                       Filename => Output_Dir & "/skp-subjects.ads");

      Tmpl := Templates.Load (Filename => "skp-subjects.adb");

      for I in 0 .. Subj_Count - 1 loop
         Write_Subject_Spec
           (Subject => DOM.Core.Nodes.Item (List  => Subjects,
                                            Index => I),
            Policy  => Policy);

         if I < Subj_Count - 1 then
            Buffer := Buffer & "," & ASCII.LF;
         end if;
      end loop;

      Templates.Replace (Template => Tmpl,
                         Pattern  => "__subjects__",
                         Content  => To_String (Buffer));

      Mulog.Log (Msg => "Writing subject spec to '"
                 & Output_Dir & "/skp-subjects.adb'");

      Templates.Write (Template => Tmpl,
                       Filename => Output_Dir & "/skp-subjects.adb");
   end Write_Subject;

end Spec.Generator;
