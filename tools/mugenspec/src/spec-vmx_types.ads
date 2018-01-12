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

with Mutools.Utils;

with Spec.Utils;

package Spec.VMX_Types
is

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
      UMInstructionPrevention,
      VMXEnable,
      SMXEnable,
      FSGSBASEEnable,
      PCIDEnable,
      XSAVEEnable,
      SMEPEnable,
      SMAPEnable,
      ProtectionKeyEnable);

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
         UMInstructionPrevention  => 11,
         VMXEnable                => 13,
         SMXEnable                => 14,
         FSGSBASEEnable           => 16,
         PCIDEnable               => 17,
         XSAVEEnable              => 18,
         SMEPEnable               => 20,
         SMAPEnable               => 21,
         ProtectionKeyEnable      => 22));

   type Exceptions_Type is
     (DivideError,
      Debug,
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

   --  Exceptions bit positions as specified by Intel SDM Vol. 3A, table 6-1.
   function Get_Exceptions is new Utils.To_Number
     (Bitfield_Type => Exceptions_Type,
      Mapping_Type  => Exceptions_Map_Type,
      Map           =>
        (DivideError                => 0,
         Debug                      => 1,
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

   type EFER_Flags_Type is
     (SYSCALLEnable,
      IA32eModeEnable,
      ExecuteDisableBitEnable);

   type EFER_Flags_Map_Type is array (EFER_Flags_Type)
     of Mutools.Utils.Unsigned_64_Pos;

   --  EFER flag bit positions as specified by Intel SDM Vol. 3A, table 2-1.
   function Get_EFER is new Utils.To_Number
     (Bitfield_Type => EFER_Flags_Type,
      Mapping_Type  => EFER_Flags_Map_Type,
      Map           =>
        (SYSCALLEnable           => 0,
         IA32eModeEnable         => 8,
         ExecuteDisableBitEnable => 11));

end Spec.VMX_Types;
