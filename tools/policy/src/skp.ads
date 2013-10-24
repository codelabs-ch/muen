--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;

with SK;

package Skp
is

   type Memory_Type_Type is (UC, WC, WT, WB, WP);

   type Memory_Region_Type is record
      Physical_Address : SK.Word64;
      Virtual_Address  : SK.Word64;
      Size             : SK.Word64;
      Alignment        : SK.Word64;
      Writable         : Boolean;
      Executable       : Boolean;
      Memory_Type      : Memory_Type_Type;
   end record;

   package Memregion_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Memory_Region_Type);

   subtype Memory_Layout_Type is Memregion_Package.List;

   type IO_Port_Range is record
      Start_Port : SK.Word16;
      End_Port   : SK.Word16;
   end record;

   package Ports_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => IO_Port_Range);

   subtype IO_Ports_Type is Ports_Package.List;

   type MSR_Mode_Type is (MSR_Read, MSR_Write, MSR_Read_Write);

   type MSR_Type is record
      Start_Addr : SK.Word32;
      End_Addr   : SK.Word32;
      Mode       : MSR_Mode_Type;
   end record;

   package MSRs_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => MSR_Type);

   subtype MSRs_Type is MSRs_Package.List;

   subtype MSR_Low_Range  is SK.Word32 range 16#00000000# .. 16#00001fff#;
   subtype MSR_High_Range is SK.Word32 range 16#c0000000# .. 16#c0001fff#;

   type Initial_State_Type is record
      Stack_Address : SK.Word64;
      Entry_Point   : SK.Word64;
   end record;

   type Binary_Ref_Type is record
      Name             : Ada.Strings.Unbounded.Unbounded_String;
      Physical_Address : SK.Word64;
   end record;

   type Trap_Kind is
     (Exception_Or_NMI,
      Triple_Fault,
      INIT_Signal,
      Start_Up_IPI,
      IO_SMI,
      Other_SMI,
      Interrupt_Window,
      NMI_Window,
      Task_Switch,
      CPUID,
      GETSEC,
      HLT,
      INVD,
      INVLPG,
      RDPMC,
      RDTSC,
      RSM,
      VMCLEAR,
      VMLAUNCH,
      VMPTRLD,
      VMPTRST,
      VMREAD,
      VMRESUME,
      VMWRITE,
      VMXOFF,
      VMXON,
      Control_Register_Access,
      MOV_DR,
      IO_Instruction,
      RDMSR,
      WRMSR,
      VM_Entry_Fail_Invalid_Guest_State,
      VM_Entry_Fail_MSR_Loading,
      MWAIT,
      Monitor_Trap_Flag,
      MONITOR,
      PAUSE,
      VM_Entry_Fail_Machine_Check,
      TPR_Below_Threshold,
      APIC_Access,
      Virtualized_EOI,
      GDTR_IDTR_Access,
      LDTR_TR_Access,
      EPT_Violation,
      EPT_Misconfiguration,
      INVEPT,
      RDTSCP,
      INVVPID,
      WBINVD,
      XSETBV,
      APIC_Write,
      RDRAND,
      INVPCID,
      VMFUNC);

   for Trap_Kind use
     (Exception_Or_NMI                  => 0,
      Triple_Fault                      => 2,
      INIT_Signal                       => 3,
      Start_Up_IPI                      => 4,
      IO_SMI                            => 5,
      Other_SMI                         => 6,
      Interrupt_Window                  => 7,
      NMI_Window                        => 8,
      Task_Switch                       => 9,
      CPUID                             => 10,
      GETSEC                            => 11,
      HLT                               => 12,
      INVD                              => 13,
      INVLPG                            => 14,
      RDPMC                             => 15,
      RDTSC                             => 16,
      RSM                               => 17,
      VMCLEAR                           => 19,
      VMLAUNCH                          => 20,
      VMPTRLD                           => 21,
      VMPTRST                           => 22,
      VMREAD                            => 23,
      VMRESUME                          => 24,
      VMWRITE                           => 25,
      VMXOFF                            => 26,
      VMXON                             => 27,
      Control_Register_Access           => 28,
      MOV_DR                            => 29,
      IO_Instruction                    => 30,
      RDMSR                             => 31,
      WRMSR                             => 32,
      VM_Entry_Fail_Invalid_Guest_State => 33,
      VM_Entry_Fail_MSR_Loading         => 34,
      MWAIT                             => 36,
      Monitor_Trap_Flag                 => 37,
      MONITOR                           => 39,
      PAUSE                             => 40,
      VM_Entry_Fail_Machine_Check       => 41,
      TPR_Below_Threshold               => 43,
      APIC_Access                       => 44,
      Virtualized_EOI                   => 45,
      GDTR_IDTR_Access                  => 46,
      LDTR_TR_Access                    => 47,
      EPT_Violation                     => 48,
      EPT_Misconfiguration              => 49,
      INVEPT                            => 50,
      RDTSCP                            => 51,
      INVVPID                           => 53,
      WBINVD                            => 54,
      XSETBV                            => 55,
      APIC_Write                        => 56,
      RDRAND                            => 57,
      INVPCID                           => 58,
      VMFUNC                            => 59);

   type Trap_Table_Entry_Type is record
      Kind        : Trap_Kind;
      Dst_Subject : Ada.Strings.Unbounded.Unbounded_String;
      Dst_Vector  : Natural := 256;
   end record;

   package Traps_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Trap_Kind,
      Element_Type => Trap_Table_Entry_Type);

   subtype Trap_Table_Type is Traps_Package.Map;

   type Event_Table_Entry_Type is record
      Event_Nr    : Natural;
      Dst_Subject : Ada.Strings.Unbounded.Unbounded_String;
      Dst_Vector  : Natural := 256;
      Handover    : Boolean;
      Send_IPI    : Boolean;
   end record;

   package Events_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Natural,
      Element_Type => Event_Table_Entry_Type);

   subtype Event_Table_Type is Events_Package.Map;

   type Subject_Profile_Type is (Native, Vm);

   type Subject_Type is record
      Id                 : Natural;
      Name               : Ada.Strings.Unbounded.Unbounded_String;
      Profile            : Subject_Profile_Type;
      CPU                : Natural;
      Pml4_Address       : SK.Word64;
      IO_Bitmap_Address  : SK.Word64;
      MSR_Bitmap_Address : SK.Word64;
      ZP_Bitmap_Address  : SK.Word64;
      ACPI_Base_Address  : SK.Word64;
      Init_State         : Initial_State_Type;
      Memory_Layout      : Memory_Layout_Type;
      Binary             : Binary_Ref_Type;
      IO_Ports           : IO_Ports_Type;
      MSRs               : MSRs_Type;
      Trap_Table         : Trap_Table_Type;
      Event_Table        : Event_Table_Type;
   end record;

   function "<" (Left, Right : Subject_Type) return Boolean;

   package Subjects_Package is new Ada.Containers.Ordered_Sets
     (Element_Type => Subject_Type);

   subtype Subjects_Type is Subjects_Package.Set;

   --  Returns subject id of subject with given name. If no subject with
   --  specified name exists, -1 is returned.
   function Get_Id
     (Subjects : Subjects_Type;
      Name     : Ada.Strings.Unbounded.Unbounded_String)
      return Integer;

   --  Returns associated CPU number of subject given by id. If the subject is
   --  not found, -1 is returned.
   function Get_CPU
     (Subjects   : Subjects_Type;
      Subject_Id : Natural)
      return Integer;

   type Binary_Format is (Elf, Raw, BzImage);

   type Binary_Type is record
      Path   : Ada.Strings.Unbounded.Unbounded_String;
      Format : Binary_Format;
   end record;

   package Binary_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Binary_Type,
      "<"          => Ada.Strings.Unbounded."<");

   subtype Binaries_Type is Binary_Package.Map;

   type Kernel_Type is record
      Stack_Address    : SK.Word64;
      Pml4_Address     : SK.Word64;
      CPU_Page_Address : SK.Word64;
      Memory_Layout    : Memory_Layout_Type;
   end record;

   package Owners_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Natural);

   subtype Owners_Type is Owners_Package.List;

   type Device_Type is record
      Name          : Ada.Strings.Unbounded.Unbounded_String;
      Memory_Layout : Memory_Layout_Type;
      IO_Ports      : IO_Ports_Type;
      IRQ           : Integer := -1;
      Owners        : Owners_Type;
   end record;

   package Devices_Package is new Ada.Containers.Ordered_Maps
     (Key_Type     => Ada.Strings.Unbounded.Unbounded_String,
      Element_Type => Device_Type,
      "<"          => Ada.Strings.Unbounded."<");

   type Processor_Type is record
      Logical_CPUs   : Positive;
      Speed          : Positive;
      VMX_Timer_Rate : Natural;
   end record;

   type Hardware_Type is record
      Processor : Processor_Type;
      Devices   : Devices_Package.Map;
   end record;

   type Minor_Frame_Type is record
      Subject_Id : Natural;
      Ticks      : Positive;
   end record;

   package Minor_Frames_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Minor_Frame_Type);

   subtype CPU_Type is Minor_Frames_Package.List;

   package CPU_Package is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => CPU_Type,
      "="          => Minor_Frames_Package."=");

   subtype Major_Frame_Type is CPU_Package.Vector;

   package Major_Frames_Package is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Major_Frame_Type,
      "="          => CPU_Package."=");

   subtype Major_Frames_Type is Major_Frames_Package.List;

   type Scheduling_Type is record
      Tick_Rate    : Positive;
      Major_Frames : Major_Frames_Type;
   end record;

   type Policy_Type is record
      Vmxon_Address      : SK.Word64;
      Vmcs_Start_Address : SK.Word64;
      Hardware           : Hardware_Type;
      Kernel             : Kernel_Type;
      Subjects           : Subjects_Type;
      Binaries           : Binaries_Type;
      Scheduling         : Scheduling_Type;
   end record;

   IO_Error : exception;

end Skp;
