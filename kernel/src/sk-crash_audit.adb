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

with System.Machine_Code;

with Interfaces;

with Skp.Kernel;

with SK.Dump;
with SK.CPU;
with SK.Delays;
with SK.Power;
with SK.Version;
with SK.Strings;
with SK.Constants;

package body SK.Crash_Audit
with
   Refined_State => (State => (Global_Next_Slot, Instance))
is

   --  100 ms delay before warm reset.
   Reset_Delay : constant := 100000;

   --D @Interface
   --D Index of next free crash audit dump slot. It can be read and written by
   --D all CPUs. Data consistency is established via atomic access.
   Global_Next_Slot : Positive := Positive'First
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Linker_Section => Constants.Global_Data_Section;

   type Padding_Type is
     array (Crash_Audit_Types.Dump_Type_Size + 1 .. Page_Size) of Byte
   with
      Size => (Page_Size - Crash_Audit_Types.Dump_Type_Size) * 8;

   --D @Interface
   --D A crash audit page consist of the crash dump data and is padded to
   --D a full 4K memory page. Explicit padding makes sure the entirety of the
   --D memory is covered and initialized.
   type Crash_Audit_Page is record
      --D @Interface
      --D Crash information containing the entire crash audit dump data.
      Crash_Info : Crash_Audit_Types.Dump_Type;
      --D @Interface
      --D Padding to fill the memory page.
      Padding    : Padding_Type;
   end record
   with
      Object_Size => Page_Size * 8;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   --D @Interface
   --D Crash Audit Store. It provides storage space for multiple data sets of
   --D crash audit information and associated meta data.
   Instance : Crash_Audit_Page
   with
      Volatile,
      Import,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Skp.Kernel.Crash_Audit_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   pragma Compile_Time_Error
     (Instance'Size <= Skp.Kernel.Crash_Audit_Size,
      "Crash audit region too small");

   -------------------------------------------------------------------------

   --  Atomically retrieve next slot index and increment Global_Next_Slot
   --  variable.
   procedure Get_And_Inc (Slot : out Positive)
   with
      Global => (In_Out => Global_Next_Slot);

   procedure Get_And_Inc (Slot : out Positive)
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "movq $1, %%rax; lock xadd %%eax, %0",
         Outputs  => (Positive'Asm_Output ("=m", Global_Next_Slot),
                      Positive'Asm_Output ("=a", Slot)),
         Volatile => True,
         Clobber  => "cc");
   end Get_And_Inc;

   -------------------------------------------------------------------------

   --  Atomically increment Crash_Count.
   procedure Atomic_Inc_Crash_Count
   with
      Global => (In_Out => Instance);

   procedure Atomic_Inc_Crash_Count
   with
      SPARK_Mode => Off
   is
   begin
      System.Machine_Code.Asm
        (Template => "lock incq %0",
         Outputs  => (Interfaces.Unsigned_64'Asm_Output
                       ("=m", Instance.Crash_Info.Header.Crash_Count)),
         Volatile => True);
   end Atomic_Inc_Crash_Count;

   -------------------------------------------------------------------------

   --D @Section Id => impl_crash_audit, Label => Crash Audit, Parent => implementation
   --D @Section Id => impl_crash_audit_init, Label => Initialization, Parent => impl_crash_audit
   --D @Text Section => impl_crash_audit_init
   --D Initialization of the Crash Audit facility puts the crash audit store in
   --D a well-defined state in order to be ready for the addition of new audit
   --D entries in case of a crash.
   --D @OL Id => impl_crash_audit_init_steps, Section => impl_crash_audit_init
   procedure Init
   is
      H : constant Header_Type := Instance.Crash_Info.Header;
   begin
      if H.Version_Magic /= Crash_Magic then
         --D @Item List => impl_crash_audit_init_steps
         --D Initialize the audit instance to the well-known empty state if the
         --D crash audit does not have a matching version number.
         Instance.Crash_Info := Null_Dump;
         pragma Debug (Dump.Print_Message
                       (Msg => "Crash audit: Initialized"));
      else
         --D @Item List => impl_crash_audit_init_steps
         --D Increase the boot counter but retain current audit data, if it is
         --D already initialized.
         Instance.Crash_Info.Header.Boot_Count := H.Boot_Count + 1;
         pragma Debug
           (Dump.Print_Message
              (Msg => "Crash audit: Reset detected, setting boot count to "
               & Strings.Img (H.Boot_Count + 1)));
      end if;

      pragma Debug (H.Crash_Count > 0
                    and then H.Boot_Count + 1 = H.Generation,
                    Dump.Print_Message
                      (Msg => "Crash audit: Records found, dump count is "
                       & Strings.Img (Byte (H.Dump_Count))));
   end Init;

   -------------------------------------------------------------------------

   --D @Section Id => impl_crash_audit_alloc, Label => Allocation, Parent => impl_crash_audit, Priority => 10
   --D @Text Section => impl_crash_audit_alloc
   --D Allocate a global crash audit entry termed \emph{slot}. For a full
   --D description of the crash audit entry data structure see section
   --D \ref{SK.Crash_Audit_Types}.
   --D @OL Id => impl_crash_audit_alloc_steps, Section => impl_crash_audit_alloc
   procedure Allocate (Audit : out Entry_Type)
   is
      S : Positive;
   begin
      --D @Item List => impl_crash_audit_alloc_steps
      --D Initialize the audit entry.
      Audit := Null_Entry;

      --D @Item List => impl_crash_audit_alloc_steps
      --D Atomically get and increment the audit slot index. If no free audit
      --D slot is available, halt execution.
      Get_And_Inc (Slot => S);
      if S > Max_Dumps then
         pragma Debug
           (Dump.Print_Message
              (Msg => "Crash audit: Unable to allocate record, halting CPU "
               & "- slot count is " & Strings.Img (Byte (S))));
         CPU.Stop;
      end if;

      --D @Item List => impl_crash_audit_alloc_steps
      --D Set index of current audit slot.
      Audit.Slot := Dumpdata_Index (S);
      pragma Debug (Dump.Print_Message
                    (Msg => "Crash audit: CPU APIC ID "
                     & Strings.Img (Byte (CPU_Info.APIC_ID))
                     & " - Allocated record "
                     & Strings.Img (Byte (Audit.Slot))));

      --D @Item List => impl_crash_audit_alloc_steps
      --D Clear crash dump fields of current audit slot.
      Instance.Crash_Info.Data (Audit.Slot) := Null_Dumpdata;

      --D @Item List => impl_crash_audit_alloc_steps
      --D Set crash data APIC ID to this CPU.
      Instance.Crash_Info.Data (Audit.Slot).APIC_ID := Byte (CPU_Info.APIC_ID);
      --D @Item List => impl_crash_audit_alloc_steps
      --D Set crash data timestamp to the current TSC value.
      Instance.Crash_Info.Data (Audit.Slot).TSC_Value := CPU.RDTSC;
   end Allocate;

   -------------------------------------------------------------------------

   --D @Section Id => impl_crash_audit_final, Label => Finalization, Parent => impl_crash_audit, Priority => 10
   --D @Text Section => impl_crash_audit_final
   --D Finalize the given audit slot.
   --D @OL Id => impl_crash_audit_final_steps, Section => impl_crash_audit_final
   procedure Finalize (Audit : Entry_Type)
   is
      pragma Unreferenced (Audit);
      --  Audit token authorizes to finalize crash dump and restart.

      Next  : constant Positive := Global_Next_Slot;
      Boots : constant Interfaces.Unsigned_64
        := Instance.Crash_Info.Header.Boot_Count;
   begin
      if Next > Positive (Dumpdata_Length'Last) then
         --D @Item List => impl_crash_audit_final_steps
         --D Set active crash dump count to the last index if the next slot
         --D index is too large.
         Instance.Crash_Info.Header.Dump_Count := Dumpdata_Length'Last;
      else
         --D @Item List => impl_crash_audit_final_steps
         --D Set active crash dump count to the current slot index.
         Instance.Crash_Info.Header.Dump_Count := Dumpdata_Length (Next - 1);
      end if;

      for I in Version.Version_String'Range loop
         --D @Item List => impl_crash_audit_final_steps
         --D Set the version string in the header to the current version.
         Instance.Crash_Info.Header.Version_String (I)
           := Version.Version_String (I);
      end loop;

      --D @Item List => impl_crash_audit_final_steps
      --D Increase the generation.
      Instance.Crash_Info.Header.Generation := Boots + 1;
      --D @Item List => impl_crash_audit_final_steps
      --D Increase the crash counter.
      Atomic_Inc_Crash_Count;

      --D @Item List => impl_crash_audit_final_steps
      --D Pause for a given amount before rebooting the system to enable
      --D potentially simultaneously faulting cores to finish writing their
      --D crash audit entries.
      Delays.U_Delay (US => Reset_Delay);
      pragma Debug (Dump.Print_Message
                    (Msg => "Crash audit: CPU APIC ID "
                     & Strings.Img (Byte (CPU_Info.APIC_ID))
                     & " - Initiating reboot in 10 seconds ..."));
      pragma Debug (Delays.U_Delay (US => 10 * 10 ** 6));
      Power.Reboot (Power_Cycle => False);
   end Finalize;

   -------------------------------------------------------------------------

   procedure Set_Exception_Context
     (Audit   : Entry_Type;
      Context : Exception_Context_Type)
   is
   begin
      Instance.Crash_Info.Data (Audit.Slot).Exception_Context := Context;
      Instance.Crash_Info.Data (Audit.Slot).Field_Validity.Ex_Context := True;
   end Set_Exception_Context;

   -------------------------------------------------------------------------

   procedure Set_Init_Context
     (Audit   : Entry_Type;
      Context : Init_Context_Type)
   is
   begin
      Instance.Crash_Info.Data (Audit.Slot).Init_Context := Context;
      Instance.Crash_Info.Data (Audit.Slot).Field_Validity.Init_Context
        := True;
   end Set_Init_Context;

   -------------------------------------------------------------------------

   procedure Set_MCE_Context
     (Audit   : Entry_Type;
      Context : MCE_Context_Type)
   is
   begin
      Instance.Crash_Info.Data (Audit.Slot).MCE_Context := Context;
      Instance.Crash_Info.Data (Audit.Slot).Field_Validity.MCE_Context := True;
   end Set_MCE_Context;

   -------------------------------------------------------------------------

   procedure Set_Reason
     (Audit  : in out Entry_Type;
      Reason :        Reason_Type)
   is
   begin
      Instance.Crash_Info.Data (Audit.Slot).Reason := Reason;
      Audit.Reason := Reason;
   end Set_Reason;

   -------------------------------------------------------------------------

   procedure Set_Subject_Context
     (Audit   : Entry_Type;
      Context : Subj_Context_Type)
   is
   begin
      Instance.Crash_Info.Data (Audit.Slot).Subject_Context := Context;
      Instance.Crash_Info.Data (Audit.Slot).Field_Validity.Subj_Context
        := True;
   end Set_Subject_Context;

   -------------------------------------------------------------------------

   procedure Set_VTx_Context
     (Audit   : Entry_Type;
      Context : VTx_Context_Type)
   is
   begin
      Instance.Crash_Info.Data (Audit.Slot).VTx_Context := Context;
      Instance.Crash_Info.Data (Audit.Slot).Field_Validity.VTx_Context := True;
   end Set_VTx_Context;

end SK.Crash_Audit;
