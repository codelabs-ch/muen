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

   Global_Next_Slot : Positive := Positive'First
   with
      Volatile,
      Async_Readers,
      Async_Writers,
      Linker_Section => Constants.Global_Data_Section;

   pragma Warnings
     (GNAT, Off, "* bits of ""Instance"" unused",
      Reason => "We only care if the region is too small");
   Instance : Dump_Type
   with
      Volatile,
      Import,
      Async_Readers,
      Async_Writers,
      Address => System'To_Address (Skp.Kernel.Crash_Audit_Address),
      Size    => Skp.Kernel.Crash_Audit_Size * 8;
   pragma Warnings (GNAT, On, "* bits of ""Instance"" unused");

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

   procedure Init
   is
      H : constant Header_Type := Instance.Header;
   begin
      if H.Version_Magic /= Crash_Magic then
         Instance := Null_Dump;
         pragma Debug (Dump.Print_Message
                       (Msg => "Crash audit: Initialized"));
      else
         Instance.Header.Boot_Count := H.Boot_Count + 1;
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

   procedure Allocate (Audit : out Entry_Type)
   is
      S : Positive;
   begin
      Audit := Null_Entry;

      Get_And_Inc (Slot => S);
      if S > Max_Dumps then
         pragma Debug
           (Dump.Print_Message
              (Msg => "Crash audit: Unable to allocate record, halting CPU "
               & "- slot count is " & Strings.Img (Byte (S))));
         CPU.Stop;
      end if;

      Audit.Slot := Dumpdata_Index (S);
      pragma Debug (Dump.Print_Message
                    (Msg => "Crash audit: CPU APIC ID "
                     & Strings.Img (Byte (CPU_Info.APIC_ID))
                     & " - Allocated record "
                     & Strings.Img (Byte (Audit.Slot))));

      Instance.Data (Audit.Slot) := Null_Dumpdata;

      Instance.Data (Audit.Slot).APIC_ID   := Byte (CPU_Info.APIC_ID);
      Instance.Data (Audit.Slot).TSC_Value := CPU.RDTSC;
   end Allocate;

   -------------------------------------------------------------------------

   procedure Finalize (Audit : Entry_Type)
   is
      pragma Unreferenced (Audit);
      --  Audit token authorizes to finalize crash dump and restart.

      Next   : constant Positive               := Global_Next_Slot;
      Boots  : constant Interfaces.Unsigned_64 := Instance.Header.Boot_Count;
      Crashs : constant Interfaces.Unsigned_64 := Instance.Header.Crash_Count;
   begin
      if Next > Positive (Dumpdata_Length'Last) then
         Instance.Header.Dump_Count := Dumpdata_Length'Last;
      else
         Instance.Header.Dump_Count := Dumpdata_Length (Next - 1);
      end if;

      for I in Version.Version_String'Range loop
         Instance.Header.Version_String (I) := Version.Version_String (I);
      end loop;

      Instance.Header.Generation  := Boots  + 1;
      Instance.Header.Crash_Count := Crashs + 1;

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
      Instance.Data (Audit.Slot).Exception_Context := Context;
      Instance.Data (Audit.Slot).Field_Validity.Ex_Context := True;
   end Set_Exception_Context;

   -------------------------------------------------------------------------

   procedure Set_Init_Context
     (Audit   : Entry_Type;
      Context : Init_Context_Type)
   is
   begin
      Instance.Data (Audit.Slot).Reason := System_Init_Failure;
      Instance.Data (Audit.Slot).Init_Context := Context;
      Instance.Data (Audit.Slot).Field_Validity.Init_Context := True;
   end Set_Init_Context;

   -------------------------------------------------------------------------

   procedure Set_MCE_Context
     (Audit   : Entry_Type;
      Context : MCE_Context_Type)
   is
   begin
      Instance.Data (Audit.Slot).MCE_Context := Context;
      Instance.Data (Audit.Slot).Field_Validity.MCE_Context := True;
   end Set_MCE_Context;

   -------------------------------------------------------------------------

   procedure Set_Reason
     (Audit  : in out Entry_Type;
      Reason :        Reason_Type)
   is
   begin
      Instance.Data (Audit.Slot).Reason := Reason;
      Audit.Reason := Reason;
   end Set_Reason;

   -------------------------------------------------------------------------

   procedure Set_Subject_Context
     (Audit   : Entry_Type;
      Context : Subj_Context_Type)
   is
   begin
      Instance.Data (Audit.Slot).Subject_Context := Context;
      Instance.Data (Audit.Slot).Field_Validity.Subj_Context := True;
   end Set_Subject_Context;

   -------------------------------------------------------------------------

   procedure Set_VTx_Context
     (Audit   : Entry_Type;
      Context : VTx_Context_Type)
   is
   begin
      Instance.Data (Audit.Slot).VTx_Context := Context;
      Instance.Data (Audit.Slot).Field_Validity.VTx_Context := True;
   end Set_VTx_Context;

end SK.Crash_Audit;
