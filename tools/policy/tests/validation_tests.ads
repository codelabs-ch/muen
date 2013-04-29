with Ahven.Framework;

package Validation_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Verify invalid memory region size check.
   procedure Invalid_Memregion_Size;

   --  Verify invalid memory region addresses.
   procedure Invalid_Memregion_Addrs;

   --  Verify invalid VMXON addresses.
   procedure Invalid_Vmxon_Addrs;

   --  Verify invalid VMCS addresses.
   procedure Invalid_Vmcs_Addrs;

   --  Verify scheduling CPU elements with different total tick count.
   procedure Invalid_Sched_CPU_Ticks;

   --  Verify scheduling major frame with incorrect CPU element count.
   procedure Invalid_Sched_CPU_Count;

   --  Verify subject with invalid PML4 address.
   procedure Invalid_Subj_Pml4_Addr;

   --  Verify subject with invalid I/O bitmap address.
   procedure Invalid_Subj_IO_Bitmap_Addr;

   --  Verify subject with invalid binary reference.
   procedure Invalid_Subj_Binary;

   --  Verify subject with trap table entry self-reference.
   procedure Invalid_Subj_Trap_Self_Ref;

   --  Verify subject trap table entry with invalid destination subject.
   procedure Invalid_Subj_Trap_Dst;

   --  Verify subject with signal table entry self-reference.
   procedure Invalid_Subj_Signal_Self_Ref;

   --  Verify subject signal table entry with invalid destination subject.
   procedure Invalid_Subj_Signal_Dst;

   --  Verify subject signal table entry with invalid destination vector.
   procedure Invalid_Subj_Signal_Dst_Vec;

   --  Verify device with non-unique IRQ.
   procedure Invalid_Device_IRQ;

   --  Validate policy.
   procedure Policy_Validation;

end Validation_Tests;
