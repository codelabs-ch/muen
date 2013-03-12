with System;

with Config_Subjects.Bins;

with SK.Constants;

package body SK.Subjects
is

   type Subject_Array is array (Index_Type) of State_Type;

   --  Descriptors used to manage subjects.
   Descriptors : Subject_Array;

   --# accept Warning, 350, VMCS_Address, "Imported from Linker";
   VMCS_Address : SK.Word64;
   pragma Import (C, VMCS_Address, "vmcs_ptr");
   --# end accept;

   --# accept Warning, 350, Pagetable_Address, "Imported from Linker";
   Pagetable_Address : SK.Word64;
   pragma Import (C, Pagetable_Address, "subjects_pt_ptr");
   --# end accept;

   --# accept Warning, 350, IO_Bitmap_Address, "Imported from Linker";
   IO_Bitmap_Address : SK.Word64;
   pragma Import (C, IO_Bitmap_Address, "io_bitmap_ptr");
   --# end accept;

   --  Size of page table (4 pages).
   Pagetable_Size : constant := 4 * SK.Page_Size;

   -------------------------------------------------------------------------

   function Get_State (Idx : Index_Type) return State_Type
   is
   begin
      return Descriptors (Idx);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_State
     (Idx   : Index_Type;
      State : State_Type)
   is
   begin
      Descriptors (Idx) := State;
   end Set_State;

begin

   --# hide SK.Subjects;

   declare
      Revision, Unused_High, VMCS_Region0, VMCS_Region1 : SK.Word32;
      for VMCS_Region0'Address use System'To_Address (VMCS_Address);
      for VMCS_Region1'Address use System'To_Address
        (VMCS_Address + Page_Size);
   begin
      CPU.Get_MSR
        (Register => Constants.IA32_VMX_BASIC,
         Low      => Revision,
         High     => Unused_High);
      VMCS_Region0 := Revision;
      VMCS_Region1 := Revision;

      for S in Config_Subjects.Bins.Subjects'Range loop
         Descriptors (Index_Type (S - 1))
           := State_Type'
             (Launched          => False,
              Regs              => CPU.Null_Regs,
              Stack_Address     => Config_Subjects.Bins.Subjects
                (S).Stack_Address,
              VMCS_Address      => VMCS_Address,
              PML4_Address      => Pagetable_Address,
              IO_Bitmap_Address => IO_Bitmap_Address,
              Ctls_Exec_Pin     => Constants.VM_CTRL_PREEMPT_TIMER,
              Ctls_Exec_Proc    => Constants.VM_CTRL_IO_BITMAPS
              or Constants.VM_CTRL_SECONDARY_PROC
              or Constants.VM_CTRL_EXIT_HLT
              or Constants.VM_CTRL_EXIT_INVLPG
              or Constants.VM_CTRL_EXIT_MWAIT
              or Constants.VM_CTRL_EXIT_RDPMC
              or Constants.VM_CTRL_EXIT_RDTSC
              or Constants.VM_CTRL_EXIT_CR3_LOAD
              or Constants.VM_CTRL_EXIT_CR3_STORE
              or Constants.VM_CTRL_EXIT_CR8_LOAD
              or Constants.VM_CTRL_EXIT_CR8_STORE
              or Constants.VM_CTRL_EXIT_MOV_DR
              or Constants.VM_CTRL_EXIT_MONITOR,
              Ctls_Exec_Proc2   => Constants.VM_CTRL_EXIT_DT
              or Constants.VM_CTRL_EXIT_WBINVD,
              Entry_Point       => Config_Subjects.Bins.Subjects
                (S).Entry_Point);

         VMCS_Address      := VMCS_Address      + Page_Size;
         Pagetable_Address := Pagetable_Address + Pagetable_Size;
      end loop;
   end;
end SK.Subjects;
