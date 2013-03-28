with Skc.Subjects;

with SK.Constants;

package body SK.Subjects
is

   type Subject_Array is array (Skp.Subject_Id_Type) of State_Type;

   --  Descriptors used to manage subjects.
   Descriptors : Subject_Array;

   -------------------------------------------------------------------------

   function Get_State (Id : Skp.Subject_Id_Type) return State_Type
   is
   begin
      return Descriptors (Id);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_State
     (Id    : Skp.Subject_Id_Type;
      State : State_Type)
   is
   begin
      Descriptors (Id) := State;
   end Set_State;

begin

   --#  hide SK.Subjects;

   for S in Skc.Subjects.Binaries'Range loop
      Descriptors (Skp.Subject_Id_Type (S - 1))
        := State_Type'
          (Launched        => False,
           Regs            => CPU.Null_Regs,
           Stack_Address   => Skc.Subjects.Binaries (S).Stack_Address,
           Ctls_Exec_Pin   => Constants.VM_CTRL_PREEMPT_TIMER,
           Ctls_Exec_Proc  => Constants.VM_CTRL_IO_BITMAPS
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
           Ctls_Exec_Proc2 => Constants.VM_CTRL_EXIT_DT
           or Constants.VM_CTRL_EXIT_WBINVD,
           Entry_Point     => Skc.Subjects.Binaries (S).Entry_Point);
   end loop;

end SK.Subjects;
