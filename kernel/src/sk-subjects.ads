with Skp.Subjects;

with SK.CPU;

--# inherit
--#    Skp.Subjects,
--#    SK.CPU;
package SK.Subjects
--# own
--#    Descriptors,
--#    VMCS_Address;
--# initializes
--#    Descriptors,
--#    VMCS_Address;
is

   --  Subject state.
   type State_Type is record
      Launched        : Boolean;
      Regs            : CPU.Registers_Type;
      Stack_Address   : SK.Word64;
      VMCS_Address    : SK.Word64;
      Ctls_Exec_Pin   : SK.Word32;
      Ctls_Exec_Proc  : SK.Word32;
      Ctls_Exec_Proc2 : SK.Word32;
      Entry_Point     : SK.Word64;
   end record;

   --  Get state of subject with given ID.
   function Get_State (Id : Skp.Subjects.Subject_Id_Type) return State_Type;
   --# global
   --#    Descriptors;

   --  Set state of subject identified by ID.
   procedure Set_State
     (Id    : Skp.Subjects.Subject_Id_Type;
      State : State_Type);
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, State;

end SK.Subjects;
