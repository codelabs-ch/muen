with SK.CPU;

--# inherit
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
      Launched      : Boolean;
      Regs          : CPU.Registers_Type;
      Stack_Address : SK.Word64;
      VMCS_Address  : SK.Word64;
      Entry_Point   : SK.Word64;
   end record;

   type Index_Type is mod 2 ** 1;

   --  Get subject state with given index.
   function Get_State (Idx : Index_Type) return State_Type;
   --# global
   --#    Descriptors;

   --  Set state of subject identified by index.
   procedure Set_State
     (Idx   : Index_Type;
      State : State_Type);
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Idx, State;

end SK.Subjects;
