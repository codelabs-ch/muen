with SK.CPU;

--# inherit
--#    SK.CPU;
package SK.Subjects
--# own
--#    Descriptors;
--# initializes
--#    Descriptors;
is

   --  Subject state.
   type Subject_State_Type is record
      Regs        : CPU.Registers_Type;
      Entry_Point : SK.Word64;
   end record;

   type Subject_Idx_Type is mod 2 ** 1;

   --  Get subject state with given index.
   function Get_State (Idx : Subject_Idx_Type) return Subject_State_Type;
   --# global
   --#    Descriptors;

   --  Set state of subject identified by index.
   procedure Set_State
     (Idx   : Subject_Idx_Type;
      State : Subject_State_Type);
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Idx, State;

end SK.Subjects;
