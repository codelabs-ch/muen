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
   Descriptors := Subject_Array'
     (others => State_Type'(Launched => False,
                            Regs     => CPU.Null_Regs));
end SK.Subjects;
