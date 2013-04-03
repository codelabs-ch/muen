package body SK.Subjects
is

   subtype Descriptor_Array is SK.Subject_State_Array (Skp.Subject_Id_Type);

   --  Descriptors used to manage subject states.
   Descriptors : Descriptor_Array;

   -------------------------------------------------------------------------

   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type
   is
   begin
      return Descriptors (Id);
   end Get_State;

   -------------------------------------------------------------------------

   procedure Set_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type)
   is
   begin
      Descriptors (Id) := State;
   end Set_State;

begin
   Descriptors := Descriptor_Array'
     (others => SK.Subject_State_Type'
        (Launched => False,
         Regs     => SK.CPU_Null_Regs));
end SK.Subjects;
