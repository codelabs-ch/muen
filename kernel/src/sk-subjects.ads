with Skp;

--# inherit
--#    Skp,
--#    SK.CPU;
package SK.Subjects
--# own
--#    Descriptors : Descriptor_Array;
--# initializes
--#    Descriptors;
is

   subtype Descriptor_Array is SK.Subject_State_Array (Skp.Subject_Id_Type);

   --  Get state of subject with given ID.
   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type;
   --# global
   --#    Descriptors;
   --# return
   --#    Descriptors (Id);

   --  Set state of subject identified by ID.
   procedure Set_State
     (Id    : Skp.Subject_Id_Type;
      State : SK.Subject_State_Type);
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, State;
   --# post
   --#    Descriptors (Id) =  Descriptors (Id)
   --#      [Launched           => State.Launched;
   --#       Regs               => State.Regs;
   --#       Exit_Reason        => State.Exit_Reason;
   --#       Exit_Qualification => State.Exit_Qualification;
   --#       Pending_Event      => Descriptors~ (Id).Pending_Event;
   --#       Interrupt_Info     => State.Interrupt_Info;
   --#       Instruction_Len    => State.Instruction_Len;
   --#       RIP                => State.RIP;
   --#       CS                 => State.CS;
   --#       RSP                => State.RSP;
   --#       SS                 => State.SS;
   --#       CR0                => State.CR0;
   --#       CR3                => State.CR3;
   --#       CR4                => State.CR4;
   --#       RFLAGS             => State.RFLAGS];

   --  Set pending event of subject specified by id to given vector.
   procedure Set_Pending_Event
     (Id     : Skp.Subject_Id_Type;
      Vector : SK.Byte);
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Vector;
   --# post
   --#    Descriptors (Id).Pending_Event = Vector;

   --  Set RIP of subject specified by id to given value.
   procedure Set_RIP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64);
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RIP = Value;

   --  Set RSP of subject specified by id to given value.
   procedure Set_RSP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64);
   --# global
   --#    Descriptors;
   --# derives
   --#    Descriptors from *, Id, Value;
   --# post
   --#    Descriptors (Id).RSP = Value;

end SK.Subjects;
