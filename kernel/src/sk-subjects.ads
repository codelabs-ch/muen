with Skp;

--# inherit
--#    Skp,
--#    SK.CPU,
--#    SK.Locks;
package SK.Subjects
--# own
--#    State;
--# initializes
--#    State;
is

   --  Get state of subject with given ID.
   function Get_State (Id : Skp.Subject_Id_Type) return SK.Subject_State_Type;
   --# global
   --#    State;

   --  Set state of subject identified by ID.
   procedure Set_State
     (Id            : Skp.Subject_Id_Type;
      Subject_State : SK.Subject_State_Type);
   --# global
   --#    State;
   --# derives
   --#    State from *, Id, Subject_State;

   --  Return True if the subject identified by id has events pending.
   function Has_Pending_Events (Id : Skp.Subject_Id_Type) return Boolean;
   --# global
   --#    State;

   --  Set pending event of subject specified by id to given vector.
   procedure Set_Pending_Event
     (Id     : Skp.Subject_Id_Type;
      Vector : SK.Byte);
   --# global
   --#    in out State;
   --#    in out Locks.State;
   --# derives
   --#    State       from *, Id, Vector &
   --#    Locks.State from *;

   --  Return pending event of subject identified by id. If no event is
   --  pending, zero is returned.
   procedure Get_Pending_Event
     (Id    :     Skp.Subject_Id_Type;
      Event : out SK.Byte);
   --# global
   --#    in out State;
   --#    in out Locks.State;
   --# derives
   --#    State       from *, Id     &
   --#    Event       from State, Id &
   --#    Locks.State from *;

   --  Set RIP of subject specified by id to given value.
   procedure Set_RIP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64);
   --# global
   --#    State;
   --# derives
   --#    State from *, Id, Value;

   --  Set RSP of subject specified by id to given value.
   procedure Set_RSP
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64);
   --# global
   --#    State;
   --# derives
   --#    State from *, Id, Value;

end SK.Subjects;
