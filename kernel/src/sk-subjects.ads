with Skp;

--# inherit
--#    Skp,
--#    SK.CPU;
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

   --  Set pending event of subject specified by id to given vector.
   procedure Set_Pending_Event
     (Id     : Skp.Subject_Id_Type;
      Vector : SK.Byte);
   --# global
   --#    State;
   --# derives
   --#    State from *, Id, Vector;

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
