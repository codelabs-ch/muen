--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Skp;

--# inherit
--#    Skp,
--#    SK.Constants,
--#    SK.CPU,
--#    SK.VMX,
--#    X86_64;
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

   --  Get RFLAGS of subject with given ID.
   function Get_RFLAGS (Id : Skp.Subject_Id_Type) return SK.Word64;
   --# global
   --#    State;

   --  Set CR0 of subject specified by id to given value.
   procedure Set_CR0
     (Id    : Skp.Subject_Id_Type;
      Value : SK.Word64);
   --# global
   --#    State;
   --# derives
   --#    State from *, Id, Value;

   --  Get RIP of subject with given ID.
   function Get_RIP (Id : Skp.Subject_Id_Type) return SK.Word64;
   --# global
   --#    State;

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

   --  Get instruction length of subject with given ID.
   function Get_Instruction_Length (Id : Skp.Subject_Id_Type) return SK.Word64;
   --# global
   --#    State;

   --  Get interrupt information of subject with given ID.
   function Get_Interrupt_Info (Id : Skp.Subject_Id_Type) return SK.Word64;
   --# global
   --#    State;

   --  Restore non-GPR guest registers from the subject state identified by ID.
   --  GPRs are returned in Regs.
   procedure Restore_State
     (Id   :     Skp.Subject_Id_Type;
      GPRs : out SK.CPU_Registers_Type);
   --# global
   --#    in     State;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from
   --#       *,
   --#       Id,
   --#       State &
   --#    GPRs from State, Id;

   --  Save GPRs and VMCS guest data to the state of the subject identified by
   --  ID.
   procedure Save_State
     (Id   : Skp.Subject_Id_Type;
      GPRs : SK.CPU_Registers_Type);
   --# global
   --#    in out X86_64.State;
   --#    in out State;
   --# derives
   --#    State from
   --#       *,
   --#       Id,
   --#       GPRs,
   --#       X86_64.State &
   --#    X86_64.State from *;

end SK.Subjects;
