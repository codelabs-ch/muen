--
--  Copyright (C) 2015, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

private with System;

private with Skp.Kernel;

with X86_64;

with Skp;

with SK.Crash_Audit_Types;

package SK.FPU
with
   Abstract_State => State,
   Initializes    => State
is

   --  Check validity of FPU state and return results.
   procedure Check_State
     (Is_Valid : out Boolean;
      Ctx      : out Crash_Audit_Types.FPU_Init_Context_Type)
   with
      Global => (Input => X86_64.State);

   --  Enable floating-point unit by initializing and configuring the
   --  FPU-related hardware registers.
   procedure Enable
   with
      Global  => (In_Out => (State, X86_64.State)),
      Depends => ((State, X86_64.State) =>+ X86_64.State);

   --  Save current FPU state to save area of subject specified by ID.
   procedure Save_State (ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (Input  => X86_64.State,
                  In_Out => State),
      Depends => (State  =>+ (ID, X86_64.State));

   --  Restore FPU state from save area of subject specified by ID.
   procedure Restore_State (ID : Skp.Global_Subject_ID_Type)
   with
     Global  => (Input  => State,
                 In_Out => X86_64.State),
     Depends => (X86_64.State =>+ (ID, State));

   --  Reset FPU state of subject with given ID, see Intel SDM Vol. 3A, "9.1.1
   --  Processor State After Reset", table 9-1, column INIT.
   procedure Reset_State (ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => (State, X86_64.State)),
      Depends => ((State, X86_64.State) => (ID, State, X86_64.State));

   --  Get FPU XSAVE Legacy registers from save area of subject specified by
   --  ID.
   procedure Get_Registers
     (ID   :     Skp.Global_Subject_ID_Type;
      Regs : out XSAVE_Legacy_Header_Type)
   with
      Global  => (Input => State),
      Depends => (Regs  => (ID, State));

private

   type Subject_FPU_State_Array is array
     (Skp.Global_Subject_ID_Type) of SK.XSAVE_Area_Type
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size,
      Object_Size    => Page_Size * 8 * (Skp.Global_Subject_ID_Type'Last + 1);

   pragma Warnings
     (GNATprove, Off,
      "indirect writes to * through a potential alias are ignored",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Subject_FPU_States : Subject_FPU_State_Array
   with
      Part_Of => State,
      Address => System'To_Address (Skp.Kernel.Subj_FPU_State_Address);
   pragma Annotate
     (GNATprove, Intentional,
      "not initialized",
      "Subject FPU states are initialized by their owning CPU.");
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");
   pragma Warnings
     (GNATprove, On,
      "indirect writes to * through a potential alias are ignored");

end SK.FPU;
