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

--D @Interface
--D This package contains subprograms to interact with the FPU, i.e. to check
--D its state, enable it during startup and save as well as restore the
--D hardware FPU state to/from memory.
package SK.FPU
with
   Abstract_State => State,
   Initializes    => State
is

   --  Check validity of FPU state and return results. Is_Valid is set to True
   --  if the hardware supports the required FPU features.
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
      Global  => (In_Out => (State, X86_64.State)),
      Depends => (State        =>+ (ID, X86_64.State),
                  X86_64.State =>+ State);

   --  Restore FPU state from save area of subject specified by ID.
   procedure Restore_State (ID : Skp.Global_Subject_ID_Type)
   with
     Global  => (In_Out => (State, X86_64.State)),
     Depends => ((State, X86_64.State) =>+ (ID, State));

   --  Reset FPU state of subject with given ID, see Intel SDM Vol. 3A, "9.1.1
   --  Processor State After Reset", table 9-1, column INIT.
   procedure Reset_State (ID : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ ID);

   --  Get FPU XSAVE Legacy registers from save area of subject specified by
   --  ID.
   procedure Get_Registers
     (ID   :     Skp.Global_Subject_ID_Type;
      Regs : out XSAVE_Legacy_Header_Type)
   with
      Global  => (Input => State),
      Depends => (Regs  => (ID, State));

   --  Get the XCR0 value of active FPU features.
   function Get_Active_XCR0_Features return Word64
   with
      Global => (Input => State);

   --  Set XCR0 of subject with given ID to specified value.
   procedure Set_XCR0
     (ID    : Skp.Global_Subject_ID_Type;
      Value : Word64)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (ID, Value));

private

   --  Set the XCR0 register to the given value. The value of the physical XCR0
   --  is only update if it differs from the current value.
   procedure Write_XCR0 (Value : Word64)
   with
      Global  => (In_Out => (State, X86_64.State)),
      Depends => ((State, X86_64.State) =>+ (Value, State));

   --D @Text Section => SK.FPU.Active_XCR0_Features
   --D Active FPU features that are supported by the hardware and are enabled.
   Active_XCR0_Features : Word64 := 0
   with
      Part_Of => State;

   --D @Text Section => SK.FPU.Current_XCR0
   --D Current value of XCR0. Used to determine if write to XCR0 register is
   --D actually necessary or if it already contains that value.
   Current_XCR0 : Word64 := 0
   with
      Part_Of => State;

   function Get_Active_XCR0_Features return Word64
   is (Active_XCR0_Features);

   type Padding_Type is array (1 .. FPU_Info_Size - 8) of Byte
     with Size => (FPU_Info_Size - 8) * 8;

   --D @Interface
   --D The FPU state consist of the subject XCR0 value and the hardware-managed
   --D XSAVE area which is used to store the FPU state.
   type FPU_State_Type is record
      --D @Interface
      --D Extended Control Register 0 (XCR0).
      XCR0       : Word64;
      --D @Interface
      --D Padding in order to guarantee 64-byte alignment of XSAVE area.
      Padding    : Padding_Type;
      --D @Interface
      --D XSAVE area used to save the FPU state.
      XSAVE_Area : XSAVE_Area_Type;
   end record
   with Object_Size => Page_Size * 8;

   for FPU_State_Type use record
      XCR0       at  0 range  0 .. 63;
      Padding    at  8 range  0 .. 56 * 8 - 1;
      XSAVE_Area at 64 range  0 .. XSAVE_Area_Size * 8 - 1;
   end record;

   type Subject_FPU_State_Array is array
     (Skp.Global_Subject_ID_Type) of FPU_State_Type
   with
      Independent_Components,
      Alignment   => Page_Size,
      Object_Size => Page_Size * 8 * (Skp.Global_Subject_ID_Type'Last + 1);

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

   --D @Interface
   --D The FPU state array stores the hardware FPU state of each subject in a
   --D separate save area.
   --D Prior to the execution of a subject, its FPU state is loaded from the
   --D associated storage area into the FPU.
   --D On exit from a subject, the hardware FPU state is stored to the same
   --D area. Note that Muen performs \emph{eager} FPU state switching.
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
