--
--  Copyright (C) 2013, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Skp;

package SK.Subjects_Interrupts
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers)),
   Initializes    => State
is

   --  Insert new interrupt with given vector for specified subject.
   procedure Insert_Interrupt
     (Subject : Skp.Global_Subject_ID_Type;
      Vector  : SK.Byte)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ (Vector, Subject));

   --  Return True if the subject identified by ID has interrupt(s) pending.
   procedure Has_Pending_Interrupt
     (Subject           :     Skp.Global_Subject_ID_Type;
      Interrupt_Pending : out Boolean)
   with
      Global  => (Input => State),
      Depends => (Interrupt_Pending => (Subject, State));

   --  Consume an interrupt of a subject given by ID. Returns False if no
   --  outstanding interrupt is found.
   procedure Consume_Interrupt
     (Subject :     Skp.Global_Subject_ID_Type;
      Found   : out Boolean;
      Vector  : out SK.Byte)
   with
      Global  => (In_Out => State),
      Depends => ((Vector, Found, State) => (State, Subject));

   --  Initialize pending interrupts of subject with given ID.
   procedure Init_Interrupts (Subject : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Subject);

private

   Interrupt_Count      : constant := 256;
   Bits_In_Word         : constant := 64;
   Interrupt_Words      : constant := Interrupt_Count / Bits_In_Word;
   Interrupt_Array_Size : constant := Interrupt_Count / 8;

   type Interrupt_Word_Type is range 0 .. (Interrupt_Words - 1);

   type Interrupts_Array is array (Interrupt_Word_Type) of Word64
   with
      Size => Interrupt_Array_Size * 8;

   type Padding_Type is array (1 .. Page_Size - Interrupt_Array_Size) of Byte
   with
      Size => (Page_Size - Interrupt_Array_Size) * 8;

   type Interrupt_Page is record
      Data    : Interrupts_Array;
      Padding : Padding_Type;
   end record
   with
      Size => Page_Size * 8;

   type Pending_Interrupts_Array is
     array (Skp.Global_Subject_ID_Type) of Interrupt_Page
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Object_Size    => (Skp.Global_Subject_ID_Type'Last + 1) * Page_Size * 8,
      Alignment      => Page_Size;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   Pending_Interrupts : Pending_Interrupts_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Part_Of => State,
      Address => System'To_Address (Skp.Kernel.Subj_Interrupts_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end SK.Subjects_Interrupts;
