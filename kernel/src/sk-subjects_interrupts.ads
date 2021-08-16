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

--D @Interface
--D This package provides facilities for managing subject interrupts. An
--D interrupt is identified by the vector number and can be marked as pending.
--D Pending interrupts are injected upon resumption of a subject.
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

   --D @Interface
   --D An subject interrupt page consist of the pending interrupt data and is
   --D padded to a full 4K memory page. Explicit padding makes sure the entirety
   --D of the memory is covered and initialized.
   type Interrupt_Page is record
      --D @Interface
      --D Pending interrupts stored in the form of a bitmap.
      Data    : Interrupts_Array;
      --D @Interface
      --D Padding to fill the memory page.
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
   --D @Interface
   --D Bitmap of the currently pending subject interrupts. The CPU executing
   --D the associated subject consumes one pending interrupt prior to resuming
   --D the subject if it is in a state to accept interrupts.
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
