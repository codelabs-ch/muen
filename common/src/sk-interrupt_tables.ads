--
--  Copyright (C) 2013, 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with X86_64;

with SK.Task_State;
with SK.Descriptors;

package SK.Interrupt_Tables
with
   Abstract_State => State,
   Initializes    => State
is

   --  The interrupt tables manager initializes all tables (i.e. GDT, IDT, TSS)
   --  required to handle exceptions/interrupts using a separate interrupt
   --  stack.
   type Manager_Type is private;

   --  Initialize interrupt handling using the given interrupt stack address.
   procedure Initialize
     (Manager    : out Manager_Type;
      Stack_Addr :     Word64)
   with
      Global => (Input => State, In_Out => X86_64.State);

   --  Return base addresses of GDT/IDT/TSS tables.
   procedure Get_Base_Addresses
     (Manager :     Manager_Type;
      GDT     : out Word64;
      IDT     : out Word64;
      TSS     : out Word64);

private

   use type SK.Descriptors.Vector_Range;

   subtype ISR_Array is Descriptors.ISR_Array (Descriptors.Vector_Range);

   IDT_Type_Size : constant := 256 * 16 * 8;

   subtype IDT_Type is Descriptors.IDT_Type (Descriptors.Vector_Range);

   GDT_Type_Size : constant := 5 * 8 * 8;

   type GDT_Type is array (1 .. 5) of Word64
   with
      Size      => GDT_Type_Size,
      Alignment => 8;

   type Manager_Type is record
      GDT            : GDT_Type;
      IDT            : IDT_Type;
      TSS            : Task_State.TSS_Type;
      GDT_Descriptor : Pseudo_Descriptor_Type;
      IDT_Descriptor : Pseudo_Descriptor_Type;
   end record;

   TSS_Type_Size : constant := 104 * 8;
   Descr_Size    : constant := 10 * 8;

   GDT_Offsetbits  : constant := 0;
   IDT_Offsetbits  : constant := GDT_Type_Size;
   TSS_Offsetbits  : constant := IDT_Offsetbits  + IDT_Type_Size;
   GDTD_Offsetbits : constant := TSS_Offsetbits  + TSS_Type_Size;
   IDTD_Offsetbits : constant := GDTD_Offsetbits + Descr_Size;

   for Manager_Type use record
      GDT            at 0                   range 0 .. GDT_Type_Size - 1;
      IDT            at IDT_Offsetbits / 8  range 0 .. IDT_Type_Size - 1;
      TSS            at TSS_Offsetbits / 8  range 0 .. TSS_Type_Size - 1;
      GDT_Descriptor at GDTD_Offsetbits / 8 range 0 .. Descr_Size - 1;
      IDT_Descriptor at IDTD_Offsetbits / 8 range 0 .. Descr_Size - 1;
   end record;

end SK.Interrupt_Tables;
