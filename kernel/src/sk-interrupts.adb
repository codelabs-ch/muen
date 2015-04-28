--
--  Copyright (C) 2013-2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System.Storage_Elements;

with SK.CPU;
with SK.Dump;
with SK.IO;

package body SK.Interrupts
with
   Refined_State => (State => (IDT, IDT_Pointer))
is

   subtype Exception_Range is Descriptors.Vector_Range range 0 .. 19;

   --  ISR trampolines.
   subtype ISR_List_Type is Descriptors.ISR_Array (Exception_Range);

   ISR_List : constant ISR_List_Type
   with
      Import,
      Convention => C,
      Link_Name  => "isrlist";

   subtype IDT_Type is Descriptors.IDT_Type (Exception_Range);

   --  IDT, see Intel SDM 3A, chapter 6.10.
   IDT : IDT_Type := IDT_Type'(others => Descriptors.Null_Gate);

   --  Interrupt table pointer, loaded into IDTR
   IDT_Pointer : Descriptors.Pseudo_Descriptor_Type;

   Pic_Cmd_Master  : constant := 16#20#;
   Pic_Cmd_Slave   : constant := 16#a0#;
   Pic_Data_Master : constant := 16#21#;
   Pic_Data_Slave  : constant := 16#a1#;

   Pit_Ch0_Data : constant := 16#40#;
   Pit_Mode     : constant := 16#43#;

   -------------------------------------------------------------------------

   procedure Disable_Legacy_PIC
   is
   begin

      --  Start initialization sequence in cascade mode

      IO.Outb (Port  => Pic_Cmd_Master,
               Value => 16#11#);
      IO.Outb (Port  => Pic_Cmd_Slave,
               Value => 16#11#);

      --  ICW2: Master PIC vector offset (32)

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#20#);

      --  ICW2: Slave PIC vector offset (40)

      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#28#);

      --  ICW3: Tell Master PIC that there is a slave PIC at IRQ2

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#04#);

      --  ICW3: Tell Slave PIC its cascade identity

      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#02#);

      --  ICW4: Enable 8086 mode

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#01#);
      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#01#);

      --  Disable slave.

      IO.Outb (Port  => Pic_Data_Slave,
               Value => 16#ff#);

      --  Disable master.

      IO.Outb (Port  => Pic_Data_Master,
               Value => 16#ff#);
   end Disable_Legacy_PIC;

   -------------------------------------------------------------------------

   procedure Disable_Legacy_PIT
   is
   begin
      IO.Outb (Port  => Pit_Mode,
               Value => 16#30#);
      IO.Outb (Port  => Pit_Ch0_Data,
               Value => 0);
      IO.Outb (Port  => Pit_Ch0_Data,
               Value => 0);
   end Disable_Legacy_PIT;

   -------------------------------------------------------------------------

   function Get_IDT_Pointer return Descriptors.Pseudo_Descriptor_Type
   with
      Refined_Global => IDT_Pointer
   is
   begin
      return IDT_Pointer;
   end Get_IDT_Pointer;

   -------------------------------------------------------------------------

   procedure Init
   with
      Refined_Global  => (In_Out => IDT),
      Refined_Depends => (IDT =>+ null)
   is
   begin
      Descriptors.Setup_IDT (ISRs => ISR_List,
                             IDT  => IDT,
                             IST  => 0);
   end Init;

   -------------------------------------------------------------------------

   procedure Load
   with
      SPARK_Mode      => Off,
      Refined_Global  => (Input  => IDT_Pointer,
                          In_Out => X86_64.State),
      Refined_Depends => (X86_64.State =>+ IDT_Pointer)
   is
   begin
      CPU.Lidt (Address => SK.Word64 (System.Storage_Elements.To_Integer
                (Value => IDT_Pointer'Address)));
   end Load;

   -------------------------------------------------------------------------

   procedure Dispatch_Exception (Unused_Context : SK.Isr_Context_Type)
   is
   begin
      pragma Debug (Dump.Print_ISR_State (Unused_Context));

      CPU.Stop;
   end Dispatch_Exception;

begin

   pragma SPARK_Mode (Off);

   IDT_Pointer := Descriptors.Create_Descriptor
     (Table_Address => SK.Word64
        (System.Storage_Elements.To_Integer (Value => IDT'Address)),
      Table_Length  => IDT'Length);
end SK.Interrupts;
