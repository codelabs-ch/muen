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

with System.Storage_Elements;

with Skp.Interrupts;

with SK.CPU;
with SK.Descriptors;
with SK.Dump;
with SK.IO;

use type SK.Descriptors.Pseudo_Descriptor_Type;

package body SK.Interrupts
with
   Refined_State => (State =>  (IDT, IDT_Pointer))
is

   subtype Exception_Range is Skp.Vector_Range range 0 .. 19;

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

   -------------------------------------------------------------------------

   procedure Disable_Legacy_PIC
   is
   begin

      --  Disable slave.

      IO.Outb (Port  => 16#a1#,
               Value => 16#ff#);

      --  Disable master.

      IO.Outb (Port  => 16#21#,
               Value => 16#ff#);
   end Disable_Legacy_PIC;

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

   procedure Setup_IRQ_Routing (VTd_Enabled : Boolean)
   is
      Route    : Skp.Interrupts.IRQ_Route_Type;
      Dest_ID  : SK.Byte;
      Shiftpos : Natural;
   begin
      for I in Skp.Interrupts.Routing_Range loop
         Route := Skp.Interrupts.IRQ_Routing (I);

         if VTd_Enabled then

            --  See Intel VT-d specification, section 5.5.1.

            Shiftpos := 49;
            Dest_ID  := Route.IRQ;
         else

            --  See Intel IOAPIC specification, section 3.2.4.

            Shiftpos := 56;
            Dest_ID  := CPU_Registry.Get_APIC_ID (CPU_ID => Route.CPU);
         end if;

         pragma Debug (Dump.Print_IRQ_Routing
                       (IRQ         => Route.IRQ,
                        Vector      => SK.Byte (Route.Vector),
                        CPU_ID      => SK.Byte (Route.CPU),
                        Dest_ID     => Dest_ID,
                        VTd_Enabled => VTd_Enabled));

         if Route.Vector /= Skp.Invalid_Vector then
            IO_Apic.Route_IRQ
              (IRQ            => Route.IRQ,
               Vector         => SK.Byte (Route.Vector),
               Trigger_Mode   => Route.IRQ_Mode,
               Trigger_Level  => Route.IRQ_Level,
               Destination_Id => SK.Word64 (Dest_ID) * 2 ** Shiftpos);
         end if;
      end loop;
   end Setup_IRQ_Routing;

   -------------------------------------------------------------------------

   procedure Dispatch_Exception (Unused_Context : SK.Isr_Context_Type)
   is
   begin
      pragma Debug (Dump.Print_ISR_State (Unused_Context));

      pragma Assume (False); --  Workaround for No_Return: Pre => False
      CPU.Stop;
   end Dispatch_Exception;

begin

   pragma SPARK_Mode (Off);

   IDT_Pointer := Descriptors.Create_Descriptor
     (Table_Address => SK.Word64
        (System.Storage_Elements.To_Integer (Value => IDT'Address)),
      Table_Length  => IDT'Length);
end SK.Interrupts;
