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
with SK.CPU_Registry;
with SK.Dump;
with SK.IO;
with SK.IO_Apic;
with SK.KC;

package body SK.Interrupts
--# own
--#    State is ISR_List, IDT, IDT_Pointer;
is

   subtype Exception_Range is Skp.Vector_Range range 0 .. 19;

   --  ISR trampolines.
   subtype ISR_List_Type is Descriptors.ISR_Array (Exception_Range);
   --# accept Warning, 350, ISR_List, "Imported from Linker";
   ISR_List : ISR_List_Type;
   pragma Import (C, ISR_List, "isrlist");
   --# end accept;

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
   --# global
   --#    IDT_Pointer;
   --# return
   --#    IDT_Pointer;
   is
   begin
      return IDT_Pointer;
   end Get_IDT_Pointer;

   -------------------------------------------------------------------------

   procedure Init
   --# global
   --#    in     ISR_List;
   --#    in out IDT;
   --# derives
   --#    IDT from *, ISR_List;
   is
   begin
      Descriptors.Setup_IDT (ISRs => ISR_List,
                             IDT  => IDT);
   end Init;

   -------------------------------------------------------------------------

   procedure Load
   --# global
   --#    in     IDT_Pointer;
   --#    in out X86_64.State;
   --# derives
   --#    X86_64.State from *, IDT_Pointer;
   is
      --# hide Load;
   begin
      CPU.Lidt (Address => SK.Word64 (System.Storage_Elements.To_Integer
                (Value => IDT_Pointer'Address)));
   end Load;

   -------------------------------------------------------------------------

   procedure Setup_IRQ_Routing
   is
      Route   : Skp.Interrupts.IRQ_Route_Type;
      APIC_ID : SK.Byte;
   begin
      for I in Skp.Interrupts.Routing_Range loop
         Route   := Skp.Interrupts.IRQ_Routing (I);
         APIC_ID := CPU_Registry.Get_APIC_ID (CPU_ID => Route.CPU);

         pragma Debug (KC.Put_String (Item => "Routing IRQ "));
         pragma Debug (KC.Put_Byte   (Item => Route.IRQ));
         pragma Debug (KC.Put_String (Item => " as vector "));
         pragma Debug (KC.Put_Byte   (Item => SK.Byte (Route.Vector)));
         pragma Debug (KC.Put_String (Item => " to CPU "));
         pragma Debug (KC.Put_Byte   (Item => SK.Byte (Route.CPU)));
         pragma Debug (KC.Put_String (Item => " with APIC ID "));
         pragma Debug (KC.Put_Byte   (Item => APIC_ID));
         pragma Debug (KC.New_Line);

         if Skp.Interrupts.IRQ_Routing (I).Vector /= Skp.Invalid_Vector then
            IO_Apic.Route_IRQ
              (IRQ            => Skp.Interrupts.IRQ_Routing (I).IRQ,
               Vector         =>
                 SK.Byte (Skp.Interrupts.IRQ_Routing (I).Vector),
               Trigger_Mode   => IO_Apic.Edge,
               Destination_Id => APIC_ID);
         end if;
      end loop;
   end Setup_IRQ_Routing;

   -------------------------------------------------------------------------

   procedure Dispatch_Exception (Unused_Context : SK.Isr_Context_Type)
   is
   begin
      pragma Debug (Dump.Print_ISR_State (Unused_Context));
      CPU.Stop;

      --# accept F, 30, Unused_Context,
      --#    "Isr Context is used for debugging only";
   end Dispatch_Exception;

begin

   --# hide SK.Interrupts;

   IDT_Pointer := Descriptors.Create_Descriptor
     (Table_Address => SK.Word64
        (System.Storage_Elements.To_Integer (Value => IDT'Address)),
      Table_Length  => IDT'Length);
end SK.Interrupts;
