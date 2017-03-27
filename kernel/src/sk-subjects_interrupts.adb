--
--  Copyright (C) 2013-2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013-2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with System;

with Skp.Kernel;

package body SK.Subjects_Interrupts
with
   Refined_State => (State => Pending_Interrupts)
is

   subtype Interrupt_Range is SK.Byte range 0 .. 255;

   type Interrupts_Array is array (Interrupt_Range) of Boolean
   with
      Pack;

   pragma Warnings (GNAT, Off, "*padded by * bits");
   type Pending_Interrupts_Array is
     array (Skp.Subject_Id_Type) of Interrupts_Array
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size;
   pragma Warnings (GNAT, On, "*padded by * bits");

   Pending_Interrupts : Pending_Interrupts_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Address => System'To_Address (Skp.Kernel.Subj_Interrupts_Address);

   -------------------------------------------------------------------------

   procedure Init_Interrupts (Subject : Skp.Subject_Id_Type)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => (Pending_Interrupts =>+ Subject)
   is
   begin
      for I in Interrupt_Range loop
         Pending_Interrupts (Subject) (I) := False;
      end loop;
   end Init_Interrupts;

   -------------------------------------------------------------------------

   procedure Insert_Interrupt
     (Subject : Skp.Subject_Id_Type;
      Vector  : SK.Byte)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => (Pending_Interrupts =>+ (Vector, Subject))
   is
   begin
      Pending_Interrupts (Subject) (Vector) := True;
   end Insert_Interrupt;

   -------------------------------------------------------------------------

   procedure Has_Pending_Interrupt
     (Subject           :     Skp.Subject_Id_Type;
      Interrupt_Pending : out Boolean)
   with
      Refined_Global  => Pending_Interrupts,
      Refined_Depends => (Interrupt_Pending => (Subject, Pending_Interrupts))
   is
   begin
      Interrupt_Pending := False;

      Search_Pending :
      for I in reverse Interrupt_Range loop
         declare
            Intr : constant Boolean := Pending_Interrupts (Subject) (I);
         begin
            if Intr then
               Interrupt_Pending := True;
               exit Search_Pending;
            end if;
         end;
      end loop Search_Pending;
   end Has_Pending_Interrupt;

   -------------------------------------------------------------------------

   procedure Consume_Interrupt
     (Subject :     Skp.Subject_Id_Type;
      Found   : out Boolean;
      Vector  : out SK.Byte)
   with
      Refined_Global  => (In_Out => Pending_Interrupts),
      Refined_Depends => ((Vector, Found, Pending_Interrupts) =>
                              (Pending_Interrupts, Subject))
   is
   begin
      Found  := False;
      Vector := 0;

      Search_Interrupt :
      for I in reverse Interrupt_Range loop
         declare
            Intr : constant Boolean := Pending_Interrupts (Subject) (I);
         begin
            if Intr then
               Found  := True;
               Vector := I;
               Pending_Interrupts (Subject) (I) := False;
               exit Search_Interrupt;
            end if;
         end;
      end loop Search_Interrupt;
   end Consume_Interrupt;

end SK.Subjects_Interrupts;
