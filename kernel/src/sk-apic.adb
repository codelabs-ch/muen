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

with SK.Apic_Mode;

package body SK.Apic
is

   Ipi_Init_Broadcast  : constant := 16#000c4500#;
   Ipi_Start_Broadcast : constant := 16#000c4600#;

   -------------------------------------------------------------------------

   pragma $Prove_Warnings (Off, "subprogram ""Busy_Wait"" has no effect",
      Reason => "By design, a busy loop has no effect except burning time.");

   --  Busy-sleep for a given (scaled) period of time.
   procedure Busy_Wait (Count : Positive)
   with
      Depends => (null => Count),
      Pre     => Count < Integer'Last / 2 ** 8
   is
   begin
      for I in Integer range 1 .. Count * (2 ** 8) loop
         null;
      end loop;
   end Busy_Wait;

   pragma $Prove_Warnings (On, "subprogram ""Busy_Wait"" has no effect");

   -------------------------------------------------------------------------

   procedure Enable renames Apic_Mode.Enable;

   -------------------------------------------------------------------------

   procedure EOI renames Apic_Mode.EOI;

   -------------------------------------------------------------------------

   function Get_ID return SK.Byte renames Apic_Mode.Get_ID;

   -------------------------------------------------------------------------

   procedure Start_AP_Processors
   is
   begin
      Apic_Mode.Write_ICR
        (Low  => Ipi_Init_Broadcast,
         High => 0);
      Busy_Wait (Count => 10);

      Apic_Mode.Write_ICR
        (Low  => Ipi_Start_Broadcast,
         High => 0);
      Busy_Wait (Count => 200);

      Apic_Mode.Write_ICR
        (Low  => Ipi_Start_Broadcast,
         High => 0);
      Busy_Wait (Count => 200);
   end Start_AP_Processors;

   -------------------------------------------------------------------------

   procedure Send_IPI
     (Vector  : SK.Byte;
      Apic_Id : SK.Byte)
   is
   begin
      Apic_Mode.Write_ICR (Low  => SK.Word32 (Vector),
                           High => SK.Word32 (Apic_Id));
   end Send_IPI;

end SK.Apic;
