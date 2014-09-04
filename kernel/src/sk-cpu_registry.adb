--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.Dump;

package body SK.CPU_Registry
with
   Refined_State => (State => CPUs)
is

   type ID_Array is array (Skp.CPU_Range) of SK.Byte;

   CPUs : ID_Array := ID_Array'(others => 0);

   -------------------------------------------------------------------------

   procedure Register
     (CPU_ID  : Skp.CPU_Range;
      APIC_ID : SK.Byte)
   with
      Refined_Global  => (In_Out => CPUs),
      Refined_Depends => (CPUs =>+ (CPU_ID, APIC_ID))
   is
   begin
      CPUs (CPU_ID) := APIC_ID;
      pragma Debug (Dump.Print_CPU_IDs
                    (CPU_ID  => SK.Byte (CPU_ID),
                     APIC_ID => APIC_ID));
   end Register;

   -------------------------------------------------------------------------

   function Get_APIC_ID (CPU_ID : Skp.CPU_Range) return SK.Byte
   with
      Refined_Global => CPUs
   is
   begin
      return CPUs (CPU_ID);
   end Get_APIC_ID;

end SK.CPU_Registry;
