--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body SK.Subjects_MSR_Store
with
   Refined_State => (State => MSR_Storage)
is

   --  MSR-store area entry type, see Intel SDM Vol. 3C, table 24-11.
   type MSR_Entry_Type is record
      Index    : SK.Word32;
      Reserved : SK.Word32;
      Data     : SK.Word64;
   end record
   with
      Size => 16 * 8;

   for MSR_Entry_Type use record
      Index    at 0  range  0 ..  31;
      Reserved at 0  range 32 ..  63;
      Data     at 0  range 64 .. 127;
   end record;

   --  Maximum number of MSR entries is currently 32, see Intel SDM Vol. 3C,
   --  section 24.7.8.
   type MSR_Entry_Range is range 1 .. 32;

   type MSR_Storage_Table is array (MSR_Entry_Range) of MSR_Entry_Type;

   pragma Warnings (GNAT, Off, "*padded by * bits");
   type MSR_Storage_Array is array (Skp.Global_Subject_ID_Type)
     of MSR_Storage_Table
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size;
   pragma Warnings (GNAT, On, "*padded by * bits");

   MSR_Storage : MSR_Storage_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Address => System'To_Address (Skp.Kernel.Subj_MSR_Store_Address);

   -------------------------------------------------------------------------

   procedure Clear_MSRs (ID : Skp.Global_Subject_ID_Type)
   with
      Refined_Global  => (In_Out => MSR_Storage),
      Refined_Depends => (MSR_Storage =>+ ID)
   is
   begin
      for I in MSR_Entry_Range loop
         MSR_Storage (ID)(I).Data := 0;
      end loop;
   end Clear_MSRs;

end SK.Subjects_MSR_Store;
