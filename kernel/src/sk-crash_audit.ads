--
--  Copyright (C) 2017  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2017  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Interfaces;

with X86_64;

with Skp.Interrupts;

with SK.CPU_Global;

package SK.Crash_Audit
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers)),
   Initializes    => State
is

   --  Initialize crash audit facility.
   procedure Init
   with
      Pre => CPU_Global.Is_BSP;

   --  Crash audit entry.
   type Entry_Type (<>) is private;

   Null_Entry : constant Entry_Type;

   --  Allocate new crash audit entry. If this operation fails because no crash
   --  audit entries are available, the calling CPU will be halted.
   procedure Allocate (Audit : out Entry_Type)
   with
      Global => (Input  => CPU_Global.CPU_ID,
                 In_Out => (State, X86_64.State)),
      Post   => Audit /= Null_Entry;

   --  Set ISR context information for given entry and mark it as valid.
   procedure Set_Isr_Context
     (Audit       : Entry_Type;
      Isr_Context : Isr_Context_Type)
   with
      Global => (In_Out => State),
      Pre    => Audit /= Null_Entry;

   --  Finalize crash audit by performing a warm system restart. By setting the
   --  generation counter to boot counter + 1, the crash dump will be active on
   --  the next reboot.
   --
   --  The procedure spins 100 ms before hitting reset to give other cores
   --  time to write their dumps.
   procedure Finalize (Audit : Entry_Type)
   with
      Global => (In_Out => (State, X86_64.State)),
      Pre    => Audit /= Null_Entry,
      No_Return;

private

   use type Interfaces.Unsigned_64;

   --  xxd -l 8 -p /dev/random
   Crash_Magic : constant := 16#d5ab_c59c_4a9a_2a93#;

   subtype Version_Str_Range is Positive range 1 .. 64;

   type Version_String_Type is new String (Version_Str_Range);

   Null_Version_String : constant Version_String_Type := (others => ASCII.NUL);

   Max_Dumps : constant := 3;

   type Dumpdata_Length is range 0 .. Max_Dumps
   with
      Size => 8;

   subtype Dumpdata_Index is Dumpdata_Length range 1 .. Dumpdata_Length'Last;

   Header_Type_Size : constant := 8 + 64 + (3 * 8) + 2 + 4 + 2;

   type Header_Type is record
      Version_Magic  : Interfaces.Unsigned_64;
      Version_String : Version_String_Type;
      Generation     : Interfaces.Unsigned_64;
      Boot_Count     : Interfaces.Unsigned_64;
      Crash_Count    : Interfaces.Unsigned_64;
      Max_Dump_Count : Dumpdata_Index;
      Dump_Count     : Dumpdata_Length;
      Crc32          : Interfaces.Unsigned_32;
      Padding        : Interfaces.Unsigned_16;
   end record
   with
      Size => Header_Type_Size * 8;

   for Header_Type use record
      Version_Magic  at   0 range 0 .. 63;
      Version_String at   8 range 0 .. Version_Str_Range'Last * 8 - 1;
      Generation     at  72 range 0 .. 63;
      Boot_Count     at  80 range 0 .. 63;
      Crash_Count    at  88 range 0 .. 63;
      Max_Dump_Count at  96 range 0 .. 7;
      Dump_Count     at  97 range 0 .. 7;
      Crc32          at  98 range 0 .. 31;
      Padding        at 102 range 0 .. 15;
   end record;

   Null_Header : constant Header_Type
     := (Version_Magic  => Crash_Magic,
         Version_String => Null_Version_String,
         Generation     => 0,
         Boot_Count     => 1,
         Crash_Count    => 0,
         Max_Dump_Count => Max_Dumps,
         Dump_Count     => 0,
         Crc32          => 0,
         Padding        => 0);

   ---------------------
   --  Crash Reasons  --
   ---------------------

   type Reason_Type is new Interfaces.Unsigned_64;

   Reason_Undefined : constant Reason_Type := 16#0000#;

   --  Exceptions.

   Hardware_Exception : constant Reason_Type := 16#1000#;

   type Valid_Flags_Type is record
      Isr_Context : Boolean;
   end record
   with
      Size => 8;

   for Valid_Flags_Type use record
      Isr_Context at 0 range 0 .. 0;
   end record;

   Null_Validity_Flags : constant Valid_Flags_Type
     := (Isr_Context => False);

   type Dumpdata_Type is record
      TSC_Value      : Interfaces.Unsigned_64;
      Reason         : Reason_Type;
      APIC_ID        : Skp.Interrupts.APIC_ID_Range;
      Field_Validity : Valid_Flags_Type;
      Isr_Context    : Isr_Context_Type;
   end record
   with
      Size => (8 + 8 + 1 + 1 + Isr_Context_Type_Size) * 8;

   for Dumpdata_Type use record
      TSC_Value      at  0 range 0 .. 63;
      Reason         at  8 range 0 .. 63;
      APIC_ID        at 16 range 0 .. 7;
      Field_Validity at 17 range 0 .. 7;
      Isr_Context    at 18 range 0 .. Isr_Context_Type_Size * 8 - 1;
   end record;

   Null_Dumpdata : constant Dumpdata_Type
     := (TSC_Value      => 0,
         APIC_ID        => 0,
         Reason         => Reason_Undefined,
         Field_Validity => Null_Validity_Flags,
         Isr_Context    => Null_Isr_Context);

   type Dumpdata_Array is array (Dumpdata_Index) of Dumpdata_Type;

   Null_Dumpdata_Array : constant Dumpdata_Array
     := (others => Null_Dumpdata);

   type Dump_Type is record
      Header : Header_Type;
      Data   : Dumpdata_Array;
   end record;

   Null_Dump : constant Dump_Type
     := (Header => Null_Header,
         Data   => Null_Dumpdata_Array);

   type Entry_Type is record
      Slot : Dumpdata_Length;
   end record;

   Null_Entry : constant Entry_Type
     := (Slot => Dumpdata_Length'First);

end SK.Crash_Audit;
