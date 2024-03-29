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

private with System;

private with Skp.Kernel;

with Skp.Subjects;

--D @Interface
--D This package provides facilities for managing subject MSR storage areas.
--D The MSR storage area specifies which MSRs must be saved/restored by the
--D hardware when entering/exiting a subject.
package SK.Subjects_MSR_Store
with
   Abstract_State => (State with External => (Async_Writers, Async_Readers)),
   Initializes    => State
is

   --  Clear MSR values in storage area of subject with given ID.
   procedure Clear_MSRs (Subject : Skp.Global_Subject_ID_Type)
   with
      Global  => (In_Out => State),
      Depends => (State =>+ Subject),
      Pre     => Skp.Subjects.Get_MSR_Count (Subject_ID => Subject) > 0;

private

   --  Size of one MSR entry in bytes.
   MSR_Entry_Size : constant := 16;

   --  MSR-store area entry type, see Intel SDM Vol. 3C, "24.7.2
   --  VM-Exit Controls for MSRs".
   type MSR_Entry_Type is record
      Index    : SK.Word32;
      Reserved : SK.Word32;
      Data     : SK.Word64;
   end record
   with
      Size => MSR_Entry_Size * 8;

   for MSR_Entry_Type use record
      Index    at 0  range  0 ..  31;
      Reserved at 0  range 32 ..  63;
      Data     at 0  range 64 .. 127;
   end record;

   --  Maximum number of MSR entries is currently 32, see Intel SDM Vol. 3C,
   --  "24.7.2 VM-Exit Controls for MSRs".
   type MSR_Entry_Range is range 1 .. 32;

   MSR_Storage_Table_Size : constant := MSR_Entry_Range'Last * MSR_Entry_Size;

   type MSR_Storage_Table is array (MSR_Entry_Range) of MSR_Entry_Type
   with
      Size => MSR_Storage_Table_Size * 8;

   type Padding_Type is array (MSR_Storage_Table_Size + 1 .. Page_Size) of Byte
   with
      Size => (Page_Size - MSR_Storage_Table_Size) * 8;

   --D @Interface
   --D A subject MSR storage page consist of the MSR data and is padded to a
   --D full 4K memory page. Explicit padding makes sure the entirety of the
   --D memory is covered and initialized.
   type MSR_Storage_Page is record
      --D @Interface
      --D MSR data as saved and restored by the CPU/hardware.
      MSRs    : MSR_Storage_Table;
      --D @Interface
      --D Padding to fill the memory page.
      Padding : Padding_Type;
   end record
   with
      Size => Page_Size * 8;

   type MSR_Storage_Array is array (Skp.Global_Subject_ID_Type)
     of MSR_Storage_Page
   with
      Independent_Components,
      Component_Size => Page_Size * 8,
      Alignment      => Page_Size,
      Object_Size    => (Skp.Global_Subject_ID_Type'Last + 1) * Page_Size * 8;

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   --D @Interface
   --D MSR save/restore storage area of each subject identified by ID. Hardware
   --D saves and restores MSRs on each VM-Entry and Exit as specified by Intel
   --D SDM Vol. 3C, "24.7.2 VM-Exit Controls for MSRs" and Intel SDM Vol. 3C,
   --D "24.8.2 VM-Entry Controls for MSRs" \cite{intelsdm}.
   MSR_Storage : MSR_Storage_Array
   with
      Volatile,
      Async_Writers,
      Async_Readers,
      Part_Of => State,
      Address => System'To_Address (Skp.Kernel.Subj_MSR_Store_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

end SK.Subjects_MSR_Store;
