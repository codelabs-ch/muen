--
--  Copyright (C) 2015, 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015, 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package body SK.Tau0_Interface
with
   Refined_State => (State => New_Major)
is

   pragma Warnings
     (GNATprove, Off,
      "writing * is assumed to have no effects on other non-volatile objects",
      Reason => "All objects with address clause are mapped to external "
      & "interfaces. Non-overlap is checked during system build.");
   --D @Interface
   --D ID of major frame designated as active on next major frame switch. Tau0
   --D writes this value while the kernel executing on BSP reads it.
   New_Major : Skp.Scheduling.Major_Frame_Range'Base
   with
      Atomic,
      Async_Writers,
      Address => System'To_Address (Skp.Kernel.Tau0_Iface_Address);
   pragma Warnings
     (GNATprove, On,
      "writing * is assumed to have no effects on other non-volatile objects");

   -------------------------------------------------------------------------

   procedure Get_Major_Frame (ID : out Skp.Scheduling.Major_Frame_Range)
   with
      Refined_Global  => (Input    => New_Major,
                          Proof_In => CPU_Info.Is_BSP),
      Refined_Depends => (ID => New_Major)
   is
   begin
      --D @Interface
      --D Read current \texttt{New\_Major} value into ID out parameter.
      ID := New_Major;
      pragma Annotate
        (GNATprove, Intentional,
         "range check might fail",
         "Tau0 always provides valid major frame values.");
   end Get_Major_Frame;

end SK.Tau0_Interface;
