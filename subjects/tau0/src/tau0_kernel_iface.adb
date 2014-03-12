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

with System;

with Skp.Kernel;
with Skp.Scheduling;

use type Skp.Scheduling.Major_Frame_Range;

package body Tau0_Kernel_Iface
--# own
--#    State is out Active_Major, Cur_Major;
is

   Cur_Major : Skp.Scheduling.Major_Frame_Range
     := Skp.Scheduling.Major_Frame_Range'First;

   Active_Major : Skp.Scheduling.Major_Frame_Range;
   for Active_Major'Address use System'To_Address
     (Skp.Kernel.Tau0_Iface_Address);
   pragma Atomic (Active_Major);

   -------------------------------------------------------------------------

   procedure Switch_Major_Frame
   --# global
   --#       out Active_Major;
   --#    in out Cur_Major;
   --# derives
   --#    Cur_Major    from * &
   --#    Active_Major from Cur_Major;
   is
   begin

      --  A warning is issued for a scheduling plan with only one major frame.

      pragma Warnings (Off);
      if Cur_Major = Skp.Scheduling.Major_Frame_Range'Last then
         Cur_Major := Skp.Scheduling.Major_Frame_Range'First;
      else
         Cur_Major := Cur_Major + 1;
      end if;
      pragma Warnings (On);

      Active_Major := Cur_Major;
   end Switch_Major_Frame;

end Tau0_Kernel_Iface;
