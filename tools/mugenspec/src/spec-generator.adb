--
--  Copyright (C) 2014, 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014, 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Spec.Skp;
with Spec.Skp_Events;
with Spec.Skp_Scheduling;
with Spec.Skp_Hardware;
with Spec.Skp_Interrupts;
with Spec.Skp_IOMMU;
with Spec.Skp_Subjects;
with Spec.Skp_Kernel_Policy_H;

package body Spec.Generator
is

   -------------------------------------------------------------------------

   procedure Write
     (Output_Dir : String;
      Policy     : Muxml.XML_Data_Type)
   is
   begin
      Skp.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Skp_Scheduling.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Skp_Interrupts.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Spec.Skp_Kernel_Policy_H.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Skp_Subjects.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Skp_Hardware.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Skp_Events.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
      Skp_IOMMU.Write
        (Output_Dir => Output_Dir,
         Policy     => Policy);
   end Write;

end Spec.Generator;
