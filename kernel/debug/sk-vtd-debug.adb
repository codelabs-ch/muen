--
--  Copyright (C) 2021  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2021  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.VTd.Dump;

package body SK.VTd.Debug
is

   use Skp.IOMMU;

   -------------------------------------------------------------------------

   procedure Process_Fault
   is
      Status : Reg_Fault_Status_Type;
   begin
      for I in IOMMU_Device_Range loop
         Status := Read_Fault_Status (Index => (I));

         if Status.PPF = 1 then
            declare
               Fault_Record : Reg_Fault_Recording_Type;
            begin
               Fault_Record := Read_Fault_Recording (Index => I);
               Dump.Print_VTd_Fault
                 (IOMMU  => I,
                  Status => Status,
                  Fault  => Fault_Record);
            end;

            VTd.Clear_Fault_Record (IOMMU => I);
         end if;
      end loop;
   end Process_Fault;

end SK.VTd.Debug;
