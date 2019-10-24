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

with Skp.IOMMU;

package SK.VTd.Dump
with
   SPARK_Mode => Off
is

   --  Print VT-d fault information for given IOMMU.
   procedure Print_VTd_Fault
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Status : Skp.IOMMU.Reg_Fault_Status_Type;
      Fault  : Skp.IOMMU.Reg_Fault_Recording_Type);

   --  Print general remapping hardware status for given IOMMU.
   procedure Print_Global_Status
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Status : Skp.IOMMU.Reg_Global_Status_Type);

   --  Print message for given IOMMU.
   procedure Print_Message
     (IOMMU   : Skp.IOMMU.IOMMU_Device_Range;
      Message : String;
      Newline : Boolean := True);

end SK.VTd.Dump;
