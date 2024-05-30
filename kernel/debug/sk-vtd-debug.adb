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

with SK.Constants;
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
         Status := Read_Fault_Status (Index => I);

         --  Intel VT-d Specification, "7.2.1 Primary Fault Logging".

         if Status.PPF = 1 then
            declare
               Start_Idx    : constant Fault_Recording_Index
                 := Fault_Recording_Index (Status.FRI);
               Fault_Record : Reg_Fault_Recording_Type;
            begin
               for J in Fault_Recording_Index range
                 Start_Idx .. Fault_Recording_Index'Last
               loop
                  Fault_Record := Read_Fault_Recording
                      (Index => I,
                       FRI   => J);
                  exit when Fault_Record.F = 0;
                  Dump.Print_VTd_Fault
                    (IOMMU  => I,
                     FRI    => J,
                     Fault  => Fault_Record);
                  VTd.Clear_Fault_Record
                    (IOMMU => I,
                     FRI   => J);
               end loop;
            end;
         end if;
      end loop;
   end Process_Fault;

   -------------------------------------------------------------------------

   procedure Setup_Fault_Interrupt (IOMMU : IOMMU_Device_Range)
   is
      Fault_Event_Addr : Reg_Fault_Event_Address_Type;
      Fault_Event_Data : Reg_Fault_Event_Data_Type;
   begin
      Fault_Event_Addr := Read_Fault_Event_Address (Index => IOMMU);

      Fault_Event_Addr.APIC_ID := 0;
      Write_Fault_Event_Address
        (Index => IOMMU,
         Value => Fault_Event_Addr);

      Fault_Event_Data.EIMD := 0;
      Fault_Event_Data.IMD  := SK.Word16 (SK.Constants.VTd_Fault_Vector);
      Write_Fault_Event_Data
        (Index => IOMMU,
         Value => Fault_Event_Data);
   end Setup_Fault_Interrupt;

end SK.VTd.Debug;
