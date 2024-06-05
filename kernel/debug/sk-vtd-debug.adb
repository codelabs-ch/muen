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
with SK.Strings;

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
            if Status.FRI > Byte (Fault_Recording_Index'Last) then
               SK.VTd.Dump.Print_Message
                 (IOMMU   => I,
                  Message => "Invalid FRI " & Strings.Img (Status.FRI));
            else
               declare
                  One_FR : constant Boolean               :=
                    Fault_Recording_Index'Last = 0;
                  Inc    : constant Fault_Recording_Index :=
                    (if One_FR then 0 else 1);
                  FRI    : Fault_Recording_Index;
                  FR     : Reg_Fault_Recording_Type;
               begin
                  FRI := Fault_Recording_Index (Status.FRI);

                  loop
                     FR := Read_Fault_Recording
                         (Index => I,
                          FRI   => FRI);

                     --  FRI might wrap around until FR.F = 0.

                     exit when FR.F = 0;
                     Dump.Print_VTd_Fault
                       (IOMMU => I,
                        FRI   => FRI,
                        Fault => FR);
                     VTd.Clear_Fault_Record
                       (IOMMU => I,
                        FRI   => FRI);
                     exit when One_FR;
                     FRI := FRI + Inc;
                  end loop;
               end;
            end if;
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
