--
--  Copyright (C) 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with SK.KC;
with SK.Locks;

package body SK.VTd.Dump
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   procedure Print_Global_Status
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Status : Types.Reg_Global_Status_Type)
   is
   begin
      Locks.Acquire;

      KC.Put_String (Item => "IOMMU ");
      KC.Put_Byte   (Item => SK.Byte (IOMMU));
      KC.Put_String (Item => ": TES ");
      KC.Put_Byte   (Item => SK.Byte (Status.TES));
      KC.Put_String (Item => ", RTPS ");
      KC.Put_Byte   (Item => SK.Byte (Status.RTPS));
      KC.Put_String (Item => ", FLS ");
      KC.Put_Byte   (Item => SK.Byte (Status.FLS));
      KC.Put_String (Item => ", AFLS ");
      KC.Put_Byte   (Item => SK.Byte (Status.AFLS));
      KC.Put_String (Item => ", WBFS ");
      KC.Put_Byte   (Item => SK.Byte (Status.WBFS));
      KC.Put_String (Item => ", QIES ");
      KC.Put_Byte   (Item => SK.Byte (Status.QIES));
      KC.Put_String (Item => ", IRES ");
      KC.Put_Byte   (Item => SK.Byte (Status.IRES));
      KC.Put_String (Item => ", IRTPS ");
      KC.Put_Byte   (Item => SK.Byte (Status.IRTPS));
      KC.Put_String (Item => ", CFIS ");
      KC.Put_Byte   (Item => SK.Byte (Status.CFIS));
      KC.New_Line;

      Locks.Release;
   end Print_Global_Status;

   -------------------------------------------------------------------------

   procedure Print_VTd_Fault
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Status : Types.Reg_Fault_Status_Type;
      Fault  : Types.Reg_Fault_Recording_Type)
   is
      use type SK.VTd.Types.Bit_Type;
      use type SK.VTd.Types.Bit_52_Type;
   begin
      Locks.Acquire;

      KC.Put_String (Item => "IOMMU ");
      KC.Put_Byte   (Item => SK.Byte (IOMMU));
      KC.Put_String (Item => ": VT-d fault with FRI ");
      KC.Put_Byte   (Item => Status.FRI);

      if Fault.F = 1 then
         KC.Put_String (Item => " - Reason: ");
         KC.Put_Byte   (Item => Fault.FR);
         KC.Put_String (Item => ", Info: ");
         KC.Put_Word64 (Item => SK.Word64 (Fault.FI * 2 ** 12));

         KC.Put_String (Item => ", Type: ");
         if Fault.T = 0 then
            KC.Put_String (Item => "Write");
         else
            KC.Put_String (Item => "Read");
         end if;

         KC.Put_String (Item => ", Source: ");
         KC.Put_Byte   (Item => SK.Byte (Fault.SID / 2 ** 8));
         KC.Put_String (Item => ":");
         KC.Put_Byte   (Item => SK.Byte ((Fault.SID / 2 ** 3) and 16#1f#));
         KC.Put_String (Item => ".");
         KC.Put_Byte   (Item => SK.Byte (Fault.SID and 16#07#));
      end if;

      KC.New_Line;

      Locks.Release;
   end Print_VTd_Fault;

end SK.VTd.Dump;
