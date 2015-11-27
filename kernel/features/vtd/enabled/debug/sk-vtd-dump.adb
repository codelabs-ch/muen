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

with SK.KC;
with SK.Locks;

package body SK.VTd.Dump
with
   SPARK_Mode => Off
is

   -------------------------------------------------------------------------

   procedure Print_Global_Status
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Status : Skp.IOMMU.Reg_Global_Status_Type)
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
      Status : Skp.IOMMU.Reg_Fault_Status_Type;
      Fault  : Skp.IOMMU.Reg_Fault_Recording_Type)
   is
      use type Skp.IOMMU.Bit_Type;
      use type Skp.IOMMU.Bit_52_Type;

      --  Interrupt translation fault reason range, see Intel VT-d spec,
      --  section 7.1.
      subtype IR_Fault_Range is SK.Byte range 16#20# .. 16#26#;
   begin
      Locks.Acquire;

      KC.Put_String (Item => "IOMMU ");
      KC.Put_Byte   (Item => SK.Byte (IOMMU));
      KC.Put_String (Item => ": VT-d fault with FRI ");
      KC.Put_Byte   (Item => Status.FRI);

      if Fault.F = 1 then
         KC.Put_String (Item => " - Reason: ");
         KC.Put_Byte   (Item => Fault.FR);

         if Fault.FR in IR_Fault_Range then

            --  FI field is undefined for interrupt-remapping fault condition
            --  of blocked Compatibility mode interrupt (fault reason 16#25#,
            --  see Intel VT-d spec, section 10.4.14).

            if Fault.FR /= 16#25# then
               KC.Put_String (Item => ", IRT index: ");
               KC.Put_Word16 (Item => SK.Word16 (Fault.FI / 2 ** 36));
            end if;
         else
            KC.Put_String (Item => ", Address: ");
            KC.Put_Word64 (Item => SK.Word64 (Fault.FI * 2 ** 12));

            KC.Put_String (Item => ", Type: ");
            if Fault.T = 0 then
               KC.Put_String (Item => "Write");
            else
               KC.Put_String (Item => "Read");
            end if;
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
