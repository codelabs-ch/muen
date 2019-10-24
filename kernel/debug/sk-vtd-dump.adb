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
with SK.Strings;

package body SK.VTd.Dump
with
   SPARK_Mode => Off
is

   use SK.Strings;

   -------------------------------------------------------------------------

   procedure Print_Global_Status
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Status : Skp.IOMMU.Reg_Global_Status_Type)
   is
   begin
      KC.Put_Line
        (Item => "IOMMU " & Img_Nobase (Byte (IOMMU))
         & ": TES "   & Img_Nobase (Byte (Status.TES))
         & ", RTPS "  & Img_Nobase (Byte (Status.RTPS))
         & ", FLS "   & Img_Nobase (Byte (Status.FLS))
         & ", AFLS "  & Img_Nobase (Byte (Status.AFLS))
         & ", WBFS "  & Img_Nobase (Byte (Status.WBFS))
         & ", QIES "  & Img_Nobase (Byte (Status.QIES))
         & ", IRES "  & Img_Nobase (Byte (Status.IRES))
         & ", IRTPS " & Img_Nobase (Byte (Status.IRTPS))
         & ", CFIS "  & Img_Nobase (Byte (Status.CFIS)));
   end Print_Global_Status;

   -------------------------------------------------------------------------

   procedure Print_Message
     (IOMMU   : Skp.IOMMU.IOMMU_Device_Range;
      Message : String;
      Newline : Boolean := True)
   is
   begin
      KC.Put_String (Item => "IOMMU " & Img_Nobase (Byte (IOMMU)) & ": ");
      KC.Put_String (Item => Message);
      if Newline then
         KC.New_Line;
      end if;
   end Print_Message;

   -------------------------------------------------------------------------

   procedure Print_VTd_Fault
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Status : Skp.IOMMU.Reg_Fault_Status_Type;
      Fault  : Skp.IOMMU.Reg_Fault_Recording_Type)
   is
      use type Skp.IOMMU.Bit_Type;
      use type Skp.IOMMU.Bit_52_Type;

      --  Interrupt translation fault reason range, see Intel VT-d
      --  Specification, "5.1.4.1 Interrupt Remapping Fault Conditions".
      subtype IR_Fault_Range is SK.Byte range 16#20# .. 16#26#;
   begin
      Locks.Acquire;
      Print_Message (IOMMU   => IOMMU,
                     Message => "VT-d fault with FRI " & Img (Status.FRI),
                     Newline => False);

      if Fault.F = 1 then
         KC.Put_String (Item => " - Reason: " & Img (Fault.FR));

         if Fault.FR in IR_Fault_Range then

            --  FI field is undefined for interrupt-remapping fault condition
            --  of blocked Compatibility mode interrupt (fault reason 16#25#,
            --  see Intel VT-d Specification, "10.4.14 Fault Recording
            --  Registers [n]").

            if Fault.FR /= 16#25# then
               KC.Put_String (Item => ", IRT index: "
                              & Img (Word16 (Fault.FI / 2 ** 36)));
            end if;
         else
            KC.Put_String (Item => ", Address: "
                           & Img (Word64 (Fault.FI * 2 ** 12)));

            KC.Put_String (Item => ", Type: ");
            if Fault.T = 0 then
               KC.Put_String (Item => "Write");
            else
               KC.Put_String (Item => "Read");
            end if;
         end if;

         KC.Put_String
           (Item => ", Source: " & Img_Nobase (Byte (Fault.SID / 2 ** 8))
            & ":" & Img_Nobase (Byte ((Fault.SID / 2 ** 3) and 16#1f#))
            & "." & Img_Nobase (Byte (Fault.SID and 16#07#)));
      end if;

      KC.New_Line;

      Locks.Release;
   end Print_VTd_Fault;

end SK.VTd.Dump;
