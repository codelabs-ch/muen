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

with System;

with Skp.IOMMU;

with SK.KC;
with SK.CPU;

package body SK.VTd
is

   --  Basic types

   type Bit_Type is mod 2 ** 1
     with
       Size => 1;

   type Bit_2_Type is mod 2 ** 2
     with
       Size => 2;

   type Bit_4_Type is mod 2 ** 4
     with
       Size => 4;

   type Bit_23_Type is mod 2 ** 23
     with
       Size => 23;

   type Bit_24_Type is mod 2 ** 24
     with
       Size => 24;

   type Bit_60_Type is mod 2 ** 60
     with
       Size => 60;

   type Bit_61_Type is mod 2 ** 61
     with
       Size => 61;

   --  Registers

   --  Version register (16#0000#)
   type Reg_Version_Type is record
      MIN      : Bit_4_Type;
      MAX      : Bit_4_Type;
      Reserved : Bit_24_Type;
   end record
     with
       Size => 32;

   for Reg_Version_Type use record
      MIN      at 0 range 0 ..  3;
      MAX      at 0 range 4 ..  7;
      Reserved at 0 range 8 .. 31;
   end record;

   --  Global Command Register (16#0018#)
   type Reg_Global_Command_Type is record
      Reserved : Bit_23_Type;
      CFI      : Bit_Type;
      SIRTP    : Bit_Type;
      IRE      : Bit_Type;
      QIE      : Bit_Type;
      WBF      : Bit_Type;
      EAFL     : Bit_Type;
      SFL      : Bit_Type;
      SRTP     : Bit_Type;
      TE       : Bit_Type;
   end record
     with
       Size => 32;

   for Reg_Global_Command_Type use record
      Reserved at 0 range 0  .. 22;
      CFI      at 0 range 23 .. 23;
      SIRTP    at 0 range 24 .. 24;
      IRE      at 0 range 25 .. 25;
      QIE      at 0 range 26 .. 26;
      WBF      at 0 range 27 .. 27;
      EAFL     at 0 range 28 .. 28;
      SFL      at 0 range 29 .. 29;
      SRTP     at 0 range 30 .. 30;
      TE       at 0 range 31 .. 31;
   end record;

   --  Global Status Register (16#001c#)
   type Reg_Global_Status_Type is record
      Reserved : Bit_23_Type;
      CFIS     : Bit_Type;
      IRTPS    : Bit_Type;
      IRES     : Bit_Type;
      QIES     : Bit_Type;
      WBFS     : Bit_Type;
      AFLS     : Bit_Type;
      FLS      : Bit_Type;
      RTPS     : Bit_Type;
      TES      : Bit_Type;
   end record
     with
       Size => 32;

   for Reg_Global_Status_Type use record
      Reserved at 0 range 0  .. 22;
      CFIS     at 0 range 23 .. 23;
      IRTPS    at 0 range 24 .. 24;
      IRES     at 0 range 25 .. 25;
      QIES     at 0 range 26 .. 26;
      WBFS     at 0 range 27 .. 27;
      AFLS     at 0 range 28 .. 28;
      FLS      at 0 range 29 .. 29;
      RTPS     at 0 range 30 .. 30;
      TES      at 0 range 31 .. 31;
   end record;

   --  Context Command Register (16#0028#)
   type Reg_Context_Command_Type is record
      Unused : Bit_61_Type;
      CIRG   : Bit_2_Type;
      ICC    : Bit_Type;
   end record
     with
       Size => 64;

   for Reg_Context_Command_Type use record
      Unused at 0 range  0 .. 60;
      CIRG   at 0 range 61 .. 62;
      ICC    at 0 range 63 .. 63;
   end record;

   --  IOTLB Invalidate Register (dynamic)

   type Reg_IOTLB_Invalidate is record
      Unused   : Bit_60_Type;
      IIRG     : Bit_2_Type;
      Reserved : Bit_Type;
      IVT      : Bit_Type;
   end record;

   for Reg_IOTLB_Invalidate use record
      Unused   at 0 range 0  .. 59;
      IIRG     at 0 range 60 .. 61;
      Reserved at 0 range 62 .. 62;
      IVT      at 0 range 63 .. 63;
   end record;

   --  Specified by Skp.IOMMU package (TODO)

   IOMMU_Base_Address : constant := 16#001f_d000#;
   IOMMU_Count        : constant := 2;

   --  IOTLB Invalidate Register offset, must be calculated using Extended
   --  Capability Register IRO field (TODO)

   IOTLB_Offset : constant := 16#108#;

   type IOMMU_Range is range 1 .. IOMMU_Count;

   type IOMMU_Type is record
      Version            : Reg_Version_Type;
      Reserved           : SK.Word32;
      Capability         : SK.Word64;
      Ext_Capability     : SK.Word64;
      Global_Command     : Reg_Global_Command_Type;
      Global_Status      : Reg_Global_Status_Type;
      Root_Table_Address : SK.Word64;
      Context_Command    : Reg_Context_Command_Type;
      IOTLB_Invalidate   : Reg_IOTLB_Invalidate;
   end record;

   pragma Warnings (Off, "*-bit gap before component *");
   for IOMMU_Type use record
      Version            at  0 range 0 .. 31;
      Reserved           at  4 range 0 .. 31;
      Capability         at  8 range 0 .. 63;
      Ext_Capability     at 16 range 0 .. 63;
      Global_Command     at 24 range 0 .. 31;
      Global_Status      at 28 range 0 .. 31;
      Root_Table_Address at 32 range 0 .. 63;
      Context_Command    at 40 range 0 .. 63;
      IOTLB_Invalidate   at IOTLB_Offset range 0 .. 63;
   end record;
   pragma Warnings (On, "*-bit gap before component *");

   pragma $Build_Warnings (Off, "*padded by * bits");
   type IOMMU_Array is array (IOMMU_Range) of IOMMU_Type
     with
       Component_Size => Page_Size * 8,
       Alignment      => Page_Size;
   pragma $Build_Warnings (On, "*padded by * bits");

   IOMMUs : IOMMU_Array
     with
       Volatile,
       Address => System'To_Address (IOMMU_Base_Address);

   -------------------------------------------------------------------------

   procedure Initialize
   is
      Version : Reg_Version_Type;
   begin
      for I in IOMMU_Range loop
         Version := IOMMUs (I).Version;

         --  Basic sanity check, TODO: check IOMMU capabilities.

         if Version.MAX /= 1 or else Version.MIN /= 0 then
            pragma Debug (KC.Put_String
                          (Item => "Unsupported IOMMU version "));
            pragma Debug (KC.Put_Byte (Item => SK.Byte (Version.MAX)));
            pragma Debug (KC.Put_Byte (Item => SK.Byte (Version.MIN)));
            pragma Debug (KC.New_Line);

            CPU.Panic;
         end if;

         Set_Root_Table_Address :
         declare
            Global_Status : Reg_Global_Status_Type;
         begin
            IOMMUs (I).Root_Table_Address  := Skp.IOMMU.Root_Table_Address;
            IOMMUs (I).Global_Command.SRTP := 1;

            Global_Status := IOMMUs (I).Global_Status;
            while Global_Status.RTPS = 0 loop
               Global_Status := IOMMUs (I).Global_Status;
            end loop;
         end Set_Root_Table_Address;

         Invalidate_Context_Cache :
         declare
            Context_Command : Reg_Context_Command_Type;
         begin
            IOMMUs (I).Context_Command.CIRG := 1;
            IOMMUs (I).Context_Command.ICC  := 1;

            Context_Command := IOMMUs (I).Context_Command;
            while Context_Command.ICC = 1 loop
               Context_Command := IOMMUs (I).Context_Command;
            end loop;
         end Invalidate_Context_Cache;

         IOTLB_Flush :
         declare
            IOTLB_Invalidate : Reg_IOTLB_Invalidate;
         begin
            IOMMUs (I).IOTLB_Invalidate.IIRG := 1;
            IOMMUs (I).IOTLB_Invalidate.IVT  := 1;

            IOTLB_Invalidate := IOMMUs (I).IOTLB_Invalidate;
            while IOTLB_Invalidate.IVT = 1 loop
               IOTLB_Invalidate := IOMMUs (I).IOTLB_Invalidate;
            end loop;
         end IOTLB_Flush;
      end loop;
   end Initialize;

end SK.VTd;
