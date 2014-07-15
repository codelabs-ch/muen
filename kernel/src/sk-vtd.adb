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
pragma $Release_Warnings (Off, "unit * is not referenced");
with SK.Apic;
with SK.Constants;
pragma $Release_Warnings (On, "unit * is not referenced");

package body SK.VTd
with
  Refined_State => (State => IOMMUs)
is

   --  Basic types

   type Bit_Type is mod 2 ** 1
     with
       Size => 1;

   type Bit_Array is array (Positive range <>) of Bit_Type
     with
       Pack;

   type Bit_2_Type is mod 2 ** 2
     with
       Size => 2;

   type Bit_3_Type is mod 2 ** 3
     with
       Size => 3;

   type Bit_4_Type is mod 2 ** 4
     with
       Size => 4;

   type Bit_52_Type is mod 2 ** 52
     with
       Size => 52;

   --  Registers

   --  Version register
   type Reg_Version_Type is record
      MIN      : Bit_4_Type;
      MAX      : Bit_4_Type;
      Reserved : Bit_Array (1 .. 24);
   end record
     with
       Size => 32;

   for Reg_Version_Type use record
      MIN      at 0 range 0 ..  3;
      MAX      at 0 range 4 ..  7;
      Reserved at 0 range 8 .. 31;
   end record;

   --  Global Command Register
   type Reg_Global_Command_Type is record
      Reserved : Bit_Array (1 .. 23);
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

   --  Global Status Register
   type Reg_Global_Status_Type is record
      Reserved : Bit_Array (1 .. 23);
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

   --  Context Command Register
   type Reg_Context_Command_Type is record
      Unused : Bit_Array (1 .. 61);
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

   --  Fault Status Register
   type Reg_Fault_Status_Type is record
      PFO      : Bit_Type;
      PPF      : Bit_Type;
      AFO      : Bit_Type;
      APF      : Bit_Type;
      IQE      : Bit_Type;
      ICE      : Bit_Type;
      ITE      : Bit_Type;
      PRO      : Bit_Type;
      FRI      : SK.Byte;
      Reserved : SK.Word16;
   end record
     with
       Size => 32;

   for Reg_Fault_Status_Type use record
      PFO      at 0 range 0  .. 0;
      PPF      at 0 range 1  .. 1;
      AFO      at 0 range 2  .. 2;
      APF      at 0 range 3  .. 3;
      IQE      at 0 range 4  .. 4;
      ICE      at 0 range 5  .. 5;
      ITE      at 0 range 6  .. 6;
      PRO      at 0 range 7  .. 7;
      FRI      at 0 range 8  .. 15;
      Reserved at 0 range 16 .. 31;
   end record;

   type Reg_Fault_Event_Control_Type is record
      Reserved : Bit_Array (1 .. 30);
      IP       : Bit_Type;
      IM       : Bit_Type;
   end record
     with
       Size => 32;

   for Reg_Fault_Event_Control_Type use record
      Reserved at 0 range 0  .. 29;
      IP       at 0 range 30 .. 30;
      IM       at 0 range 31 .. 31;
   end record;

   type Reg_Fault_Event_Data_Type is record
      IMD  : SK.Word16;
      EIMD : SK.Word16;
   end record
     with
       Size => 32;

   for Reg_Fault_Event_Data_Type use record
      IMD  at 0 range 0  .. 15;
      EIMD at 0 range 16 .. 31;
   end record;

   type Reg_Fault_Event_Address_Type is record
      Reserved_1       : Bit_Array (1 .. 2);
      Destination_Mode : Bit_Type;
      Redirection_Hint : Bit_Type;
      Reserved_2       : SK.Byte;
      APIC_ID          : SK.Byte;
      FEEh             : Bit_Array (1 .. 12);
   end record
     with
       Size => 32;

   for Reg_Fault_Event_Address_Type use record
      Reserved_1       at 0 range 0  .. 1;
      Destination_Mode at 0 range 2  .. 2;
      Redirection_Hint at 0 range 3  .. 3;
      Reserved_2       at 0 range 4  .. 11;
      APIC_ID          at 0 range 12 .. 19;
      FEEh             at 0 range 20 .. 31;
   end record;

   --  IOTLB Invalidate Register (dynamic)

   type Reg_IOTLB_Invalidate is record
      Unused   : Bit_Array (1 .. 60);
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

   --  Fault Recording Register (dynamic)
   type Reg_Fault_Recording_Type is record
      Reserved_1 : Bit_Array (1 .. 12);
      FI         : Bit_52_Type;
      SID        : SK.Word16;
      Reserved_2 : Bit_Array (1 .. 13);
      PRIV       : Bit_Type;
      EXE        : Bit_Type;
      PP         : Bit_Type;
      FR         : SK.Byte;
      PV         : Bit_Array (1 .. 20);
      AType      : Bit_2_Type;
      T          : Bit_Type;
      F          : Bit_Type;
   end record
     with
       Size => 128;

   for Reg_Fault_Recording_Type use record
      Reserved_1 at 0 range 0   .. 11;
      FI         at 0 range 12  .. 63;
      SID        at 0 range 64  .. 79;
      Reserved_2 at 0 range 80  .. 92;
      PRIV       at 0 range 93  .. 93;
      EXE        at 0 range 94  .. 94;
      PP         at 0 range 95  .. 95;
      FR         at 0 range 96  .. 103;
      PV         at 0 range 104 .. 123;
      AType      at 0 range 124 .. 125;
      T          at 0 range 126 .. 126;
      F          at 0 range 127 .. 127;
   end record;

   --  IOTLB Invalidate Register offset, must be calculated using Extended
   --  Capability Register IRO field (TODO)

   IOTLB_Offset : constant := 16#108#;

   --  Fault-recording register offset, must be calculcated using Capability
   --  Register FRO field (TODO).
   FR_Offset : constant := 16#20# * 16;

   --  NOTE: The Intel VT-d spec section 10.2 mentions that software is
   --  expected to access registers as a whole. To avoid side-effects from
   --  partial/wider reads always read the entire record field/register, modify
   --  the appropriate values and write back the new data (see also GNATtracker
   --  ticket N307-023).

   type IOMMU_Type is record
      Version             : Reg_Version_Type;
      Reserved_1          : SK.Word32;
      Capability          : SK.Word64;
      Ext_Capability      : SK.Word64;
      Global_Command      : Reg_Global_Command_Type;
      Global_Status       : Reg_Global_Status_Type;
      Root_Table_Address  : SK.Word64;
      Context_Command     : Reg_Context_Command_Type;
      Reserved_2          : SK.Word32;
      Fault_Status        : Reg_Fault_Status_Type;
      Fault_Event_Control : Reg_Fault_Event_Control_Type;
      Fault_Event_Data    : Reg_Fault_Event_Data_Type;
      Fault_Event_Address : Reg_Fault_Event_Address_Type;
      IOTLB_Invalidate    : Reg_IOTLB_Invalidate;
      Fault_Recording     : Reg_Fault_Recording_Type;
   end record
     with
       Alignment => Page_Size;

   pragma Warnings (Off, "*-bit gap before component *");
   for IOMMU_Type use record
      Version             at 16#00# range 0 .. 31;
      Reserved_1          at 16#04# range 0 .. 31;
      Capability          at 16#08# range 0 .. 63;
      Ext_Capability      at 16#10# range 0 .. 63;
      Global_Command      at 16#18# range 0 .. 31;
      Global_Status       at 16#1c# range 0 .. 31;
      Root_Table_Address  at 16#20# range 0 .. 63;
      Context_Command     at 16#28# range 0 .. 63;
      Reserved_2          at 16#30# range 0 .. 31;
      Fault_Status        at 16#34# range 0 .. 31;
      Fault_Event_Control at 16#38# range 0 .. 31;
      Fault_Event_Data    at 16#3c# range 0 .. 31;
      Fault_Event_Address at 16#40# range 0 .. 31;
      IOTLB_Invalidate    at IOTLB_Offset range 0 .. 63;
      Fault_Recording     at FR_Offset    range 0 .. 127;
   end record;
   pragma Warnings (On, "*-bit gap before component *");

   pragma $Build_Warnings (Off, "*padded by * bits");
   type IOMMU_Array is array (Skp.IOMMU.IOMMU_Device_Range) of IOMMU_Type
     with
       Component_Size => Page_Size * 8;
   pragma $Build_Warnings (On, "*padded by * bits");

   IOMMUs : IOMMU_Array
     with
       Volatile,
       Async_Writers,
       Async_Readers,
       Effective_Writes,
       Address => System'To_Address (Skp.IOMMU.Base_Address);

   -------------------------------------------------------------------------

   --  Clears the Fault recording register and the Primary Fault Overflow flag
   --  of the specified IOMMU.
   procedure Clear_Fault_Record (IOMMU : Skp.IOMMU.IOMMU_Device_Range)
   with
      SPARK_Mode => Off -- XXX Workaround for [N425-012]
   is
      Fault_Recording : Reg_Fault_Recording_Type;
      Fault_Status    : Reg_Fault_Status_Type;
   begin
      Fault_Recording   := IOMMUs (IOMMU).Fault_Recording;
      Fault_Recording.F := 1;
      IOMMUs (IOMMU).Fault_Recording := Fault_Recording;

      Fault_Status     := IOMMUs (IOMMU).Fault_Status;
      Fault_Status.PFO := 1;
      IOMMUs (IOMMU).Fault_Status := Fault_Status;
   end Clear_Fault_Record;

   -------------------------------------------------------------------------

   --  Sets the fault event interrupt mask of the specified IOMMU to the given
   --  state. Setting the mask to true prohibits hardware from generating an
   --  interrupt when a fault event occurs.
   procedure Set_Fault_Event_Mask
     (IOMMU  : Skp.IOMMU.IOMMU_Device_Range;
      Enable : Boolean)
   with
      SPARK_Mode => Off -- XXX Workaround for [N425-012]
   is
      Fault_Event_Control : Reg_Fault_Event_Control_Type;
   begin
      Fault_Event_Control    := IOMMUs (IOMMU).Fault_Event_Control;
      Fault_Event_Control.IM := (if Enable then 1 else 0);
      IOMMUs (IOMMU).Fault_Event_Control := Fault_Event_Control;
   end Set_Fault_Event_Mask;

   -------------------------------------------------------------------------

   --  Sets the fault interrupt vector and destination APIC ID of the specified
   --  IOMMU to the given values.
   pragma $Release_Warnings (Off, "procedure * is not referenced");
   procedure Setup_Fault_Interrupt
     (IOMMU   : Skp.IOMMU.IOMMU_Device_Range;
      Vector  : SK.Byte;
      APIC_ID : SK.Byte)
   with
      SPARK_Mode => Off -- XXX Workaround for [N425-012]
   is
      Fault_Event_Addr : Reg_Fault_Event_Address_Type;
      Fault_Event_Data : Reg_Fault_Event_Data_Type;
   begin
      Fault_Event_Addr         := IOMMUs (IOMMU).Fault_Event_Address;
      Fault_Event_Addr.APIC_ID := APIC_ID;
      IOMMUs (IOMMU).Fault_Event_Address := Fault_Event_Addr;

      Fault_Event_Data.EIMD := 0;
      Fault_Event_Data.IMD  := SK.Word16 (Vector);
      IOMMUs (IOMMU).Fault_Event_Data := Fault_Event_Data;
   end Setup_Fault_Interrupt;
   pragma $Release_Warnings (On, "procedure * is not referenced");

   -------------------------------------------------------------------------

   procedure Process_Fault
   with
      SPARK_Mode     => Off, -- XXX Workaround for [N425-012]
      Refined_Global => (In_Out => IOMMUs)
   is
      Status : Reg_Fault_Status_Type;
   begin

      --  Systems without an IOMMU have a null range.
      --  NOTE: Due to ticket N704-009 the 'Reason' attribute of pragma
      --        Warnings cannot be used.

      pragma Warnings (Off);
      for I in Skp.IOMMU.IOMMU_Device_Range loop
         pragma Warnings (On);

         Status := IOMMUs (I).Fault_Status;

         if Status.PPF = 1 then
            pragma Debug (KC.Put_String (Item => "VT-d fault with FRI "));
            pragma Debug (KC.Put_Byte (Item => Status.FRI));
            pragma Debug (KC.Put_String (Item => " - "));

            declare
               Dummy : Reg_Fault_Recording_Type;
            begin
               Dummy := IOMMUs (I).Fault_Recording;

               pragma Debug (Dummy.F = 1, KC.Put_String (Item => "Reason: "));
               pragma Debug (Dummy.F = 1, KC.Put_Byte   (Item => Dummy.FR));
               pragma Debug (Dummy.F = 1, KC.Put_String (Item => ", Info: "));
               pragma Debug (Dummy.F = 1, KC.Put_Word64
                             (Item => SK.Word64 (Dummy.FI * 2 ** 12)));
               pragma Debug (Dummy.F = 1, KC.Put_String (Item => ", Type: "));
               pragma Debug (Dummy.F = 1 and Dummy.T = 0,
                             KC.Put_String ("Write"));
               pragma Debug (Dummy.F = 1 and Dummy.T = 1,
                             KC.Put_String ("Read"));
               pragma Debug (Dummy.F = 1,
                             KC.Put_String (Item => ", Source: "));
               pragma Debug (Dummy.F = 1, KC.Put_Byte
                               (Item => SK.Byte (Dummy.SID / 2 ** 8)));
               pragma Debug (Dummy.F = 1, KC.Put_String (Item => ":"));
               pragma Debug (Dummy.F = 1, KC.Put_Byte
                             (Item => SK.Byte
                              ((Dummy.SID / 2 ** 3) and 16#1f#)));
               pragma Debug (Dummy.F = 1, KC.Put_String (Item => "."));
               pragma Debug (Dummy.F = 1, KC.Put_Byte
                             (Item => SK.Byte (Dummy.SID and 16#07#)));
               pragma Debug (Dummy.F = 1, KC.New_Line);
            end;
            Clear_Fault_Record (IOMMU => I);
         end if;
      end loop;
   end Process_Fault;

   -------------------------------------------------------------------------

   --  Check capabilities of IOMMU given by index. Return False if capability
   --  requirements are not met.
   function Check_Capabilities
     (Idx : Skp.IOMMU.IOMMU_Device_Range)
      return Boolean
   with
      SPARK_Mode => Off -- XXX Workaround for [N425-012]
   is
      Version : Reg_Version_Type;
   begin

      --  Basic sanity check, TODO: check IOMMU capabilities.

      Version := IOMMUs (Idx).Version;
      if Version.MAX /= 1 or else Version.MIN /= 0 then
         pragma Debug (KC.Put_String
                       (Item => "Unsupported IOMMU version "));
         pragma Debug (KC.Put_Byte (Item => SK.Byte (Version.MAX)));
         pragma Debug (KC.Put_Byte (Item => SK.Byte (Version.MIN)));
         pragma Debug (KC.New_Line);
         return False;
      end if;

      return True;
   end Check_Capabilities;

   -------------------------------------------------------------------------

   procedure Initialize
   with
      SPARK_Mode      => Off, -- XXX Workaround for [N425-012]
      Refined_Global  => (In_Out => (X86_64.State, IOMMUs)),
      Refined_Depends => ((X86_64.State, IOMMUs) =>+ null)
   is
      Loop_Count_Max : constant := 10000;
   begin

      --  Systems without an IOMMU have a null range.
      --  NOTE: Due to ticket N704-009 the 'Reason' attribute of pragma
      --        Warnings cannot be used.

      pragma Warnings (Off);
      for I in Skp.IOMMU.IOMMU_Device_Range loop
         pragma Warnings (On);

         if not Check_Capabilities (Idx => I) then
            pragma Debug (KC.Put_String (Item => "IOMMU "));
            pragma Debug (KC.Put_Byte   (Item => SK.Byte (I)));
            pragma Debug (KC.Put_Line   (Item => ": capability check failed"));
            CPU.Panic;
         end if;

         Set_Fault_Event_Mask (IOMMU  => I,
                               Enable => True);
         Clear_Fault_Record (IOMMU => I);
         pragma Debug (Setup_Fault_Interrupt
                       (IOMMU   => I,
                        Vector  => SK.Constants.VTd_Fault_Vector,
                        APIC_ID => SK.Apic.Get_ID));
         pragma Debug (Set_Fault_Event_Mask (IOMMU  => I,
                                             Enable => False));

         Set_Root_Table_Address :
         declare
            Global_Status  : Reg_Global_Status_Type;
            Global_Command : Reg_Global_Command_Type;
         begin
            IOMMUs (I).Root_Table_Address := Skp.IOMMU.Root_Table_Address;
            Global_Command      := IOMMUs (I).Global_Command;
            Global_Command.SRTP := 1;
            IOMMUs (I).Global_Command := Global_Command;

            for J in 1 .. Loop_Count_Max loop
               Global_Status := IOMMUs (I).Global_Status;
               exit when Global_Status.RTPS = 1;
            end loop;
            if Global_Status.RTPS = 0 then
               pragma Debug (KC.Put_String (Item => "IOMMU "));
               pragma Debug (KC.Put_Byte   (Item => SK.Byte (I)));
               pragma Debug (KC.Put_Line
                             (Item => ": unable to set root table address"));
               CPU.Panic;
            end if;
         end Set_Root_Table_Address;

         Invalidate_Context_Cache :
         declare
            Context_Command : Reg_Context_Command_Type;
         begin
            Context_Command      := IOMMUs (I).Context_Command;
            Context_Command.ICC  := 1;
            Context_Command.CIRG := 1;
            IOMMUs (I).Context_Command := Context_Command;

            for J in 1 .. Loop_Count_Max loop
               Context_Command := IOMMUs (I).Context_Command;
               exit when Context_Command.ICC = 0;
            end loop;
            if Context_Command.ICC = 1 then
               pragma Debug (KC.Put_String (Item => "IOMMU "));
               pragma Debug (KC.Put_Byte   (Item => SK.Byte (I)));
               pragma Debug (KC.Put_Line
                             (Item => ": unable to invalidate context cache"));
               CPU.Panic;
            end if;
         end Invalidate_Context_Cache;

         IOTLB_Flush :
         declare
            IOTLB_Invalidate : Reg_IOTLB_Invalidate;
         begin
            IOTLB_Invalidate      := IOMMUs (I).IOTLB_Invalidate;
            IOTLB_Invalidate.IIRG := 1;
            IOTLB_Invalidate.IVT  := 1;
            IOMMUs (I).IOTLB_Invalidate := IOTLB_Invalidate;

            for J in 1 .. Loop_Count_Max loop
               IOTLB_Invalidate := IOMMUs (I).IOTLB_Invalidate;
               exit when IOTLB_Invalidate.IVT = 0;
            end loop;
            if IOTLB_Invalidate.IVT = 1 then
               pragma Debug (KC.Put_String (Item => "IOMMU "));
               pragma Debug (KC.Put_Byte   (Item => SK.Byte (I)));
               pragma Debug (KC.Put_Line
                             (Item => ": unable to flush IOTLB"));
               CPU.Panic;
            end if;
         end IOTLB_Flush;

         Enable_Translation :
         declare
            Global_Command : Reg_Global_Command_Type;
            Global_Status  : Reg_Global_Status_Type;
         begin
            Global_Command    := IOMMUs (I).Global_Command;
            Global_Command.TE := 1;
            IOMMUs (I).Global_Command := Global_Command;

            for J in 1 .. Loop_Count_Max loop
               Global_Status := IOMMUs (I).Global_Status;
               exit when Global_Status.TES = 1;
            end loop;
            if Global_Status.TES = 0 then
               pragma Debug (KC.Put_String (Item => "IOMMU "));
               pragma Debug (KC.Put_Byte   (Item => SK.Byte (I)));
               pragma Debug (KC.Put_Line
                             (Item => ": error enabling translation"));
               CPU.Panic;
            end if;
         end Enable_Translation;

         pragma Debug
           (KC.Put_String ("VT-d DMA address translation for IOMMU "));
         pragma Debug (KC.Put_Byte (SK.Byte (I)));
         pragma Debug (KC.Put_Line (" enabled"));
      end loop;
   end Initialize;

end SK.VTd;
