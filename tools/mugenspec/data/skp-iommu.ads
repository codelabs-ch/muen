with SK;

--D @Interface
--D This package contains constant definitions and subprograms to interface
--D with IOMMUs. The generation of the code is necessary because there exist
--D hardware platforms with multiple IOMMUs that have different register memory
--D layouts, i.e. Fault Reporting and IOTLB Registers offsets. The necessary
--D values are taken from the system policy.
package Skp.IOMMU
with
   Abstract_State =>
     (State with External => (Async_Writers, Async_Readers, Effective_Writes)),
   Initializes    => State
is

   --D @Interface
   --D Physical memory address of DMAR root table.
   Root_Table_Address    : constant := 16#0030_0000#;
   --D @Interface
   --D Physical memory address of IR table.
   IR_Table_Phys_Address : constant := 16#0301#;
   --D @Interface
   --D Size of the IR table. The number of entries in the IRT is
   --D $2^{\text{IR\_Table\_Size}+1}$.
   IR_Table_Size         : constant := 7;

   --D @Interface
   --D Expected Actual Guest Address Width used during setup to check against
   --D Supported Adjusted Guest Address Widths of IOMMUs.
   Cap_AGAW_Bit          : constant := 2;

   --D @Interface
   --D Type defining range of active IOMMU devices.
   type IOMMU_Device_Range is range 1 .. 2;

   --  Basic types

   type Bit_2_Type is range 0 .. 2 ** 2 - 1
     with
       Size => 2;

   type Bit_3_Type is range 0 .. 2 ** 3 - 1
     with
       Size => 3;

   type Bit_4_Type is range 0 .. 2 ** 4 - 1
     with
       Size => 4;

   type Bit_10_Type is range 0 .. 2 ** 10 - 1
     with
       Size => 10;

   type Bit_52_Type is range 0 .. 2 ** 52 - 1
     with
       Size => 52;

   --  Registers

   --  Version register
   type Reg_Version_Type is record
      MIN      : Bit_4_Type;
      MAX      : Bit_4_Type;
      Reserved : SK.Bit_Array (1 .. 24);
   end record
     with
       Size => 32;

   for Reg_Version_Type use record
      MIN      at 0 range 0 ..  3;
      MAX      at 0 range 4 ..  7;
      Reserved at 0 range 8 .. 31;
   end record;

   --  Capability register
   type Reg_Capability_Type is record
      ND         : Bit_3_Type;
      AFL        : SK.Bit_Type;
      RWBF       : SK.Bit_Type;
      PLMR       : SK.Bit_Type;
      PHMR       : SK.Bit_Type;
      CM         : SK.Bit_Type;
      SAGAW      : SK.Bit_Array (1 .. 5);
      Reserved_1 : SK.Bit_Array (1 .. 3);
      MGAW       : SK.Bit_Array (1 .. 6);
      ZLR        : SK.Bit_Type;
      Reserved_2 : SK.Bit_Type;
      FRO        : Bit_10_Type;
      SLLPS      : SK.Bit_Array (1 .. 4);
      Reserved_3 : SK.Bit_Type;
      PSI        : SK.Bit_Type;
      NFR        : SK.Byte;
      MAMV       : SK.Bit_Array (1 .. 6);
      DWD        : SK.Bit_Type;
      DRD        : SK.Bit_Type;
      FL1GP      : SK.Bit_Type;
      Reserved_4 : SK.Bit_Array (1 .. 7);
   end record
     with Size => 64;

   for Reg_Capability_Type use record
      ND         at 0 range 0  .. 2;
      AFL        at 0 range 3  .. 3;
      RWBF       at 0 range 4  .. 4;
      PLMR       at 0 range 5  .. 5;
      PHMR       at 0 range 6  .. 6;
      CM         at 0 range 7  .. 7;
      SAGAW      at 0 range 8  .. 12;
      Reserved_1 at 0 range 13 .. 15;
      MGAW       at 0 range 16 .. 21;
      ZLR        at 0 range 22 .. 22;
      Reserved_2 at 0 range 23 .. 23;
      FRO        at 0 range 24 .. 33;
      SLLPS      at 0 range 34 .. 37;
      Reserved_3 at 0 range 38 .. 38;
      PSI        at 0 range 39 .. 39;
      NFR        at 0 range 40 .. 47;
      MAMV       at 0 range 48 .. 53;
      DWD        at 0 range 54 .. 54;
      DRD        at 0 range 55 .. 55;
      FL1GP      at 0 range 56 .. 56;
      Reserved_4 at 0 range 57 .. 63;
   end record;

   --  Extended Capability register
   type Reg_Extcapability_Type is record
      C          : SK.Bit_Type;
      QI         : SK.Bit_Type;
      DT         : SK.Bit_Type;
      IR         : SK.Bit_Type;
      EIM        : SK.Bit_Type;
      Reserved_1 : SK.Bit_Type;
      PT         : SK.Bit_Type;
      SC         : SK.Bit_Type;
      IRO        : Bit_10_Type;
      Reserved_2 : SK.Bit_Array (1 .. 2);
      MHMV       : SK.Bit_Array (1 .. 4);
      ECS        : SK.Bit_Type;
      MTS        : SK.Bit_Type;
      NEST       : SK.Bit_Type;
      DIS        : SK.Bit_Type;
      PASID      : SK.Bit_Type;
      PRS        : SK.Bit_Type;
      ERS        : SK.Bit_Type;
      SRS        : SK.Bit_Type;
      POT        : SK.Bit_Type;
      NWFS       : SK.Bit_Type;
      EAFS       : SK.Bit_Type;
      PSS        : SK.Bit_Array (1 .. 5);
      Reserved_3 : SK.Bit_Array (1 .. 24);
   end record;

   for Reg_Extcapability_Type use record
      C          at 0 range 0  .. 0;
      QI         at 0 range 1  .. 1;
      DT         at 0 range 2  .. 2;
      IR         at 0 range 3  .. 3;
      EIM        at 0 range 4  .. 4;
      Reserved_1 at 0 range 5  .. 5;
      PT         at 0 range 6  .. 6;
      SC         at 0 range 7  .. 7;
      IRO        at 0 range 8  .. 17;
      Reserved_2 at 0 range 18 .. 19;
      MHMV       at 0 range 20 .. 23;
      ECS        at 0 range 24 .. 24;
      MTS        at 0 range 25 .. 25;
      NEST       at 0 range 26 .. 26;
      DIS        at 0 range 27 .. 27;
      PASID      at 0 range 28 .. 28;
      PRS        at 0 range 29 .. 29;
      ERS        at 0 range 30 .. 30;
      SRS        at 0 range 31 .. 31;
      POT        at 0 range 32 .. 32;
      NWFS       at 0 range 33 .. 33;
      EAFS       at 0 range 34 .. 34;
      PSS        at 0 range 35 .. 39;
      Reserved_3 at 0 range 40 .. 63;
   end record;

   --  Global Command Register
   type Reg_Global_Command_Type is record
      Reserved : SK.Bit_Array (1 .. 23);
      CFI      : SK.Bit_Type;
      SIRTP    : SK.Bit_Type;
      IRE      : SK.Bit_Type;
      QIE      : SK.Bit_Type;
      WBF      : SK.Bit_Type;
      EAFL     : SK.Bit_Type;
      SFL      : SK.Bit_Type;
      SRTP     : SK.Bit_Type;
      TE       : SK.Bit_Type;
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
      Reserved : SK.Bit_Array (1 .. 23);
      CFIS     : SK.Bit_Type;
      IRTPS    : SK.Bit_Type;
      IRES     : SK.Bit_Type;
      QIES     : SK.Bit_Type;
      WBFS     : SK.Bit_Type;
      AFLS     : SK.Bit_Type;
      FLS      : SK.Bit_Type;
      RTPS     : SK.Bit_Type;
      TES      : SK.Bit_Type;
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
      Unused : SK.Bit_Array (1 .. 59);
      CAIG   : Bit_2_Type;
      CIRG   : Bit_2_Type;
      ICC    : SK.Bit_Type;
   end record
     with
       Size => 64;

   for Reg_Context_Command_Type use record
      Unused at 0 range  0 .. 58;
      CAIG   at 0 range 59 .. 60;
      CIRG   at 0 range 61 .. 62;
      ICC    at 0 range 63 .. 63;
   end record;

   --  Fault Status Register
   type Reg_Fault_Status_Type is record
      PFO      : SK.Bit_Type;
      PPF      : SK.Bit_Type;
      AFO      : SK.Bit_Type;
      APF      : SK.Bit_Type;
      IQE      : SK.Bit_Type;
      ICE      : SK.Bit_Type;
      ITE      : SK.Bit_Type;
      PRO      : SK.Bit_Type;
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
      Reserved : SK.Bit_Array (1 .. 30);
      IP       : SK.Bit_Type;
      IM       : SK.Bit_Type;
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
      Reserved_1       : SK.Bit_Array (1 .. 2);
      Destination_Mode : SK.Bit_Type;
      Redirection_Hint : SK.Bit_Type;
      Reserved_2       : SK.Byte;
      APIC_ID          : SK.Byte;
      FEEh             : SK.Bit_Array (1 .. 12);
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

   --  Interrupt Remapping Table Address Register
   type Reg_IRT_Address is record
      S        : Bit_4_Type;
      Reserved : SK.Bit_Array (1 .. 7);
      EIME     : SK.Bit_Type;
      IRTA     : Bit_52_Type;
   end record
     with
       Size => 64;

   for Reg_IRT_Address use record
      S        at 0 range  0 .. 3;
      Reserved at 0 range  4 .. 10;
      EIME     at 0 range 11 .. 11;
      IRTA     at 0 range 12 .. 63;
   end record;

   --  IOTLB Invalidate Register (dynamic)
   type Reg_IOTLB_Invalidate is record
      Unused     : SK.Bit_Array (1 .. 57);
      IAIG       : Bit_2_Type;
      Reserved_1 : SK.Bit_Type;
      IIRG       : Bit_2_Type;
      Reserved_2 : SK.Bit_Type;
      IVT        : SK.Bit_Type;
   end record;

   for Reg_IOTLB_Invalidate use record
      Unused     at 0 range 0  .. 56;
      IAIG       at 0 range 57 .. 58;
      Reserved_1 at 0 range 59 .. 59;
      IIRG       at 0 range 60 .. 61;
      Reserved_2 at 0 range 62 .. 62;
      IVT        at 0 range 63 .. 63;
   end record;

   Reg_Fault_Recording_Size : constant := 128;

   --  Fault Recording Register (dynamic)
   type Reg_Fault_Recording_Type is record
      Reserved_1 : SK.Bit_Array (1 .. 12);
      FI         : Bit_52_Type;
      SID        : SK.Word16;
      Reserved_2 : SK.Bit_Array (1 .. 13);
      PRIV       : SK.Bit_Type;
      EXE        : SK.Bit_Type;
      PP         : SK.Bit_Type;
      FR         : SK.Byte;
      PV         : SK.Bit_Array (1 .. 20);
      AType      : Bit_2_Type;
      T          : SK.Bit_Type;
      F          : SK.Bit_Type;
   end record
     with
       Size => Reg_Fault_Recording_Size;

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

   type Fault_Recording_Index is mod 8;

   type Reg_Fault_Recording_Array is array
     (Fault_Recording_Index) of Reg_Fault_Recording_Type
   with
      Pack,
      Size => 8 * Reg_Fault_Recording_Size;

   function Read_Version
     (Index : IOMMU_Device_Range)
      return Reg_Version_Type
   with
      Volatile_Function;

   function Read_Capability
     (Index : IOMMU_Device_Range)
      return Reg_Capability_Type
   with
      Volatile_Function;

   function Read_Extended_Capability
     (Index : IOMMU_Device_Range)
      return Reg_Extcapability_Type
   with
      Volatile_Function;

   function Read_Global_Status
     (Index : IOMMU_Device_Range)
      return Reg_Global_Status_Type
   with
      Volatile_Function;

   procedure Write_Root_Table_Address
     (Index : IOMMU_Device_Range;
      Value : SK.Word64);

   procedure Write_Global_Command
     (Index : IOMMU_Device_Range;
      Value : Reg_Global_Command_Type);

   function Read_Context_Command
     (Index : IOMMU_Device_Range)
      return Reg_Context_Command_Type
   with
      Volatile_Function;

   procedure Write_Context_Command
     (Index : IOMMU_Device_Range;
      Value : Reg_Context_Command_Type);

   function Read_Fault_Recording
     (Index : IOMMU_Device_Range;
      FRI   : Fault_Recording_Index)
      return Reg_Fault_Recording_Type
   with
      Volatile_Function;

   procedure Write_Fault_Recording
     (Index : IOMMU_Device_Range;
      FRI   : Fault_Recording_Index;
      Value : Reg_Fault_Recording_Type);

   function Read_Fault_Status
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Status_Type
   with
      Volatile_Function;

   procedure Write_Fault_Status
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Status_Type);

   function Read_Fault_Event_Control
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Event_Control_Type
   with
      Volatile_Function;

   procedure Write_Fault_Event_Control
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Event_Control_Type);

   function Read_Fault_Event_Address
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Event_Address_Type
   with
      Volatile_Function;

   procedure Write_Fault_Event_Address
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Event_Address_Type);

   function Read_Fault_Event_Data
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Event_Data_Type
   with
      Volatile_Function;

   procedure Write_Fault_Event_Data
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Event_Data_Type);

   function Read_IOTLB_Invalidate
     (Index : IOMMU_Device_Range)
      return Reg_IOTLB_Invalidate
   with
      Volatile_Function;

   procedure Write_IOTLB_Invalidate
     (Index : IOMMU_Device_Range;
      Value : Reg_IOTLB_Invalidate);

   procedure Write_IRT_Address
     (Index : IOMMU_Device_Range;
      Value : Reg_IRT_Address);

   function Config_Get_IOTLB_Inv_Offset
     (Index : IOMMU_Device_Range)
      return SK.Word16;

   function Config_Get_FR_Offset
     (Index : IOMMU_Device_Range)
      return SK.Word16;

private

   IOTLB_Inv_Offset_1 : constant := 264;
   IOTLB_Inv_Offset_2 : constant := 1280;

   IOTLB_Inv_Offsets : constant array (IOMMU_Device_Range) of SK.Word16
     := (
         1 => IOTLB_Inv_Offset_1,
         2 => IOTLB_Inv_Offset_2
        );

   FR_Offset_1 : constant := 512;
   FR_Offset_2 : constant := 514;

   FR_Offsets : constant array (IOMMU_Device_Range) of SK.Word16
     := (
         1 => FR_Offset_1,
         2 => FR_Offset_2
        );

   IOMMU_1_Type_Size : constant := 8 * 512 + 1024;
   IOMMU_2_Type_Size : constant := 8 * 1280 + 64;

   function Config_Get_IOTLB_Inv_Offset
     (Index : IOMMU_Device_Range)
      return SK.Word16 is (IOTLB_Inv_Offsets (Index));

   function Config_Get_FR_Offset
     (Index : IOMMU_Device_Range)
      return SK.Word16 is (FR_Offsets (Index));

   --  NOTE: The Intel VT-d Specification, "10.2 Software Access to Registers"
   --  mentions that software is expected to access registers as a whole. To
   --  avoid side-effects from partial/wider reads always read the entire
   --  record field/register, modify the appropriate values and write back the
   --  new data (see also GNATtracker ticket N307-023).

   IOMMU_Common_Size : constant := 192 * 8;

   --D @Interface
   --D Common register definitions that are the same for IOMMUs of any
   --D generation, see Intel VT-d Specification, "10.4 Register Descriptions".
   type IOMMU_Common_Type is record
      Version             : Reg_Version_Type;
      Reserved_1          : SK.Word32;
      Capability          : Reg_Capability_Type;
      Ext_Capability      : Reg_Extcapability_Type;
      Global_Command      : Reg_Global_Command_Type;
      Global_Status       : Reg_Global_Status_Type;
      Root_Table_Address  : SK.Word64;
      Context_Command     : Reg_Context_Command_Type;
      Reserved_2          : SK.Word32;
      Fault_Status        : Reg_Fault_Status_Type;
      Fault_Event_Control : Reg_Fault_Event_Control_Type;
      Fault_Event_Data    : Reg_Fault_Event_Data_Type;
      Fault_Event_Address : Reg_Fault_Event_Address_Type;
      Reserved_3          : SK.Bit_Array (1 .. 928);
      IRT_Address         : Reg_IRT_Address;
   end record
     with
       Size => IOMMU_Common_Size;

   for IOMMU_Common_Type use record
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
      Reserved_3          at 16#44# range 0 .. 927;
      IRT_Address         at 16#b8# range 0 .. 63;
   end record;

   --D @Interface
   --D Definition of VT-d IOMMU register layout for programming IOMMU devices
   --D using memory-mapped register access.
   type IOMMU_X_Type is record
      Common           : IOMMU_Common_Type;
      --D @Interface
      --D IOTLB registers used for IOTLB invalidation. They have a variable
      --D offset depending on the specific IOMMU device, see Intel VT-d
      --D Specification, "10.4.8 IOTLB Registers".
      IOTLB_Invalidate : Reg_IOTLB_Invalidate;
      --D @Interface
      --D Fault recording registers used providing error information on VT-d
      --D faults. Their number and offset varies depending on the specific
      --D IOMMU device, see Intel VT-d Specification, "10.4.14 Fault Recording
      --D Registers [n]".
      Fault_Recording  : Reg_Fault_Recording_Array;
   end record;

   type IOMMU_1_Type is new IOMMU_X_Type with Size => IOMMU_1_Type_Size;
   type IOMMU_2_Type is new IOMMU_X_Type with Size => IOMMU_2_Type_Size;

   --  Disable potential "no warnings suppressed" warning as this is only
   --  relevant for certain hardware targets (e.g. X260, NUC 6CAYH).
   pragma Warnings (Off);
   pragma Warnings (Off, "memory layout out of order");
   pragma Warnings (On);

   pragma Warnings (Off, "*-bit gap before component *");
   for IOMMU_1_Type use record
      Common at 0 range 0 .. IOMMU_Common_Size - 1;
      IOTLB_Invalidate at IOTLB_Inv_Offset_1 range 0 .. 63;
      Fault_Recording at FR_Offset_1 range 0 .. 8 * Reg_Fault_Recording_Size - 1;
   end record;

   for IOMMU_2_Type use record
      Common at 0 range 0 .. IOMMU_Common_Size - 1;
      IOTLB_Invalidate at IOTLB_Inv_Offset_2 range 0 .. 63;
      Fault_Recording at FR_Offset_2 range 0 .. 8 * Reg_Fault_Recording_Size - 1;
   end record;
   pragma Warnings (On, "*-bit gap before component *");
   pragma Warnings (On, "memory layout out of order");

   --D @Interface
   --D Definition of VT-d IOMMU layout for all programmable IOMMU(s) of the
   --D system.
   type IOMMUs_Type is record
      --D @Interface
      --D Memory-mapped registers of IOMMU 1.
      IOMMU_1 : IOMMU_1_Type;
      --D @Interface
      --D Padding for IOMMU 1 to 4K.
      Padding_1 : SK.Bit_Array (1 .. SK.Page_Size * 8 - IOMMU_1_Type_Size);
      --D @Interface
      --D Memory-mapped registers of IOMMU 2.
      IOMMU_2 : IOMMU_2_Type;
      --D @Interface
      --D Padding for IOMMU 2 to 4K.
      Padding_2 : SK.Bit_Array (1 .. SK.Page_Size * 8 - IOMMU_2_Type_Size);
   end record
     with
       Pack,
       Alignment => SK.Page_Size;

end Skp.IOMMU;
