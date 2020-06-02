with System;

package body Skp.IOMMU
with
   Refined_State => (State => IOMMUs)
is

   pragma Warnings (Off);  --  Only emitted by recent GNATprove versions.
   IOMMUs : IOMMUs_Type
     with
       Volatile,
       Async_Writers,
       Async_Readers,
       Effective_Writes,
       Address => System'To_Address (__base_addr__);
   pragma Annotate
     (GNATprove, Intentional,
      "object is unsuitable for aliasing via address clause",
      "Only necessary registers of MMIO device are specified.");
   pragma Warnings (On);

   -------------------------------------------------------------------------

   function Read_Capability
     (Index : IOMMU_Device_Range)
      return Reg_Capability_Type
   is
      Value : Reg_Capability_Type;
   begin
      case Index is
__body_read_capability_case__
      end case;

      return Value;
   end Read_Capability;

   -------------------------------------------------------------------------

   function Read_Context_Command
     (Index : IOMMU_Device_Range)
      return Reg_Context_Command_Type
   is
      Value : Reg_Context_Command_Type;
   begin
      case Index is
__body_read_context_command_case__
      end case;

      return Value;
   end Read_Context_Command;

   -------------------------------------------------------------------------

   function Read_Extended_Capability
     (Index : IOMMU_Device_Range)
      return Reg_Extcapability_Type
   is
      Value : Reg_Extcapability_Type;
   begin
      case Index is
__body_read_extended_capability_case__
      end case;

      return Value;
   end Read_Extended_Capability;

   -------------------------------------------------------------------------

   function Read_Fault_Event_Address
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Event_Address_Type
   is
      Value : Reg_Fault_Event_Address_Type;
   begin
      case Index is
__body_read_fault_event_address_case__
      end case;

      return Value;
   end Read_Fault_Event_Address;

   -------------------------------------------------------------------------

   function Read_Fault_Event_Control
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Event_Control_Type
   is
      Value : Reg_Fault_Event_Control_Type;
   begin
      case Index is
__body_read_fault_event_control_case__
      end case;

      return Value;
   end Read_Fault_Event_Control;

   -------------------------------------------------------------------------

   function Read_Fault_Event_Data
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Event_Data_Type
   is
      Value : Reg_Fault_Event_Data_Type;
   begin
      case Index is
__body_read_fault_event_data_case__
      end case;

      return Value;
   end Read_Fault_Event_Data;

   -------------------------------------------------------------------------

   function Read_Fault_Recording
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Recording_Type
   is
      Value : Reg_Fault_Recording_Type;
   begin
      case Index is
__body_read_fault_recording_case__
      end case;

      return Value;
   end Read_Fault_Recording;

   -------------------------------------------------------------------------

   function Read_Fault_Status
     (Index : IOMMU_Device_Range)
      return Reg_Fault_Status_Type
   is
      Value : Reg_Fault_Status_Type;
   begin
      case Index is
__body_read_fault_status_case__
      end case;

      return Value;
   end Read_Fault_Status;

   -------------------------------------------------------------------------

   function Read_Global_Status
     (Index : IOMMU_Device_Range)
      return Reg_Global_Status_Type
   is
      Value : Reg_Global_Status_Type;
   begin
      case Index is
__body_read_global_status_case__
      end case;

      return Value;
   end Read_Global_Status;

   -------------------------------------------------------------------------

   function Read_IOTLB_Invalidate
     (Index : IOMMU_Device_Range)
      return Reg_IOTLB_Invalidate
   is
      Value : Reg_IOTLB_Invalidate;
   begin
      case Index is
__body_read_iotlb_invalidate_case__
      end case;

      return Value;
   end Read_IOTLB_Invalidate;

   -------------------------------------------------------------------------

   function Read_Version
     (Index : IOMMU_Device_Range)
      return Reg_Version_Type
   is
      Value : Reg_Version_Type;
   begin
      case Index is
__body_read_version_case__
      end case;

      return Value;
   end Read_Version;

   -------------------------------------------------------------------------

   procedure Write_Context_Command
     (Index : IOMMU_Device_Range;
      Value : Reg_Context_Command_Type)
   is
   begin
      case Index is
__body_write_context_command_case__
      end case;
   end Write_Context_Command;

   -------------------------------------------------------------------------

   procedure Write_Fault_Event_Address
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Event_Address_Type)
   is
   begin
      case Index is
__body_write_fault_event_address_case__
      end case;
   end Write_Fault_Event_Address;

   -------------------------------------------------------------------------

   procedure Write_Fault_Event_Control
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Event_Control_Type)
   is
   begin
      case Index is
__body_write_fault_event_control_case__
      end case;
   end Write_Fault_Event_Control;

   -------------------------------------------------------------------------

   procedure Write_Fault_Event_Data
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Event_Data_Type)
   is
   begin
      case Index is
__body_write_fault_event_data_case__
      end case;
   end Write_Fault_Event_Data;

   -------------------------------------------------------------------------

   procedure Write_Fault_Recording
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Recording_Type)
   is
   begin
      case Index is
__body_write_fault_recording_case__
      end case;
   end Write_Fault_Recording;

   -------------------------------------------------------------------------

   procedure Write_Fault_Status
     (Index : IOMMU_Device_Range;
      Value : Reg_Fault_Status_Type)
   is
   begin
      case Index is
__body_write_fault_status_case__
      end case;
   end Write_Fault_Status;

   -------------------------------------------------------------------------

   procedure Write_Global_Command
     (Index : IOMMU_Device_Range;
      Value : Reg_Global_Command_Type)
   is
   begin
      case Index is
__body_write_global_command_case__
      end case;
   end Write_Global_Command;

   -------------------------------------------------------------------------

   procedure Write_IOTLB_Invalidate
     (Index : IOMMU_Device_Range;
      Value : Reg_IOTLB_Invalidate)
   is
   begin
      case Index is
__body_write_iotlb_invalidate_case__
      end case;
   end Write_IOTLB_Invalidate;

   -------------------------------------------------------------------------

   procedure Write_IRT_Address
     (Index : IOMMU_Device_Range;
      Value : Reg_IRT_Address)
   is
   begin
      case Index is
__body_write_irt_address_case__
      end case;
   end Write_IRT_Address;

   -------------------------------------------------------------------------

   procedure Write_Root_Table_Address
     (Index : IOMMU_Device_Range;
      Value : SK.Word64)
   is
   begin
      case Index is
__body_write_root_table_address_case__
      end case;
   end Write_Root_Table_Address;

end Skp.IOMMU;
