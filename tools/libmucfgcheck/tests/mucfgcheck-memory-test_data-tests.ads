--  This package has been generated automatically by GNATtest.
--  Do not edit any part of it, see GNATtest documentation for more details.

--  begin read only
with Gnattest_Generated;

package Mucfgcheck.Memory.Test_Data.Tests is

   type Test is new GNATtest_Generated.GNATtest_Standard.Mucfgcheck.Memory.Test_Data.Test
   with null record;

   procedure Test_VMXON_Region_Presence_1b2bcc (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:25:4:VMXON_Region_Presence

   procedure Test_VMXON_Region_Size_265239 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:28:4:VMXON_Region_Size

   procedure Test_VMXON_In_Lowmem_b08e99 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:31:4:VMXON_In_Lowmem

   procedure Test_VMXON_Consecutiveness_1b32f6 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:34:4:VMXON_Consecutiveness

   procedure Test_VMCS_Region_Presence_945465 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:37:4:VMCS_Region_Presence

   procedure Test_VMCS_Region_Size_be694e (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:40:4:VMCS_Region_Size

   procedure Test_Physical_Memory_Name_Uniqueness_460dc7 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:43:4:Physical_Memory_Name_Uniqueness

   procedure Test_Physical_Memory_References_639788 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:46:4:Physical_Memory_References

   procedure Test_Physical_Address_Alignment_7431e2 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:49:4:Physical_Address_Alignment

   procedure Test_Virtual_Address_Alignment_569258 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:52:4:Virtual_Address_Alignment

   procedure Test_Region_Size_827af8 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:55:4:Region_Size

   procedure Test_Entity_Name_Encoding_01db9a (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:59:4:Entity_Name_Encoding

   procedure Test_Physical_Memory_Overlap_ac191e (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:62:4:Physical_Memory_Overlap

   procedure Test_Uncached_Crash_Audit_Presence_2147d5 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:65:4:Uncached_Crash_Audit_Presence

   procedure Test_Crash_Audit_After_Image_049f01 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:68:4:Crash_Audit_After_Image

   procedure Test_Kernel_Data_Region_Presence_18a431 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:71:4:Kernel_Data_Region_Presence

   procedure Test_Kernel_BSS_Region_Presence_c455ac (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:74:4:Kernel_BSS_Region_Presence

   procedure Test_Kernel_Stack_Region_Presence_e9e355 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:77:4:Kernel_Stack_Region_Presence

   procedure Test_Kernel_Intr_Stack_Region_Presence_36ff28 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:80:4:Kernel_Intr_Stack_Region_Presence

   procedure Test_Kernel_PT_Region_Presence_851d89 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:84:4:Kernel_PT_Region_Presence

   procedure Test_Kernel_PT_Below_4G_976943 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:87:4:Kernel_PT_Below_4G

   procedure Test_Kernel_Sched_Group_Info_Mappings_6f1b3f (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:92:4:Kernel_Sched_Group_Info_Mappings

   procedure Test_Subject_State_Region_Presence_33b778 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:95:4:Subject_State_Region_Presence

   procedure Test_Subject_Interrupts_Region_Presence_912d8d (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:99:4:Subject_Interrupts_Region_Presence

   procedure Test_Kernel_Memory_Mappings_fe36fc (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:103:4:Kernel_Memory_Mappings

   procedure Test_System_Memory_Mappings_6ca6be (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:106:4:System_Memory_Mappings

   procedure Test_Device_Memory_Mappings_11f9fd (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:109:4:Device_Memory_Mappings

   procedure Test_Subject_State_Mappings_5f6e13 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:114:4:Subject_State_Mappings

   procedure Test_Subject_Interrupts_Mappings_a36835 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:119:4:Subject_Interrupts_Mappings

   procedure Test_Subject_MSR_Store_Mappings_30a561 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:124:4:Subject_MSR_Store_Mappings

   procedure Test_Subject_Timed_Event_Mappings_fa82de (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:129:4:Subject_Timed_Event_Mappings

   procedure Test_Subject_VMCS_Mappings_6436de (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:134:4:Subject_VMCS_Mappings

   procedure Test_Subject_FPU_State_Mappings_7f61b5 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:139:4:Subject_FPU_State_Mappings

   procedure Test_Subject_FPU_State_Region_Presence_9fdd2f (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:143:4:Subject_FPU_State_Region_Presence

   procedure Test_Subject_Timed_Event_Region_Presence_8a0459 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:148:4:Subject_Timed_Event_Region_Presence

   procedure Test_Subject_MSR_Store_Region_Presence_ef7581 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:153:4:Subject_MSR_Store_Region_Presence

   procedure Test_Scheduling_Group_Info_Region_Presence_54e535 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:158:4:Scheduling_Group_Info_Region_Presence

   procedure Test_Subject_Sched_Group_Info_Mappings_97d94f (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:163:4:Subject_Sched_Group_Info_Mappings

   procedure Test_VTd_Root_Region_Size_bc3a31 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:167:4:VTd_Root_Region_Size

   procedure Test_VTd_Context_Region_Size_4d6204 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:170:4:VTd_Context_Region_Size

   procedure Test_VTd_Root_Region_Presence_b744c5 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:173:4:VTd_Root_Region_Presence

   procedure Test_VTd_IRT_Region_Presence_8b55f8 (Gnattest_T : in out Test);
   --  mucfgcheck-memory.ads:176:4:VTd_IRT_Region_Presence

end Mucfgcheck.Memory.Test_Data.Tests;
--  end read only
