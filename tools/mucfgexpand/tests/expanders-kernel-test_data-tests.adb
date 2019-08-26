--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Kernel.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Expanders.Kernel.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test);
   procedure Test_Add_Section_Skeleton_797fa9 (Gnattest_T : in out Test) renames Test_Add_Section_Skeleton;
--  id:2.2/797fa93bd19d8580/Add_Section_Skeleton/1/0/
   procedure Test_Add_Section_Skeleton (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Assert (Condition => True,
              Message   => "Tested implicitly");
--  begin read only
   end Test_Add_Section_Skeleton;
--  end read only


--  begin read only
   procedure Test_Add_Binary_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Binary_Mappings_234898 (Gnattest_T : in out Test) renames Test_Add_Binary_Mappings;
--  id:2.2/2348981df527cc4e/Add_Binary_Mappings/1/0/
   procedure Test_Add_Binary_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_binary_mappings.xml",
         Ref_Diff => "data/kernel_binary_mappings.xml.diff",
         Pre      => Add_Section_Skeleton'Access,
         Expander => Add_Binary_Mappings'Access);
--  begin read only
   end Test_Add_Binary_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Subj_VMCS_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Subj_VMCS_Mappings_0bc5b2 (Gnattest_T : in out Test) renames Test_Add_Subj_VMCS_Mappings;
--  id:2.2/0bc5b2fe24f65063/Add_Subj_VMCS_Mappings/1/0/
   procedure Test_Add_Subj_VMCS_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_subj_vmcs_mappings.xml",
         Ref_Diff => "data/kernel_subj_vmcs_mappings.xml.diff",
         Pre      => Pre_Subj_Mappings'Access,
         Expander => Add_Subj_VMCS_Mappings'Access);
--  begin read only
   end Test_Add_Subj_VMCS_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Subj_State_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Subj_State_Mappings_5c5e62 (Gnattest_T : in out Test) renames Test_Add_Subj_State_Mappings;
--  id:2.2/5c5e62317bc08193/Add_Subj_State_Mappings/1/0/
   procedure Test_Add_Subj_State_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_subj_state_mappings.xml",
         Ref_Diff => "data/kernel_subj_state_mappings.xml.diff",
         Pre      => Pre_Subj_Mappings'Access,
         Expander => Add_Subj_State_Mappings'Access);
--  begin read only
   end Test_Add_Subj_State_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Subj_Timed_Event_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Subj_Timed_Event_Mappings_a6660d (Gnattest_T : in out Test) renames Test_Add_Subj_Timed_Event_Mappings;
--  id:2.2/a6660ddc7571829d/Add_Subj_Timed_Event_Mappings/1/0/
   procedure Test_Add_Subj_Timed_Event_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_subj_timed_event_mappings.xml",
         Ref_Diff => "data/kernel_subj_timed_event_mappings.xml.diff",
         Pre      => Pre_Subj_Mappings'Access,
         Expander => Add_Subj_Timed_Event_Mappings'Access);
--  begin read only
   end Test_Add_Subj_Timed_Event_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Subj_Interrupts_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Subj_Interrupts_Mappings_4c7af6 (Gnattest_T : in out Test) renames Test_Add_Subj_Interrupts_Mappings;
--  id:2.2/4c7af612e6d2d482/Add_Subj_Interrupts_Mappings/1/0/
   procedure Test_Add_Subj_Interrupts_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_subj_interrupts_mappings.xml",
         Ref_Diff => "data/kernel_subj_interrupts_mappings.xml.diff",
         Pre      => Pre_Subj_Mappings'Access,
         Expander => Add_Subj_Interrupts_Mappings'Access);
--  begin read only
   end Test_Add_Subj_Interrupts_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Subj_MSR_Store_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Subj_MSR_Store_Mappings_09dcb8 (Gnattest_T : in out Test) renames Test_Add_Subj_MSR_Store_Mappings;
--  id:2.2/09dcb888659f5bdd/Add_Subj_MSR_Store_Mappings/1/0/
   procedure Test_Add_Subj_MSR_Store_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_subj_msr_store_mappings.xml",
         Ref_Diff => "data/kernel_subj_msr_store_mappings.xml.diff",
         Pre      => Pre_Subj_MSR_Store_Mappings'Access,
         Expander => Add_Subj_MSR_Store_Mappings'Access);
--  begin read only
   end Test_Add_Subj_MSR_Store_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Subj_FPU_State_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Subj_FPU_State_Mappings_ade765 (Gnattest_T : in out Test) renames Test_Add_Subj_FPU_State_Mappings;
--  id:2.2/ade765db964e8bfd/Add_Subj_FPU_State_Mappings/1/0/
   procedure Test_Add_Subj_FPU_State_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_subj_fpu_state_mappings.xml",
         Ref_Diff => "data/kernel_subj_fpu_state_mappings.xml.diff",
         Pre      => Pre_Subj_Mappings'Access,
         Expander => Add_Subj_FPU_State_Mappings'Access);
--  begin read only
   end Test_Add_Subj_FPU_State_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Sched_Group_Info_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Sched_Group_Info_Mappings_a6ca47 (Gnattest_T : in out Test) renames Test_Add_Sched_Group_Info_Mappings;
--  id:2.2/a6ca47d1783a39bf/Add_Sched_Group_Info_Mappings/1/0/
   procedure Test_Add_Sched_Group_Info_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_sched_group_info_mapping.xml",
         Ref_Diff => "data/kernel_sched_group_info_mapping.xml.diff",
         Pre      => Pre_Sched_Group_Info_Mappings'Access,
         Expander => Add_Sched_Group_Info_Mappings'Access);
--  begin read only
   end Test_Add_Sched_Group_Info_Mappings;
--  end read only


--  begin read only
   procedure Test_Map_Tau0_Interface (Gnattest_T : in out Test);
   procedure Test_Map_Tau0_Interface_1c6595 (Gnattest_T : in out Test) renames Test_Map_Tau0_Interface;
--  id:2.2/1c659557f0d945b9/Map_Tau0_Interface/1/0/
   procedure Test_Map_Tau0_Interface (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_tau0_iface_mapping.xml",
         Ref_Diff => "data/kernel_tau0_iface_mapping.xml.diff",
         Pre      => Pre_Map_Tau0_Interface'Access,
         Expander => Map_Tau0_Interface'Access);
--  begin read only
   end Test_Map_Tau0_Interface;
--  end read only


--  begin read only
   procedure Test_Add_Devices (Gnattest_T : in out Test);
   procedure Test_Add_Devices_52dbbf (Gnattest_T : in out Test) renames Test_Add_Devices;
--  id:2.2/52dbbf91ae5d4040/Add_Devices/1/0/
   procedure Test_Add_Devices (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_devices.xml",
         Ref_Diff => "data/kernel_devices.xml.diff",
         Pre      => Add_Section_Skeleton'Access,
         Expander => Add_Devices'Access);
--  begin read only
   end Test_Add_Devices;
--  end read only


--  begin read only
   procedure Test_Add_Crash_Audit_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Crash_Audit_Mappings_2bfbc6 (Gnattest_T : in out Test) renames Test_Add_Crash_Audit_Mappings;
--  id:2.2/2bfbc6ab4ff54717/Add_Crash_Audit_Mappings/1/0/
   procedure Test_Add_Crash_Audit_Mappings (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_crash_audit_mappings.xml",
         Ref_Diff => "data/kernel_crash_audit_mappings.xml.diff",
         Pre      => Add_Section_Skeleton'Access,
         Expander => Add_Crash_Audit_Mappings'Access);
--  begin read only
   end Test_Add_Crash_Audit_Mappings;
--  end read only


--  begin read only
   procedure Test_Remove_Diagnostics_Device (Gnattest_T : in out Test);
   procedure Test_Remove_Diagnostics_Device_b093e6 (Gnattest_T : in out Test) renames Test_Remove_Diagnostics_Device;
--  id:2.2/b093e6fd6844d691/Remove_Diagnostics_Device/1/0/
   procedure Test_Remove_Diagnostics_Device (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/kernel_diag_dev.xml",
         Ref_Diff => "data/kernel_diag_dev.xml.diff",
         Expander => Remove_Diagnostics_Device'Access);
--  begin read only
   end Test_Remove_Diagnostics_Device;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Expanders.Kernel.Test_Data.Tests;
