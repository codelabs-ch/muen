--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Memory.Test_Data.

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
package body Expanders.Memory.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Kernel_Shared_Memory (Gnattest_T : in out Test);
   procedure Test_Add_Kernel_Shared_Memory_063979 (Gnattest_T : in out Test) renames Test_Add_Kernel_Shared_Memory;
--  id:2.2/063979c43e631572/Add_Kernel_Shared_Memory/1/0/
   procedure Test_Add_Kernel_Shared_Memory (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_kernel_shared.xml",
         Ref_Diff => "data/memory_kernel_shared.xml.diff",
         Expander => Add_Kernel_Shared_Memory'Access);
--  begin read only
   end Test_Add_Kernel_Shared_Memory;
--  end read only


--  begin read only
   procedure Test_Add_Kernel_CPU_Local_Memory (Gnattest_T : in out Test);
   procedure Test_Add_Kernel_CPU_Local_Memory_fa2998 (Gnattest_T : in out Test) renames Test_Add_Kernel_CPU_Local_Memory;
--  id:2.2/fa2998bbe940e5a9/Add_Kernel_CPU_Local_Memory/1/0/
   procedure Test_Add_Kernel_CPU_Local_Memory (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_kernel_cpu_local.xml",
         Ref_Diff => "data/memory_kernel_cpu_local.xml.diff",
         Pre      => Add_Subject_Profile'Access,
         Expander => Add_Kernel_CPU_Local_Memory'Access);
--  begin read only
   end Test_Add_Kernel_CPU_Local_Memory;
--  end read only


--  begin read only
   procedure Test_Add_Kernel_PTs (Gnattest_T : in out Test);
   procedure Test_Add_Kernel_PTs_046959 (Gnattest_T : in out Test) renames Test_Add_Kernel_PTs;
--  id:2.2/046959a8b3e4ac50/Add_Kernel_PTs/1/0/
   procedure Test_Add_Kernel_PTs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Policy_Filename => "data/calculate_pt.xml",
         Policy_Format   => Muxml.Format_A,
         Filename        => "obj/memory_kernel_pts.xml",
         Ref_Diff        => "data/memory_kernel_pts.xml.diff",
         Expander        => Add_Kernel_PTs'Access);
--  begin read only
   end Test_Add_Kernel_PTs;
--  end read only


--  begin read only
   procedure Test_Add_Subject_PTs (Gnattest_T : in out Test);
   procedure Test_Add_Subject_PTs_a375d9 (Gnattest_T : in out Test) renames Test_Add_Subject_PTs;
--  id:2.2/a375d92e3b50d142/Add_Subject_PTs/1/0/
   procedure Test_Add_Subject_PTs (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_subject_pts.xml",
         Ref_Diff => "data/memory_subject_pts.xml.diff",
         Pre      => Add_Missing_Elems_Resolve_Aliases'Access,
         Expander => Add_Subject_PTs'Access);
--  begin read only
   end Test_Add_Subject_PTs;
--  end read only


--  begin read only
   procedure Test_Add_Kernel_Stack (Gnattest_T : in out Test);
   procedure Test_Add_Kernel_Stack_d953f4 (Gnattest_T : in out Test) renames Test_Add_Kernel_Stack;
--  id:2.2/d953f47a0b66df59/Add_Kernel_Stack/1/0/
   procedure Test_Add_Kernel_Stack (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_kernel_stack.xml",
         Ref_Diff => "data/memory_kernel_stack.xml.diff",
         Expander => Add_Kernel_Stack'Access);
--  begin read only
   end Test_Add_Kernel_Stack;
--  end read only


--  begin read only
   procedure Test_Add_Subject_States (Gnattest_T : in out Test);
   procedure Test_Add_Subject_States_9c50e6 (Gnattest_T : in out Test) renames Test_Add_Subject_States;
--  id:2.2/9c50e6215c6e4fd4/Add_Subject_States/1/0/
   procedure Test_Add_Subject_States (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_subject_states.xml",
         Ref_Diff => "data/memory_subject_states.xml.diff",
         Expander => Add_Subject_States'Access);
--  begin read only
   end Test_Add_Subject_States;
--  end read only


--  begin read only
   procedure Test_Add_Subject_Timed_Event_Pages (Gnattest_T : in out Test);
   procedure Test_Add_Subject_Timed_Event_Pages_7fec4c (Gnattest_T : in out Test) renames Test_Add_Subject_Timed_Event_Pages;
--  id:2.2/7fec4c8c2f74372a/Add_Subject_Timed_Event_Pages/1/0/
   procedure Test_Add_Subject_Timed_Event_Pages (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_subject_timed_event.xml",
         Ref_Diff => "data/memory_subject_timed_event.xml.diff",
         Expander => Add_Subject_Timed_Event_Pages'Access);
--  begin read only
   end Test_Add_Subject_Timed_Event_Pages;
--  end read only


--  begin read only
   procedure Test_Add_Subject_Interrupts_Pages (Gnattest_T : in out Test);
   procedure Test_Add_Subject_Interrupts_Pages_a97630 (Gnattest_T : in out Test) renames Test_Add_Subject_Interrupts_Pages;
--  id:2.2/a97630038924ff98/Add_Subject_Interrupts_Pages/1/0/
   procedure Test_Add_Subject_Interrupts_Pages (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_subject_interrupts.xml",
         Ref_Diff => "data/memory_subject_interrupts.xml.diff",
         Expander => Add_Subject_Interrupts_Pages'Access);
--  begin read only
   end Test_Add_Subject_Interrupts_Pages;
--  end read only


--  begin read only
   procedure Test_Add_Subject_FPU_State_Regions (Gnattest_T : in out Test);
   procedure Test_Add_Subject_FPU_State_Regions_aa11a3 (Gnattest_T : in out Test) renames Test_Add_Subject_FPU_State_Regions;
--  id:2.2/aa11a39208e771d0/Add_Subject_FPU_State_Regions/1/0/
   procedure Test_Add_Subject_FPU_State_Regions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_subject_fpu.xml",
         Ref_Diff => "data/memory_subject_fpu.xml.diff",
         Expander => Add_Subject_FPU_State_Regions'Access);
--  begin read only
   end Test_Add_Subject_FPU_State_Regions;
--  end read only


--  begin read only
   procedure Test_Add_Tau0_Interface (Gnattest_T : in out Test);
   procedure Test_Add_Tau0_Interface_6b67a0 (Gnattest_T : in out Test) renames Test_Add_Tau0_Interface;
--  id:2.2/6b67a0581135c397/Add_Tau0_Interface/1/0/
   procedure Test_Add_Tau0_Interface (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_tau0_iface.xml",
         Ref_Diff => "data/memory_tau0_iface.xml.diff",
         Expander => Add_Tau0_Interface'Access);
--  begin read only
   end Test_Add_Tau0_Interface;
--  end read only


--  begin read only
   procedure Test_Add_AP_Trampoline (Gnattest_T : in out Test);
   procedure Test_Add_AP_Trampoline_2db30b (Gnattest_T : in out Test) renames Test_Add_AP_Trampoline;
--  id:2.2/2db30b099e003c2a/Add_AP_Trampoline/1/0/
   procedure Test_Add_AP_Trampoline (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_trampoline.xml",
         Ref_Diff => "data/memory_trampoline.xml.diff",
         Expander => Add_AP_Trampoline'Access);
--  begin read only
   end Test_Add_AP_Trampoline;
--  end read only


--  begin read only
   procedure Test_Add_VMXON_Regions (Gnattest_T : in out Test);
   procedure Test_Add_VMXON_Regions_0233b6 (Gnattest_T : in out Test) renames Test_Add_VMXON_Regions;
--  id:2.2/0233b623f10e08f6/Add_VMXON_Regions/1/0/
   procedure Test_Add_VMXON_Regions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_vmxon.xml",
         Ref_Diff => "data/memory_vmxon.xml.diff",
         Expander => Add_VMXON_Regions'Access);
--  begin read only
   end Test_Add_VMXON_Regions;
--  end read only


--  begin read only
   procedure Test_Add_VMCS_Regions (Gnattest_T : in out Test);
   procedure Test_Add_VMCS_Regions_7b5dda (Gnattest_T : in out Test) renames Test_Add_VMCS_Regions;
--  id:2.2/7b5dda6237d4ef13/Add_VMCS_Regions/1/0/
   procedure Test_Add_VMCS_Regions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_vmcs.xml",
         Ref_Diff => "data/memory_vmcs.xml.diff",
         Expander => Add_VMCS_Regions'Access);
--  begin read only
   end Test_Add_VMCS_Regions;
--  end read only


--  begin read only
   procedure Test_Add_Missing_Attributes (Gnattest_T : in out Test);
   procedure Test_Add_Missing_Attributes_8ead35 (Gnattest_T : in out Test) renames Test_Add_Missing_Attributes;
--  id:2.2/8ead35ae70aec256/Add_Missing_Attributes/1/0/
   procedure Test_Add_Missing_Attributes (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_attributes.xml",
         Ref_Diff => "data/memory_attributes.xml.diff",
         Expander => Add_Missing_Attributes'Access);
--  begin read only
   end Test_Add_Missing_Attributes;
--  end read only


--  begin read only
   procedure Test_Add_Subject_Bitmaps (Gnattest_T : in out Test);
   procedure Test_Add_Subject_Bitmaps_4f1ecd (Gnattest_T : in out Test) renames Test_Add_Subject_Bitmaps;
--  id:2.2/4f1ecd09d13ab752/Add_Subject_Bitmaps/1/0/
   procedure Test_Add_Subject_Bitmaps (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_subject_bitmaps.xml",
         Ref_Diff => "data/memory_subject_bitmaps.xml.diff",
         Expander => Add_Subject_Bitmaps'Access);
--  begin read only
   end Test_Add_Subject_Bitmaps;
--  end read only


--  begin read only
   procedure Test_Add_Subject_MSR_Store (Gnattest_T : in out Test);
   procedure Test_Add_Subject_MSR_Store_187092 (Gnattest_T : in out Test) renames Test_Add_Subject_MSR_Store;
--  id:2.2/187092daa53d49c3/Add_Subject_MSR_Store/1/0/
   procedure Test_Add_Subject_MSR_Store (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_subject_msrstore.xml",
         Ref_Diff => "data/memory_subject_msrstore.xml.diff",
         Pre      => Add_Subject_Profile'Access,
         Expander => Add_Subject_MSR_Store'Access);
--  begin read only
   end Test_Add_Subject_MSR_Store;
--  end read only


--  begin read only
   procedure Test_Add_Reserved_Memory_Regions (Gnattest_T : in out Test);
   procedure Test_Add_Reserved_Memory_Regions_03f520 (Gnattest_T : in out Test) renames Test_Add_Reserved_Memory_Regions;
--  id:2.2/03f520b7e268b7c3/Add_Reserved_Memory_Regions/1/0/
   procedure Test_Add_Reserved_Memory_Regions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_reserved_memory_regions.xml",
         Ref_Diff => "data/memory_reserved_memory_regions.xml.diff",
         Expander => Add_Reserved_Memory_Regions'Access);
--  begin read only
   end Test_Add_Reserved_Memory_Regions;
--  end read only


--  begin read only
   procedure Test_Add_Scheduling_Group_Info_Regions (Gnattest_T : in out Test);
   procedure Test_Add_Scheduling_Group_Info_Regions_f9e756 (Gnattest_T : in out Test) renames Test_Add_Scheduling_Group_Info_Regions;
--  id:2.2/f9e756ad4d13a2e9/Add_Scheduling_Group_Info_Regions/1/0/
   procedure Test_Add_Scheduling_Group_Info_Regions (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/memory_sched_group_info.xml",
         Ref_Diff => "data/memory_sched_group_info.xml.diff",
         Pre      => Add_Tau0_And_Subject_IDs'Access,
         Expander => Add_Scheduling_Group_Info_Regions'Access);
--  begin read only
   end Test_Add_Scheduling_Group_Info_Regions;
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
end Expanders.Memory.Test_Data.Tests;
