--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Components.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

package body Expanders.Components.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Binaries (Gnattest_T : in out Test);
   procedure Test_Add_Binaries_531b41 (Gnattest_T : in out Test) renames Test_Add_Binaries;
--  id:2.2/531b419282e96384/Add_Binaries/1/0/
   procedure Test_Add_Binaries (Gnattest_T : in out Test) is
   --  expanders-components.ads:26:4:Add_Binaries
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_add_bins.xml",
         Ref_Diff => "data/components_add_bins.xml.diff",
         Expander => Add_Binaries'Access);
--  begin read only
   end Test_Add_Binaries;
--  end read only


--  begin read only
   procedure Test_Add_Channels (Gnattest_T : in out Test);
   procedure Test_Add_Channels_995b31 (Gnattest_T : in out Test) renames Test_Add_Channels;
--  id:2.2/995b3112fd688185/Add_Channels/1/0/
   procedure Test_Add_Channels (Gnattest_T : in out Test) is
   --  expanders-components.ads:29:4:Add_Channels
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_add_channels.xml",
         Ref_Diff => "data/components_add_channels.xml.diff",
         Pre      => Subjects.Add_Missing_Elements'Access,
         Expander => Add_Channels'Access);
--  begin read only
   end Test_Add_Channels;
--  end read only


--  begin read only
   procedure Test_Add_Memory (Gnattest_T : in out Test);
   procedure Test_Add_Memory_c8ae6b (Gnattest_T : in out Test) renames Test_Add_Memory;
--  id:2.2/c8ae6bbab1e32fc5/Add_Memory/1/0/
   procedure Test_Add_Memory (Gnattest_T : in out Test) is
   --  expanders-components.ads:32:4:Add_Memory
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_add_memory.xml",
         Ref_Diff => "data/components_add_memory.xml.diff",
         Pre      => Subjects.Add_Missing_Elements'Access,
         Expander => Add_Memory'Access);
--  begin read only
   end Test_Add_Memory;
--  end read only


--  begin read only
   procedure Test_Add_Devices (Gnattest_T : in out Test);
   procedure Test_Add_Devices_52dbbf (Gnattest_T : in out Test) renames Test_Add_Devices;
--  id:2.2/52dbbf91ae5d4040/Add_Devices/1/0/
   procedure Test_Add_Devices (Gnattest_T : in out Test) is
   --  expanders-components.ads:35:4:Add_Devices
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_add_devices.xml",
         Ref_Diff => "data/components_add_devices.xml.diff",
         Pre      => Subjects.Add_Missing_Elements'Access,
         Expander => Add_Devices'Access);
--  begin read only
   end Test_Add_Devices;
--  end read only


--  begin read only
   procedure Test_Add_Library_Resources (Gnattest_T : in out Test);
   procedure Test_Add_Library_Resources_794077 (Gnattest_T : in out Test) renames Test_Add_Library_Resources;
--  id:2.2/79407791296d1bb0/Add_Library_Resources/1/0/
   procedure Test_Add_Library_Resources (Gnattest_T : in out Test) is
   --  expanders-components.ads:38:4:Add_Library_Resources
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_add_libs.xml",
         Ref_Diff => "data/components_add_libs.xml.diff",
         Pre      => Subjects.Add_Missing_Elements'Access,
         Expander => Add_Library_Resources'Access);
--  begin read only
   end Test_Add_Library_Resources;
--  end read only


--  begin read only
   procedure Test_Add_Memory_Arrays (Gnattest_T : in out Test);
   procedure Test_Add_Memory_Arrays_68204f (Gnattest_T : in out Test) renames Test_Add_Memory_Arrays;
--  id:2.2/68204f48e1aa9197/Add_Memory_Arrays/1/0/
   procedure Test_Add_Memory_Arrays (Gnattest_T : in out Test) is
   --  expanders-components.ads:41:4:Add_Memory_Arrays
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_memory_arrays.xml",
         Ref_Diff => "data/components_memory_arrays.xml.diff",
         Pre      => Subjects.Add_Missing_Elements'Access,
         Expander => Add_Memory_Arrays'Access);
--  begin read only
   end Test_Add_Memory_Arrays;
--  end read only


--  begin read only
   procedure Test_Add_Channel_Arrays (Gnattest_T : in out Test);
   procedure Test_Add_Channel_Arrays_e821e5 (Gnattest_T : in out Test) renames Test_Add_Channel_Arrays;
--  id:2.2/e821e5afd6c933bc/Add_Channel_Arrays/1/0/
   procedure Test_Add_Channel_Arrays (Gnattest_T : in out Test) is
   --  expanders-components.ads:44:4:Add_Channel_Arrays
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_channel_arrays.xml",
         Ref_Diff => "data/components_channel_arrays.xml.diff",
         Pre      => Subjects.Add_Missing_Elements'Access,
         Expander => Add_Channel_Arrays'Access);
--  begin read only
   end Test_Add_Channel_Arrays;
--  end read only


--  begin read only
   procedure Test_Add_Subject_Profile_VCPU (Gnattest_T : in out Test);
   procedure Test_Add_Subject_Profile_VCPU_8ab93c (Gnattest_T : in out Test) renames Test_Add_Subject_Profile_VCPU;
--  id:2.2/8ab93cbcf52e18f0/Add_Subject_Profile_VCPU/1/0/
   procedure Test_Add_Subject_Profile_VCPU (Gnattest_T : in out Test) is
   --  expanders-components.ads:47:4:Add_Subject_Profile_VCPU
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_add_profile.xml",
         Ref_Diff => "data/components_add_profile.xml.diff",
         Pre      => Subjects.Add_Missing_Elements'Access,
         Expander => Add_Subject_Profile_VCPU'Access);
--  begin read only
   end Test_Add_Subject_Profile_VCPU;
--  end read only


--  begin read only
   procedure Test_Remove_Components (Gnattest_T : in out Test);
   procedure Test_Remove_Components_6de748 (Gnattest_T : in out Test) renames Test_Remove_Components;
--  id:2.2/6de748c3fcaaadad/Remove_Components/1/0/
   procedure Test_Remove_Components (Gnattest_T : in out Test) is
   --  expanders-components.ads:50:4:Remove_Components
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_remove.xml",
         Ref_Diff => "data/components_remove.xml.diff",
         Expander => Remove_Components'Access);
--  begin read only
   end Test_Remove_Components;
--  end read only


--  begin read only
   procedure Test_Remove_Component_Reference (Gnattest_T : in out Test);
   procedure Test_Remove_Component_Reference_da90cf (Gnattest_T : in out Test) renames Test_Remove_Component_Reference;
--  id:2.2/da90cfc643c98267/Remove_Component_Reference/1/0/
   procedure Test_Remove_Component_Reference (Gnattest_T : in out Test) is
   --  expanders-components.ads:53:4:Remove_Component_Reference
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/components_remove_reference.xml",
         Ref_Diff => "data/components_remove_reference.xml.diff",
         Expander => Remove_Component_Reference'Access);
--  begin read only
   end Test_Remove_Component_Reference;
--  end read only

end Expanders.Components.Test_Data.Tests;
