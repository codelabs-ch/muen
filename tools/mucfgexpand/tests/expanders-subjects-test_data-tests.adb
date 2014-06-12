--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Subjects.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;

package body Expanders.Subjects.Test_Data.Tests is


--  begin read only
   procedure Test_Add_Binaries (Gnattest_T : in out Test);
   procedure Test_Add_Binaries_531b41 (Gnattest_T : in out Test) renames Test_Add_Binaries;
--  id:2.2/531b419282e96384/Add_Binaries/1/0/
   procedure Test_Add_Binaries (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:24:4:Add_Binaries
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_binaries.xml",
         Ref_Filename => "data/subjects_binaries.ref.xml",
         Expander     => Add_Binaries'Access);
--  begin read only
   end Test_Add_Binaries;
--  end read only


--  begin read only
   procedure Test_Handle_Profile (Gnattest_T : in out Test);
   procedure Test_Handle_Profile_63fad5 (Gnattest_T : in out Test) renames Test_Handle_Profile;
--  id:2.2/63fad54f185e99dc/Handle_Profile/1/0/
   procedure Test_Handle_Profile (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:28:4:Handle_Profile
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_profiles.xml",
         Ref_Filename => "data/subjects_profiles.ref.xml",
         Expander     => Handle_Profile'Access);
--  begin read only
   end Test_Handle_Profile;
--  end read only


--  begin read only
   procedure Test_Add_Tau0 (Gnattest_T : in out Test);
   procedure Test_Add_Tau0_485f2f (Gnattest_T : in out Test) renames Test_Add_Tau0;
--  id:2.2/485f2f136ee7d8d2/Add_Tau0/1/0/
   procedure Test_Add_Tau0 (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:31:4:Add_Tau0
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_tau0.xml",
         Ref_Filename => "data/subjects_tau0.ref.xml",
         Expander     => Add_Tau0'Access);
--  begin read only
   end Test_Add_Tau0;
--  end read only


--  begin read only
   procedure Test_Handle_Monitors (Gnattest_T : in out Test);
   procedure Test_Handle_Monitors_f27dfd (Gnattest_T : in out Test) renames Test_Handle_Monitors;
--  id:2.2/f27dfdc9e8bf4f96/Handle_Monitors/1/0/
   procedure Test_Handle_Monitors (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:35:4:Handle_Monitors
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_monitors.xml",
         Ref_Filename => "data/subjects_monitors.ref.xml",
         Expander     => Handle_Monitors'Access);
--  begin read only
   end Test_Handle_Monitors;
--  end read only


--  begin read only
   procedure Test_Add_Ids (Gnattest_T : in out Test);
   procedure Test_Add_Ids_619049 (Gnattest_T : in out Test) renames Test_Add_Ids;
--  id:2.2/6190493a9f24bf67/Add_Ids/1/0/
   procedure Test_Add_Ids (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:38:4:Add_Ids
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_ids.xml",
         Ref_Filename => "data/subjects_ids.ref.xml",
         Expander     => Add_Ids'Access);
--  begin read only
   end Test_Add_Ids;
--  end read only


--  begin read only
   procedure Test_Add_Missing_Elements (Gnattest_T : in out Test);
   procedure Test_Add_Missing_Elements_154aec (Gnattest_T : in out Test) renames Test_Add_Missing_Elements;
--  id:2.2/154aec16850b8f76/Add_Missing_Elements/1/0/
   procedure Test_Add_Missing_Elements (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:41:4:Add_Missing_Elements
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_missing_elements.xml",
         Ref_Filename => "data/subjects_missing_elements.ref.xml",
         Expander     => Add_Missing_Elements'Access);
--  begin read only
   end Test_Add_Missing_Elements;
--  end read only


--  begin read only
   procedure Test_Add_Channel_Mappings (Gnattest_T : in out Test);
   procedure Test_Add_Channel_Mappings_4547d6 (Gnattest_T : in out Test) renames Test_Add_Channel_Mappings;
--  id:2.2/4547d6b6e6f42707/Add_Channel_Mappings/1/0/
   procedure Test_Add_Channel_Mappings (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:44:4:Add_Channel_Mappings
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_channels.xml",
         Ref_Filename => "data/subjects_channels.ref.xml",
         Expander     => Add_Channel_Mappings'Access);
--  begin read only
   end Test_Add_Channel_Mappings;
--  end read only


--  begin read only
   procedure Test_Add_Channel_Events (Gnattest_T : in out Test);
   procedure Test_Add_Channel_Events_e021f9 (Gnattest_T : in out Test) renames Test_Add_Channel_Events;
--  id:2.2/e021f991d3ef8e8e/Add_Channel_Events/1/0/
   procedure Test_Add_Channel_Events (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:47:4:Add_Channel_Events
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_channel_events.xml",
         Ref_Filename => "data/subjects_channel_events.ref.xml",
         Expander     => Add_Channel_Events'Access);
--  begin read only
   end Test_Add_Channel_Events;
--  end read only


--  begin read only
   procedure Test_Remove_Channel_Elements (Gnattest_T : in out Test);
   procedure Test_Remove_Channel_Elements_d2d8a9 (Gnattest_T : in out Test) renames Test_Remove_Channel_Elements;
--  id:2.2/d2d8a9d28497a995/Remove_Channel_Elements/1/0/
   procedure Test_Remove_Channel_Elements (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:50:4:Remove_Channel_Elements
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_remove_channels.xml",
         Ref_Filename => "data/subjects_remove_channels.ref.xml",
         Expander     => Remove_Channel_Elements'Access);
--  begin read only
   end Test_Remove_Channel_Elements;
--  end read only


--  begin read only
   procedure Test_Add_Default_Events (Gnattest_T : in out Test);
   procedure Test_Add_Default_Events_b015b8 (Gnattest_T : in out Test) renames Test_Add_Default_Events;
--  id:2.2/b015b89079ac2230/Add_Default_Events/1/0/
   procedure Test_Add_Default_Events (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:53:4:Add_Default_Events
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_default_events.xml",
         Ref_Filename => "data/subjects_default_events.ref.xml",
         Expander     => Add_Default_Events'Access);
--  begin read only
   end Test_Add_Default_Events;
--  end read only


--  begin read only
   procedure Test_Add_Initrd (Gnattest_T : in out Test);
   procedure Test_Add_Initrd_cf4328 (Gnattest_T : in out Test) renames Test_Add_Initrd;
--  id:2.2/cf4328683afda431/Add_Initrd/1/0/
   procedure Test_Add_Initrd (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:57:4:Add_Initrd
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_initrd.xml",
         Ref_Filename => "data/subjects_initrd.ref.xml",
         Expander     => Add_Initrd'Access);
--  begin read only
   end Test_Add_Initrd;
--  end read only


--  begin read only
   procedure Test_Add_CPU_Ids (Gnattest_T : in out Test);
   procedure Test_Add_CPU_Ids_cf5c89 (Gnattest_T : in out Test) renames Test_Add_CPU_Ids;
--  id:2.2/cf5c8998ce7da859/Add_CPU_Ids/1/0/
   procedure Test_Add_CPU_Ids (Gnattest_T : in out Test) is
   --  expanders-subjects.ads:60:4:Add_CPU_Ids
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename     => "obj/subjects_cpu_ids.xml",
         Ref_Filename => "data/subjects_cpu_ids.ref.xml",
         Expander     => Add_CPU_Ids'Access);
--  begin read only
   end Test_Add_CPU_Ids;
--  end read only

end Expanders.Subjects.Test_Data.Tests;
