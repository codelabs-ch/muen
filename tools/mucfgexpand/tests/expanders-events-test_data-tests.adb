--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Events.Test_Data.

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
package body Expanders.Events.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Handle_Asap_Events (Gnattest_T : in out Test);
   procedure Test_Handle_Asap_Events_be9a1d (Gnattest_T : in out Test) renames Test_Handle_Asap_Events;
--  id:2.2/be9a1d5e3f1ca349/Handle_Asap_Events/1/0/
   procedure Test_Handle_Asap_Events (Gnattest_T : in out Test) is
   --  expanders-events.ads:26:4:Handle_Asap_Events
--  end read only

      pragma Unreferenced (Gnattest_T);

   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/events_asap.xml",
         Ref_Diff => "data/events_asap.xml.diff",
         Pre      => Prepare_Asap_Events'Access,
         Expander => Handle_Asap_Events'Access);
--  begin read only
   end Test_Handle_Asap_Events;
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
end Expanders.Events.Test_Data.Tests;
