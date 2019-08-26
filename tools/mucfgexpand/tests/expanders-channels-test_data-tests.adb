--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Expanders.Channels.Test_Data.

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
package body Expanders.Channels.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Add_Physical_Memory (Gnattest_T : in out Test);
   procedure Test_Add_Physical_Memory_127041 (Gnattest_T : in out Test) renames Test_Add_Physical_Memory;
--  id:2.2/127041296e3a499b/Add_Physical_Memory/1/0/
   procedure Test_Add_Physical_Memory (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure No_Channels_Section
      is
         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");

         Muxml.Utils.Remove_Child
           (Node       => Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system"),
            Child_Name => "channels");

         Add_Physical_Memory (Data => Policy);

         --  Must not raise an exception.

      end No_Channels_Section;
   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/channels_memory.xml",
         Ref_Diff => "data/channels_memory.xml.diff",
         Expander => Add_Physical_Memory'Access);

      No_Channels_Section;
--  begin read only
   end Test_Add_Physical_Memory;
--  end read only


--  begin read only
   procedure Test_Remove_Global_Channels (Gnattest_T : in out Test);
   procedure Test_Remove_Global_Channels_d7fe15 (Gnattest_T : in out Test) renames Test_Remove_Global_Channels;
--  id:2.2/d7fe15efac82de1d/Remove_Global_Channels/1/0/
   procedure Test_Remove_Global_Channels (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type DOM.Core.Node;

      Policy : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data => Policy,
                   Kind => Muxml.Format_Src,
                   File => "data/test_policy.xml");
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/channels") /= null,
              Message   => "Channels not present");

      Remove_Global_Channels (Data => Policy);
      Assert (Condition => Muxml.Utils.Get_Element
              (Doc   => Policy.Doc,
               XPath => "/system/channels") = null,
              Message   => "Channels still present");
--  begin read only
   end Test_Remove_Global_Channels;
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
end Expanders.Channels.Test_Data.Tests;
