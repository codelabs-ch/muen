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
   --  expanders-channels.ads:26:4:Add_Physical_Memory
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure No_Channels
      is
         use type DOM.Core.Node;

         Policy : Muxml.XML_Data_Type;
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.Format_Src,
                      File => "data/test_policy.xml");

         declare
            Node     : constant DOM.Core.Node
              := Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system/channels");
            Channels : constant DOM.Core.Node_List
              := McKae.XML.XPath.XIA.XPath_Query
                (N     => Node,
                 XPath => "channel");
         begin
            for I in 0 .. DOM.Core.Nodes.Length (List => Channels) - 1 loop
               Muxml.Utils.Remove_Child
                 (Node       => Node,
                  Child_Name => "channel");
            end loop;
         end;

         Expanders.Channels.Add_Physical_Memory (Data => Policy);

         Assert (Condition => Muxml.Utils.Get_Element
                 (Doc   => Policy.Doc,
                  XPath => "/system/channels") = null,
                 Message   => "Channels still present");
      end No_Channels;

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

         Expanders.Channels.Add_Physical_Memory (Data => Policy);

         --  Must not raise an exception.

      end No_Channels_Section;
   begin
      Test_Utils.Expander.Run_Test
        (Filename => "obj/channels_memory.xml",
         Ref_Diff => "data/channels_memory.xml.diff",
         Expander => Expanders.Channels.Add_Physical_Memory'Access);

      No_Channels_Section;
      No_Channels;
--  begin read only
   end Test_Add_Physical_Memory;
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
