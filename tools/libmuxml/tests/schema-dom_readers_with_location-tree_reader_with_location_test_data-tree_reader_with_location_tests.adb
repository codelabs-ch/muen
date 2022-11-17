--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Schema.Dom_Readers_With_Location.Tree_Reader_With_Location_Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

with Muxml;
with Muxml.Utils;

--  begin read only
--  end read only
package body Schema.Dom_Readers_With_Location.Tree_Reader_With_Location_Test_Data.Tree_Reader_With_Location_Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Start_Element (Gnattest_T : in out Test_Tree_Reader_With_Location);
   procedure Test_Start_Element_ad957f (Gnattest_T : in out Test_Tree_Reader_With_Location) renames Test_Start_Element;
--  id:2.2/ad957ffba9b48e0e/Start_Element/1/0/
   procedure Test_Start_Element (Gnattest_T : in out Test_Tree_Reader_With_Location) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Data : Muxml.XML_Data_Type;
   begin
      Muxml.Parse (Data         => Data,
                   Kind         => Muxml.Format_Src,
                   File         => "data/format_src.xml",
                   Add_Location => True);
      declare
         Output1 : constant String
            :=  Muxml.Utils.Get_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/config/string",
             Name  => "originOfNode");
         Output2 : constant String
            :=  Muxml.Utils.Get_Attribute
            (Doc   => Data.Doc,
             XPath => "/system/platform",
             Name  => "originOfNode");
      begin
         Assert (Condition => "format_src.xml:4:50" = Output1,
                 Message => "String mismatch: " & Output1);
         Assert (Condition => "format_src.xml:41:12" = Output2,
                 Message => "String mismatch: " & Output2);
      end;

--  begin read only
   end Test_Start_Element;
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
end Schema.Dom_Readers_With_Location.Tree_Reader_With_Location_Test_Data.Tree_Reader_With_Location_Tests;
