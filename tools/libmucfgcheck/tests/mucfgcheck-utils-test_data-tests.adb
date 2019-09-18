--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgcheck.Utils.Test_Data.

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
package body Mucfgcheck.Utils.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Is_Adjacent_Number (Gnattest_T : in out Test);
   procedure Test_Is_Adjacent_Number_5c515c (Gnattest_T : in out Test) renames Test_Is_Adjacent_Number;
--  id:2.2/5c515cc24f6f732d/Is_Adjacent_Number/1/0/
   procedure Test_Is_Adjacent_Number (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl   : DOM.Core.DOM_Implementation;
      Data   : Muxml.XML_Data_Type;
      L_Node : DOM.Core.Node;
      R_Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      L_Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => L_Node,
                                       Name  => "number",
                                       Value => "45");
      DOM.Core.Elements.Set_Attribute (Elem  => L_Node,
                                       Name  => "vector",
                                       Value => "0");
      R_Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => R_Node,
                                       Name  => "number",
                                       Value => "46");
      DOM.Core.Elements.Set_Attribute (Elem  => R_Node,
                                       Name  => "vector",
                                       Value => "1234798734234234");

      Assert (Condition => Is_Adjacent_Number
              (Left  => L_Node,
               Right => R_Node,
               Attr  => "number"),
              Message   => "Numbers not adjacent");

      Assert (Condition => not Is_Adjacent_Number
              (Left  => L_Node,
               Right => R_Node,
               Attr  => "vector"),
              Message   => "Vectors adjacent");
--  begin read only
   end Test_Is_Adjacent_Number;
--  end read only


--  begin read only
   procedure Test_Is_Adjacent_Region (Gnattest_T : in out Test);
   procedure Test_Is_Adjacent_Region_92660e (Gnattest_T : in out Test) renames Test_Is_Adjacent_Region;
--  id:2.2/92660ec30d470fcc/Is_Adjacent_Region/1/0/
   procedure Test_Is_Adjacent_Region (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl   : DOM.Core.DOM_Implementation;
      Data   : Muxml.XML_Data_Type;
      L_Node : DOM.Core.Node;
      R_Node : DOM.Core.Node;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      L_Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => L_Node,
                                       Name  => "address",
                                       Value => "16#1000#");
      DOM.Core.Elements.Set_Attribute (Elem  => L_Node,
                                       Name  => "size",
                                       Value => "16#2000#");
      R_Node := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "node");
      DOM.Core.Elements.Set_Attribute (Elem  => R_Node,
                                       Name  => "address",
                                       Value => "16#3000#");
      DOM.Core.Elements.Set_Attribute (Elem  => R_Node,
                                       Name  => "size",
                                       Value => "16#5000#");

      Assert (Condition => Is_Adjacent_Region
              (Left      => L_Node,
               Right     => R_Node,
               Addr_Attr => "address"),
              Message   => "Regions not adjacent (1)");
      Assert (Condition => Is_Adjacent_Region
              (Left      => R_Node,
               Right     => L_Node,
               Addr_Attr => "address"),
              Message   => "Regions not adjacent (2)");

      DOM.Core.Elements.Set_Attribute
        (Elem  => L_Node,
         Name  => "size",
         Value => "16#1000#");
      Assert (Condition => not Is_Adjacent_Region
              (Left      => L_Node,
               Right     => R_Node,
               Addr_Attr => "address"),
              Message   => "Regions adjacent");
--  begin read only
   end Test_Is_Adjacent_Region;
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
end Mucfgcheck.Utils.Test_Data.Tests;
