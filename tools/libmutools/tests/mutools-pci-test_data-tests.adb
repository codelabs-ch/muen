--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.PCI.Test_Data.

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
package body Mutools.PCI.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Create (Gnattest_T : in out Test);
   procedure Test_Create_4ce129 (Gnattest_T : in out Test) renames Test_Create;
--  id:2.2/4ce1290887f9b694/Create/1/0/
   procedure Test_Create (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      BDF : BDF_Type := Create
        (Bus    => 56,
         Device => 23,
         Func   => 4);
   begin
      Assert (Condition => BDF.Bus = 56,
              Message   => "Bus mismatch");
      Assert (Condition => BDF.Device = 23,
              Message   => "Device mismatch");
      Assert (Condition => BDF.Func = 4,
              Message   => "Function mismatch");
--  begin read only
   end Test_Create;
--  end read only


--  begin read only
   procedure Test_To_SID (Gnattest_T : in out Test);
   procedure Test_To_SID_183549 (Gnattest_T : in out Test) renames Test_To_SID;
--  id:2.2/183549c092b1579a/To_SID/1/0/
   procedure Test_To_SID (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      use type Interfaces.Unsigned_16;
   begin
      Assert (Condition => To_SID
              (BDF =>
                 (Bus    => 16#f0#,
                  Device => 16#1f#,
                  Func   => 0)) = 16#f0f8#,
              Message   => "SID mismatch");
--  begin read only
   end Test_To_SID;
--  end read only


--  begin read only
   procedure Test_Get_BDF (Gnattest_T : in out Test);
   procedure Test_Get_BDF_a2731a (Gnattest_T : in out Test) renames Test_Get_BDF;
--  id:2.2/a2731a173049c762/Get_BDF/1/0/
   procedure Test_Get_BDF (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl      : DOM.Core.DOM_Implementation;
      Data      : Muxml.XML_Data_Type;
      Node, Dev : DOM.Core.Node;
      BDF       : BDF_Type;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      Dev := DOM.Core.Documents.Create_Element
        (Doc      => Data.Doc,
         Tag_Name => "device");
      Muxml.Utils.Append_Child
        (Node      => Data.Doc,
         New_Child => Dev);

      BDF := Get_BDF (Dev => Dev);
      Assert (Condition => BDF = Null_BDF,
              Message   => "BDF not nil");

      Node := DOM.Core.Nodes.Append_Child
        (N         => Dev,
         New_Child => DOM.Core.Documents.Create_Element
           (Doc      => Data.Doc,
            Tag_Name => "pci"));
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "bus",
         Value => "12");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "device",
         Value => "20");
      DOM.Core.Elements.Set_Attribute
        (Elem  => Node,
         Name  => "function",
         Value => "7");

      BDF := Get_BDF (Dev => Dev);
      Assert (Condition => BDF = Create
              (Bus    => 12,
               Device => 20,
               Func   => 7),
              Message   => "BDF mismatch");
--  begin read only
   end Test_Get_BDF;
--  end read only


--  begin read only
   procedure Test_Create_PCI_Node (Gnattest_T : in out Test);
   procedure Test_Create_PCI_Node_473719 (Gnattest_T : in out Test) renames Test_Create_PCI_Node;
--  id:2.2/473719b6e307609f/Create_PCI_Node/1/0/
   procedure Test_Create_PCI_Node (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      Impl : DOM.Core.DOM_Implementation;
      Data : Muxml.XML_Data_Type;
   begin
      Data.Doc := DOM.Core.Create_Document (Implementation => Impl);

      for B in Bus_Range loop
         for D in Device_Range loop
            for F in Function_Range loop
               declare
                  PCI_Node : constant DOM.Core.Node
                    := Create_PCI_Node (Policy => Data,
                                        Bus    => B,
                                        Device => D,
                                        Func   => F);
                  Bus_Str : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => PCI_Node,
                       Name => "bus");
                  Dev_Str : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => PCI_Node,
                       Name => "device");
                  Fun_Str : constant String
                    := DOM.Core.Elements.Get_Attribute
                      (Elem => PCI_Node,
                       Name => "function");
               begin
                  Assert (Condition => B = Bus_Range'Value (Bus_Str),
                          Message   => B'Img & "," & D'Img & "," & F'Img
                          & ": bus mismatch " & Bus_Str);
                  Assert (Condition => D = Device_Range'Value (Dev_Str),
                          Message   => B'Img & "," & D'Img & "," & F'Img
                          & ": device mismatch " & Dev_Str);
                  Assert (Condition => F = Function_Range'Value (Fun_Str),
                          Message   => B'Img & "," & D'Img & "," & F'Img
                          & ": function mismatch " & Fun_Str);
               end;
            end loop;
         end loop;
      end loop;
--  begin read only
   end Test_Create_PCI_Node;
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
end Mutools.PCI.Test_Data.Tests;
