--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

package body Expanders.Subjects.Test_Data is

   -------------------------------------------------------------------------

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   -------------------------------------------------------------------------

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   -------------------------------------------------------------------------

   procedure Remove_Subj_Device_Resources (Data : in out Muxml.XML_Data_Type)
   is
      Node : constant DOM.Core.Node
        := Muxml.Utils.Get_Element
          (Doc   => Data.Doc,
           XPath => "/system/subjects/subject[@name='lnx']/devices/"
           & "device[@logical='xhci']");
   begin
      Muxml.Utils.Remove_Child (Node       => Node,
                                Child_Name => "irq");
      Muxml.Utils.Remove_Child (Node       => Node,
                                Child_Name => "memory");
   end Remove_Subj_Device_Resources;

end Expanders.Subjects.Test_Data;
