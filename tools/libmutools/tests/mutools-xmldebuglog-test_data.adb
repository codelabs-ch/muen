--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.
with Mutools.XML_Templates;
with Mutools.Expressions;
with Mutools.Substitutions;
with Mutools.Conditionals;
with Mutools.Amend;

with Muxml.Utils;

package body Mutools.Xmldebuglog.Test_Data is

   procedure Set_Up (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Set_Up;

   procedure Tear_Down (Gnattest_T : in out Test) is
      pragma Unreferenced (Gnattest_T);
   begin
      null;
   end Tear_Down;

   procedure Merge_All_Steps (Data : in out Muxml.XML_Data_Type)
   is
      Node : DOM.Core.Node;
   begin
      Clear_Transaction_Log;
      Clear_Backtrace_Log;

      Mutools.XML_Templates.Expand (XML_Data     => Data,
                                    Debug_Active => True);
      Mutools.Xmldebuglog.Move_Origin_To_Log (Doc => Data.Doc);
      Mutools.Expressions.Expand (Policy       => Data,
                                  Debug_Active => True);
      Node := Muxml.Utils.Get_Element
         (Doc   => Data.Doc,
          XPath => "/system/expressions");
      if Node /= null then
         Mutools.Xmldebuglog.Remove_Log_Of_Subtree (Node => Node);
      end if;
      Muxml.Utils.Remove_Elements
         (Doc   => Data.Doc,
          XPath => "/system/expressions");
      Mutools.Substitutions.Process_Attributes (Data         => Data,
                                                Debug_Active => True);
      Mutools.Conditionals.Expand (Policy       => Data,
                                   Debug_Active => True);
      Mutools.Amend.Expand (XML_Data     => Data,
                            Debug_Active => True);
   end Merge_All_Steps;

end Mutools.Xmldebuglog.Test_Data;
