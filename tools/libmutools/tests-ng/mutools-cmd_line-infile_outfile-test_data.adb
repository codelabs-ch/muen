--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with DOM.Core.Documents;

package body Mutools.Cmd_Line.Infile_Outfile.Test_Data is

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

   procedure Immutable_Process
     (Input_Policy : Muxml.XML_Data_Type;
      Output_File  : String)
   is
      use type DOM.Core.Node;
   begin
      if DOM.Core.Documents.Get_Element (Input_Policy.Doc) = null then
         raise Process_Error with "Policy is empty";
      end if;

      if Output_File /= Outfile then
         raise Process_Error with "Outfile mismatch";
      end if;

      Process_Counter := Process_Counter + 1;
   end Immutable_Process;

   -------------------------------------------------------------------------

   procedure Mutable_Process
     (Policy      : in out Muxml.XML_Data_Type;
      Output_File :        String)
   is
   begin
      Immutable_Process (Input_Policy => Policy,
                         Output_File  => Output_File);
   end Mutable_Process;

end Mutools.Cmd_Line.Infile_Outfile.Test_Data;
