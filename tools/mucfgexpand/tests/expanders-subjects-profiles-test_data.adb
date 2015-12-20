--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with DOM.Core;

with Muxml.Utils;
with Mutools.XML_Utils;

with Expanders.Subjects.Test_Data;

package body Expanders.Subjects.Profiles.Test_Data is

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

   -------------------------------------------------------------------------

   procedure Prepare_Profile_BIOS (Data: in out Muxml.XML_Data_Type)
   is
   begin
      Expanders.Subjects.Test_Data.Prepare_Profile (Data => Data);

      declare
         Subj_Mem : constant DOM.Core.Node := Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/subjects/subject[@name='lnx']/memory");
      begin
         Muxml.Utils.Append_Child
           (Node      => Subj_Mem,
            New_Child => Mutools.XML_Utils.Create_Virtual_Memory_Node
              (Policy        => Data,
               Logical_Name  => "existing_bios",
               Physical_Name => "dummy",
               Address       => "16#000c_0000#",
               Writable      => False,
               Executable    => False));
      end;
   end Prepare_Profile_BIOS;

end Expanders.Subjects.Profiles.Test_Data;
