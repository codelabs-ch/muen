--  This package is intended to set up and tear down  the test environment.
--  Once created by GNATtest, this package will never be overwritten
--  automatically. Contents of this package can be modified in any way
--  except for sections surrounded by a 'read only' marker.

with Mutools.System_Config;

with Expanders.Kernel;
with Expanders.Memory;

package body Expanders.Device_Domains.Test_Data is

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

   procedure Add_Section_Skeleton_And_Kernel
     (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Section_Skeleton (Data => Data);
      Kernel.Add_Section_Skeleton (Data => Data);
   end Add_Section_Skeleton_And_Kernel;

   -------------------------------------------------------------------------

   procedure Add_Section_Skeleton_And_RMRRs (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Memory.Add_Reserved_Memory_Regions (Data => Data);
      Add_Section_Skeleton (Data => Data);
   end Add_Section_Skeleton_And_RMRRs;

   -------------------------------------------------------------------------

   procedure Prepare_Dev_Domain_Without_Mem (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Add_Section_Skeleton_And_RMRRs (Data => Data);
      Muxml.Utils.Remove_Child
        (Node       => Muxml.Utils.Get_Element
           (Doc   => Data.Doc,
            XPath => "/system/deviceDomains/domain[@name='nic1_domain']"),
         Child_Name => "memory");
   end Prepare_Dev_Domain_Without_Mem;

   -------------------------------------------------------------------------

   procedure Remove_Device_Domains
     (Data : in out Muxml.XML_Data_Type)
   is
   begin
      Muxml.Utils.Remove_Child
        (Node       => DOM.Core.Documents.Get_Element (Doc => Data.Doc),
         Child_Name => "deviceDomains");
   end Remove_Device_Domains;

end Expanders.Device_Domains.Test_Data;
