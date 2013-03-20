with Ada.Exceptions;

with Skp.Xml;

package body Xml_Tests is

   use Ahven;
   use Skp;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "XML tests");
      T.Add_Test_Routine
        (Routine => Load_Nonexistent_Xsd'Access,
         Name    => "Load nonexistent XSD");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Xsd'Access,
         Name    => "Load invalid XSD");
      T.Add_Test_Routine
        (Routine => Load_Nonexistent_Xml'Access,
         Name    => "Load nonexistent XML");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Xml'Access,
         Name    => "Load invalid XML");
      T.Add_Test_Routine
        (Routine => Load_Policy'Access,
         Name    => "Load policy");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Xml
   is
      Data    : Skp.Xml.XML_Data_Type;
      Ref_Msg : constant String := "Error reading XML file 'data/invalid' - "
        & "data/invalid:1:1: Non-white space found at top level";
   begin
      Skp.Xml.Parse (Data   => Data,
                     File   => "data/invalid",
                     Schema => "schema/system.xsd");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Invalid_Xml;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Xsd
   is
      Data    : Skp.Xml.XML_Data_Type;
      Ref_Msg : constant String := "Error reading XSD file 'data/invalid' - "
        & "data/invalid:1:1: Non-white space found at top level";
   begin
      Skp.Xml.Parse (Data   => Data,
                     File   => "examples/test_policy1.xml",
                     Schema => "data/invalid");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Invalid_Xsd;

   -------------------------------------------------------------------------

   procedure Load_Nonexistent_Xml
   is
      Data    : Skp.Xml.XML_Data_Type;
      Ref_Msg : constant String
        := "Error reading XML file 'nonexistent' - Could not open nonexistent";
   begin
      Skp.Xml.Parse (Data   => Data,
                     File   => "nonexistent",
                     Schema => "schema/system.xsd");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Nonexistent_Xml;

   -------------------------------------------------------------------------

   procedure Load_Nonexistent_Xsd
   is
      Data    : Skp.Xml.XML_Data_Type;
      Ref_Msg : constant String
        := "Error reading XSD file 'nonexistent' - Could not open nonexistent";
   begin
      Skp.Xml.Parse (Data   => Data,
                     File   => "examples/test_policy1.xml",
                     Schema => "nonexistent");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Nonexistent_Xsd;

   -------------------------------------------------------------------------

   procedure Load_Policy
   is
      Data : Skp.Xml.XML_Data_Type;
   begin
      Skp.Xml.Parse (Data   => Data,
                     File   => "data/test_policy1.xml",
                     Schema => "schema/system.xsd");

      --  Must not raise an exception.

   end Load_Policy;

end Xml_Tests;
