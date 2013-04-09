with Ahven.Framework;

package Xml_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Try to load nonexistent XSD file.
   procedure Load_Nonexistent_Xsd;

   --  Try to load an invalid XSD file.
   procedure Load_Invalid_Xsd;

   --  Try to load nonexistent XML file.
   procedure Load_Nonexistent_Xml;

   --  Try to load a file which is not XML.
   procedure Load_Non_Xml_File;

   --  Try to load policy with invalid subject device reference.
   procedure Load_Invalid_Device;

   --  Load policy from XML file.
   procedure Load_Policy_Xml;

   --  Test XML data to Ada Policy_Type conversion.
   procedure Xml_To_Policy;

   --  Test size string to memory size conversion.
   procedure String_To_Memory_Size;

end Xml_Tests;
