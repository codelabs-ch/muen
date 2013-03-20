with Ahven.Framework;

package Xml_Tests is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Try to load nonexistent XSD file.
   procedure Load_Nonexistent_Xsd;

   --  Try to load an invalid XSD file.
   procedure Load_Invalid_Xsd;

   --  Try to load nonexistent XML file.
   procedure Load_Nonexistent_Xml;

   --  Try to load an invalid XML file.
   procedure Load_Invalid_Xml;

   --  Load policy from XML file.
   procedure Load_Policy;

end Xml_Tests;
