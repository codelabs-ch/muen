with Ahven.Framework;

package Templates_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Load template from file.
   procedure Load_Template;

   --  Replace patterns in template.
   procedure Replace_Patterns;

end Templates_Tests;
