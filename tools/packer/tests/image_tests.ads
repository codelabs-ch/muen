with Ahven.Framework;

package Image_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Add section to ELF binary.
   procedure Add_Section_To_Elf;

end Image_Tests;
