with Ahven.Framework;

package Binary_Tests
is

   type Testcase is new Ahven.Framework.Test_Case with null record;

   --  Initialize testcase.
   procedure Initialize (T : in out Testcase);

   --  Read information from a binary file.
   procedure Read_Binary;

   --  Try to read a nonexistent file.
   procedure Read_Nonexistent_File;

   --  Try to read non-binary file.
   procedure Read_Invalid_File;

   --  Try to read a binary file which contains no symbols.
   procedure Read_No_Symbols;

   --  Try to read a binary file which contains no global symbols.
   procedure Read_No_Global_Symbols;

   --  Try to read a binary file which contains undefined symbols.
   procedure Read_Undefined_Symbols;

end Binary_Tests;
