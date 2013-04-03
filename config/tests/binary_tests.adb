with Skc.Subjects;

package body Binary_Tests
is

   use Ahven;
   use Skc;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Binary tests");
      T.Add_Test_Routine
        (Routine => Read_Binary'Access,
         Name    => "Read binary information");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Read_Binary
   is
      use type Skc.Subjects.Binary_Type;

      Ref_Bin : constant Subjects.Binary_Type
        := (Entry_Point   => 16#0c#,
            Stack_Address => 16#18#);
      Bin     : Subjects.Binary_Type;
   begin
      Bin := Subjects.Read (Binary => "data/bin1");

      Assert (Condition => Bin = Ref_Bin,
              Message   => "Binary info mismatch");
   end Read_Binary;

end Binary_Tests;
