with Ada.Exceptions;

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
      T.Add_Test_Routine
        (Routine => Read_Nonexistent_File'Access,
         Name    => "Read nonexistent file");
      T.Add_Test_Routine
        (Routine => Read_Invalid_File'Access,
         Name    => "Read invalid file");
      T.Add_Test_Routine
        (Routine => Read_No_Symbols'Access,
         Name    => "Read file without symbols");
      T.Add_Test_Routine
        (Routine => Read_No_Global_Symbols'Access,
         Name    => "Read file without global symbols");
      T.Add_Test_Routine
        (Routine => Read_Undefined_Symbols'Access,
         Name    => "Read file with undefined symbols");
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

   -------------------------------------------------------------------------

   procedure Read_Invalid_File
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "data/bin1.S");
      Fail (Message => "Exception expected");

   exception
      when E : Subjects.Binary_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = "File 'data/bin1.S' has invalid format",
                 Message   => "Exception message mismatch");
   end Read_Invalid_File;

   -------------------------------------------------------------------------

   procedure Read_No_Global_Symbols
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "data/noglobalmainsymbol");
      Fail (Message => "Exception expected");

   exception
      when E : Subjects.Binary_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = "main is not a global symbol",
                 Message   => "Exception message mismatch");
   end Read_No_Global_Symbols;

   -------------------------------------------------------------------------

   procedure Read_No_Symbols
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      begin
         Bin := Subjects.Read (Binary => "data/nomainsymbol");
         Fail (Message => "Exception expected");

      exception
         when E : Subjects.Binary_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message
                    (X => E) = "No main symbol",
                    Message   => "Exception message mismatch");
      end;

      begin
         Bin := Subjects.Read (Binary => "data/nostacksymbol");
         Fail (Message => "Exception expected");

      exception
         when E : Subjects.Binary_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message
                    (X => E) = "No stack symbol",
                    Message   => "Exception message mismatch");
      end;
   end Read_No_Symbols;

   -------------------------------------------------------------------------

   procedure Read_Nonexistent_File
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "nonexistent");
      Fail (Message => "Exception expected");

   exception
      when Subjects.Open_Error => null;
   end Read_Nonexistent_File;

   -------------------------------------------------------------------------

   procedure Read_Undefined_Symbols
   is
      Bin : Subjects.Binary_Type;
      pragma Unreferenced (Bin);
   begin
      Bin := Subjects.Read (Binary => "data/undefmain");
      Fail (Message => "Exception expected");

   exception
      when E : Subjects.Binary_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = "Undefined symbol main",
                 Message   => "Exception message mismatch");
   end Read_Undefined_Symbols;

end Binary_Tests;
