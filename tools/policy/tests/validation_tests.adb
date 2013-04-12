with Ada.Exceptions;

with Skp.Validators;

package body Validation_Tests
is

   use Ahven;
   use Skp;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Validation tests");
      T.Add_Test_Routine
        (Routine => Invalid_Memregion_Size'Access,
         Name    => "Invalid memory region size");
      T.Add_Test_Routine
        (Routine => Invalid_Memregion_Addrs'Access,
         Name    => "Invalid memory region addresses");
      T.Add_Test_Routine
        (Routine => Invalid_Vmxon_Addrs'Access,
         Name    => "Invalid VMXON addresses");
      T.Add_Test_Routine
        (Routine => Invalid_Vmcs_Addrs'Access,
         Name    => "Invalid VMCS addresses");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Invalid_Memregion_Addrs
   is
   begin
      declare
         R : constant Memory_Region_Type
           := (Physical_Address => 16#0023#,
               Size             => 16#1000#,
               Alignment        => 16#1000#,
               others           => <>);
      begin
         Validators.Validate_Mem_Region (R => R);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid memory region physical address 0000000000000023"
                    & " for specified alignment 0000000000001000",
                    Message   => "Exception message mismatch");
      end;

      declare
         R : constant Memory_Region_Type
           := (Physical_Address => 16#0000#,
               Virtual_Address  => 16#0028#,
               Size             => 16#1000#,
               Alignment        => 16#1000#,
               others           => <>);
      begin
         Validators.Validate_Mem_Region (R => R);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid memory region virtual address 0000000000000028 "
                    & "for specified alignment 0000000000001000",
                    Message   => "Exception message mismatch");
      end;
   end Invalid_Memregion_Addrs;

   -------------------------------------------------------------------------

   procedure Invalid_Memregion_Size
   is
      R : constant Memory_Region_Type
        := (Size      => 16#010000#,
            Alignment => 16#200000#,
            others    => <>);
   begin
      Validators.Validate_Mem_Region (R => R);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid memory region size 0000000000010000 for specified "
                 & "alignment 0000000000200000",
                 Message   => "Exception message mismatch");
   end Invalid_Memregion_Size;

   -------------------------------------------------------------------------

   procedure Invalid_Vmcs_Addrs
   is
   begin
      declare
         P : Policy_Type;
      begin
         P.Vmxon_Address      := 0;
         P.Vmcs_Start_Address := 16#23#;
         Validators.Validate_Policy (P => P);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid VMCS start address 0000000000000023 - address "
                    & "must be 4k aligned",
                    Message   => "Exception message mismatch (alignment)");
      end;

      declare
         P : Policy_Type;
      begin
         P.Vmxon_Address      := 0;
         P.Vmcs_Start_Address := 16#110000#;
         P.Subjects.Insert (New_Item => (others => <>));

         Validators.Validate_Policy (P => P);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid VMCS start address 0000000000110000 - address "
                    & "must be below 1m - 4k * 1",
                    Message   => "Exception message mismatch (lowmem)");
      end;
   end Invalid_Vmcs_Addrs;

   -------------------------------------------------------------------------

   procedure Invalid_Vmxon_Addrs
   is
   begin
      declare
         P : Policy_Type;
      begin
         P.Vmxon_Address := 16#42#;
         Validators.Validate_Policy (P => P);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid VMXON address 0000000000000042 - address must "
                    & "be 4k aligned",
                    Message   => "Exception message mismatch (alignment)");
      end;

      declare
         P : Policy_Type;
      begin
         P.Vmxon_Address := 16#200000#;
         Validators.Validate_Policy (P => P);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid VMXON address 0000000000200000 - address must "
                    & "be below 1m",
                    Message   => "Exception message mismatch (lowmem)");
      end;
   end Invalid_Vmxon_Addrs;

end Validation_Tests;
