with Ada.Exceptions;
with Ada.Strings.Unbounded;

with Skp.Xml;
with Skp.Validators;

package body Validation_Tests
is

   use Ada.Strings.Unbounded;
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
      T.Add_Test_Routine
        (Routine => Invalid_Sched_CPU_Ticks'Access,
         Name    => "Invalid CPU ticks in scheduling plan");
      T.Add_Test_Routine
        (Routine => Invalid_Sched_CPU_Count'Access,
         Name    => "Invalid CPU elements in scheduling plan");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Pml4_Addr'Access,
         Name    => "Invalid subject PML4 address");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_IO_Bitmap_Addr'Access,
         Name    => "Invalid subject I/O bitmap address");
      T.Add_Test_Routine
        (Routine => Invalid_Device_IRQ'Access,
         Name    => "Invalid device IRQ");
      T.Add_Test_Routine
        (Routine => Policy_Validation'Access,
         Name    => "Validate policy");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Invalid_Device_IRQ
   is
      Hw : Hardware_Type;
   begin
      Hw.Devices.Insert
        (Key      => To_Unbounded_String ("d1"),
         New_Item => (Name   => To_Unbounded_String ("d1"),
                      IRQ    => 10,
                      others => <>));
      Hw.Devices.Insert
        (Key      => To_Unbounded_String ("d2"),
         New_Item => (Name   => To_Unbounded_String ("d2"),
                      IRQ    => 10,
                      others => <>));
      Validators.Validate_Hardware (H => Hw);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Device 'd2' IRQ 10 is not unique",
                 Message   => "Exception message mismatch");
   end Invalid_Device_IRQ;

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

   procedure Invalid_Sched_CPU_Count
   is
      Policy : Policy_Type;
      CPU    : CPU_Type;
      Major  : Major_Frame_Type;
   begin
      Policy.Hardware.Processor.Logical_CPUs := 1;

      Major.Append (New_Item => CPU);
      Major.Append (New_Item => CPU);

      Policy.Scheduling.Major_Frames.Append (New_Item => Major);
      Validators.Validate_Scheduling (P => Policy);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid CPU elements in scheduling plan, logical CPU count"
                 & " differs",
                 Message   => "Exception message mismatch");
   end Invalid_Sched_CPU_Count;

   -------------------------------------------------------------------------

   procedure Invalid_Sched_CPU_Ticks
   is
      Policy     : Policy_Type;
      Major      : Major_Frame_Type;
      CPU1, CPU2 : CPU_Type;
   begin
      Policy.Hardware.Processor.Logical_CPUs := 2;

      CPU1.Append (New_Item => (0, 100));
      CPU2.Append (New_Item => (2, 200));
      Major.Append (New_Item => CPU1);
      Major.Append (New_Item => CPU2);
      Policy.Scheduling.Major_Frames.Append (New_Item => Major);

      Validators.Validate_Scheduling (P => Policy);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid CPU elements in scheduling plan, tick counts "
                 & "differ",
                 Message   => "Exception message mismatch");
   end Invalid_Sched_CPU_Ticks;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_IO_Bitmap_Addr
   is
      P : Policy_Type;
   begin
      P.Subjects.Insert
        (New_Item => (Name              => To_Unbounded_String ("s1"),
                      Pml4_Address      => 0,
                      IO_Bitmap_Address => 15,
                      others            => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : others =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid I/O bitmap address 000000000000000f - "
                 & "address must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_IO_Bitmap_Addr;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Pml4_Addr
   is
      P : Policy_Type;
   begin
      P.Subjects.Insert
        (New_Item => (Name         => To_Unbounded_String ("s1"),
                      Pml4_Address => 13,
                      others       => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : others =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid PML4 address 000000000000000d - "
                 & "address must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Pml4_Addr;

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

   -------------------------------------------------------------------------

   procedure Policy_Validation
   is
      D : Xml.XML_Data_Type;
      P : Policy_Type;
   begin
      Xml.Parse (Data   => D,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");
      P := Xml.To_Policy (Data => D);
      Validators.Validate_Policy (P => P);

      --  Must not raise an exception.

   end Policy_Validation;

end Validation_Tests;
