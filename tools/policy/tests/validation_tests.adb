--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--
--  This program is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

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
        (Routine => Invalid_Knl_Pml4_Addr'Access,
         Name    => "Invalid kernel PML4 address");
      T.Add_Test_Routine
        (Routine => Invalid_Knl_Stack_Addr'Access,
         Name    => "Invalid kernel stack address");
      T.Add_Test_Routine
        (Routine => Invalid_Knl_CPU_Page_Addr'Access,
         Name    => "Invalid kernel CPU page address");
      T.Add_Test_Routine
        (Routine => Invalid_Sched_CPU_Ticks'Access,
         Name    => "Invalid CPU ticks in scheduling plan");
      T.Add_Test_Routine
        (Routine => Invalid_Sched_CPU_Count'Access,
         Name    => "Invalid CPU elements in scheduling plan");
      T.Add_Test_Routine
        (Routine => Invalid_Sched_Subject'Access,
         Name    => "Invalid subject in scheduling plan");
      T.Add_Test_Routine
        (Routine => Invalid_Sched_Subject_CPU'Access,
         Name    => "Invalid subject CPU in scheduling plan");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Pml4_Addr'Access,
         Name    => "Invalid subject PML4 address");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_IO_Bitmap_Addr'Access,
         Name    => "Invalid subject I/O bitmap address");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_CPU'Access,
         Name    => "Invalid subject CPU number");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Binary'Access,
         Name    => "Invalid subject binary");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Trap_Self_Ref'Access,
         Name    => "Invalid subject trap table self-reference");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Trap_Dst'Access,
         Name    => "Invalid subject trap table entry dst");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Trap_Dst_CPU'Access,
         Name    => "Invalid subject trap table entry dst CPU");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Event_Self_Ref'Access,
         Name    => "Invalid subject event table self-reference");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Event_Dst'Access,
         Name    => "Invalid subject event table entry dst");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Event_Dst_CPU'Access,
         Name    => "Invalid subject event entry dst CPU");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_Event_IPI_Dst_CPU'Access,
         Name    => "Invalid subject IPI event dst CPU");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_MSR'Access,
         Name    => "Invalid subject MSR");
      T.Add_Test_Routine
        (Routine => Invalid_Subj_MSR_Bitmap_Addr'Access,
         Name    => "Invalid subject MSR bitmap address");
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

   procedure Invalid_Knl_CPU_Page_Addr
   is
      K : constant Kernel_Type := (Pml4_Address     => 0,
                                   Stack_Address    => 0,
                                   CPU_Page_Address => 13,
                                   others           => <>);
   begin
      Validators.Validate_Kernel (K => K);

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid kernel CPU page address 000000000000000d - address"
                 & " must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Knl_CPU_Page_Addr;

   -------------------------------------------------------------------------

   procedure Invalid_Knl_Pml4_Addr
   is
      K : constant Kernel_Type := (Pml4_Address => 13,
                                   others       => <>);
   begin
      Validators.Validate_Kernel (K => K);

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid kernel PML4 address 000000000000000d - address "
                 & "must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Knl_Pml4_Addr;

   -------------------------------------------------------------------------

   procedure Invalid_Knl_Stack_Addr
   is
      K : constant Kernel_Type := (Pml4_Address  => 0,
                                   Stack_Address => 13,
                                   others        => <>);
   begin
      Validators.Validate_Kernel (K => K);

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid kernel stack address 000000000000000d - address "
                 & "must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Knl_Stack_Addr;

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
      Policy.Subjects.Insert
        (New_Item =>
           (Id     => 0,
            Name   => To_Unbounded_String ("s1"),
            CPU    => 0,
            others => <>));
      Policy.Subjects.Insert
        (New_Item =>
           (Id     => 2,
            Name   => To_Unbounded_String ("s2"),
            CPU    => 1,
            others => <>));

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

   procedure Invalid_Sched_Subject
   is
      Policy : Policy_Type;
      Major  : Major_Frame_Type;
      CPU    : CPU_Type;
   begin
      Policy.Hardware.Processor.Logical_CPUs := 1;

      CPU.Append (New_Item => (0, 100));
      Major.Append (New_Item => CPU);
      Policy.Scheduling.Major_Frames.Append (New_Item => Major);

      Validators.Validate_Scheduling (P => Policy);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid scheduling plan: Reference to undefined subject 0",
                 Message   => "Exception message mismatch");
   end Invalid_Sched_Subject;

   -------------------------------------------------------------------------

   procedure Invalid_Sched_Subject_CPU
   is
      Policy : Policy_Type;
      Major  : Major_Frame_Type;
      CPU    : CPU_Type;
   begin
      Policy.Hardware.Processor.Logical_CPUs := 1;
      Policy.Subjects.Insert
        (New_Item =>
           (Id     => 0,
            Name   => To_Unbounded_String ("s1"),
            CPU    => 1,
            others => <>));

      CPU.Append (New_Item => (0, 100));
      Major.Append (New_Item => CPU);
      Policy.Scheduling.Major_Frames.Append (New_Item => Major);

      Validators.Validate_Scheduling (P => Policy);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Invalid scheduling plan: Subject 0 scheduled on wrong CPU "
                 & "1, should be 0",
                 Message   => "Exception message mismatch");
   end Invalid_Sched_Subject_CPU;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Binary
   is
      P : Policy_Type;
   begin
      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("s2"),
                                   others => 0),
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Referenced binary 's2' not found in policy",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Binary;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_CPU
   is
      P : Policy_Type;
   begin
      P.Hardware.Processor.Logical_CPUs := 3;
      P.Subjects.Insert
        (New_Item => (Name               => To_Unbounded_String ("s1"),
                      CPU                => 3,
                      Pml4_Address       => 0,
                      IO_Bitmap_Address  => 0,
                      MSR_Bitmap_Address => 0,
                      others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid CPU specified - 3 given, must be in "
                 & "range 0 .. 2",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_CPU;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Event_Dst
   is
      E_Table : Event_Table_Type;
      P       : Policy_Type;
   begin
      E_Table.Insert (Key      => 0,
                      New_Item => (Event_Nr    => 0,
                                   Dst_Subject => To_Unbounded_String ("xy"),
                                   Dst_Vector  => 12,
                                   Handover    => True,
                                   Send_IPI    => False));
      P.Binaries.Insert
        (Key      => To_Unbounded_String ("s2"),
         New_Item => (Path   => To_Unbounded_String ("path/to/s2"),
                      Format => Elf));

      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("s2"),
                                   others => 0),
            Event_Table        => E_Table,
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Undefined destination subject 'xy' in "
                 & "event table entry 0",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Event_Dst;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Event_Dst_CPU
   is
      E_Table : Event_Table_Type;
      P       : Policy_Type;
   begin
      E_Table.Insert (Key      => 0,
                      New_Item => (Event_Nr    => 0,
                                   Dst_Subject => To_Unbounded_String ("s2"),
                                   Dst_Vector  => 12,
                                   Handover    => True,
                                   Send_IPI    => False));
      P.Binaries.Insert
        (Key      => To_Unbounded_String ("bin"),
         New_Item => (Path   => To_Unbounded_String ("path/to/bin"),
                      Format => Elf));

      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Id                 => 0,
            Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("bin"),
                                   others => 0),
            Event_Table        => E_Table,
            others             => <>));
      P.Subjects.Insert
        (New_Item =>
           (Id                 => 1,
            Name               => To_Unbounded_String ("s2"),
            CPU                => 1,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("bin"),
                                   others => 0),
            Event_Table        => E_Table,
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid destination subject 's2' in handover "
                 & "event - runs on different CPU",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Event_Dst_CPU;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Event_IPI_Dst_CPU
   is
      E_Table : Event_Table_Type;
      P       : Policy_Type;
   begin
      E_Table.Insert (Key      => 0,
                      New_Item => (Event_Nr    => 0,
                                   Dst_Subject => To_Unbounded_String ("s2"),
                                   Dst_Vector  => 42,
                                   Handover    => False,
                                   Send_IPI    => True));
      P.Binaries.Insert
        (Key      => To_Unbounded_String ("bin"),
         New_Item => (Path   => To_Unbounded_String ("path/to/bin"),
                      Format => Elf));

      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Id                 => 0,
            Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("bin"),
                                   others => 0),
            Event_Table        => E_Table,
            others             => <>));
      P.Subjects.Insert
        (New_Item =>
           (Id                 => 1,
            Name               => To_Unbounded_String ("s2"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("bin"),
                                   others => 0),
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid destination subject 's2' in "
                 & "interrupt event - runs on same CPU, no IPI allowed",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Event_IPI_Dst_CPU;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Event_Self_Ref
   is
      E_Table : Event_Table_Type;
      P       : Policy_Type;
   begin
      E_Table.Insert (Key      => 0,
                      New_Item => (Event_Nr    => 0,
                                   Dst_Subject => To_Unbounded_String ("s1"),
                                   Dst_Vector  => 12,
                                   Handover    => True,
                                   Send_IPI    => False));
      P.Binaries.Insert
        (Key      => To_Unbounded_String ("s2"),
         New_Item => (Path   => To_Unbounded_String ("path/to/s2"),
                      Format => Elf));

      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("s2"),
                                   others => 0),
            Event_Table        => E_Table,
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Reference to self in event table entry 0",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Event_Self_Ref;

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
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid I/O bitmap address 000000000000000f - "
                 & "address must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_IO_Bitmap_Addr;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_MSR
   is
   begin
      begin
         Validators.Validate_MSR (M => (Start_Addr => 16#12#,
                                        End_Addr   => 16#1#,
                                        Mode       => MSR_Read));
         Fail (Message => "Exception expected (1)");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid MSR - start address 00000012 bigger than end "
                    & "address 00000001",
                    Message   => "Exception message mismatch (1)");
      end;

      begin
         Validators.Validate_MSR (M => (Start_Addr => 16#2000#,
                                        End_Addr   => 16#c0000000#,
                                        Mode       => MSR_Read));
         Fail (Message => "Exception expected (2)");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid MSR - start address 00002000 not in valid MSR"
                    & " address range",
                    Message   => "Exception message mismatch (2)");
      end;

      begin
         Validators.Validate_MSR (M => (Start_Addr => 16#0#,
                                        End_Addr   => 16#2000#,
                                        Mode       => MSR_Read));
         Fail (Message => "Exception expected (3)");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid MSR - end address 00002000 not in valid MSR"
                    & " address range",
                    Message   => "Exception message mismatch (3)");
      end;

      begin
         Validators.Validate_MSR (M => (Start_Addr => 16#0#,
                                        End_Addr   => 16#c0000000#,
                                        Mode       => MSR_Read));
         Fail (Message => "Exception expected (4)");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Invalid MSR - start address 00000000 and end address "
                    & "c0000000 not in same address range (low/high)",
                    Message   => "Exception message mismatch (4)");
      end;
   end Invalid_Subj_MSR;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_MSR_Bitmap_Addr
   is
      P : Policy_Type;
   begin
      P.Subjects.Insert
        (New_Item => (Name               => To_Unbounded_String ("s1"),
                      Pml4_Address       => 0,
                      IO_Bitmap_Address  => 0,
                      MSR_Bitmap_Address => 15,
                      others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid MSR bitmap address 000000000000000f - "
                 & "address must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_MSR_Bitmap_Addr;

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
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid PML4 address 000000000000000d - "
                 & "address must be 4k aligned",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Pml4_Addr;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Trap_Dst
   is
      T_Table : Trap_Table_Type;
      P       : Policy_Type;
   begin
      T_Table.Insert (Key      => Exception_Or_NMI,
                      New_Item => (Kind        => Exception_Or_NMI,
                                   Dst_Subject => To_Unbounded_String ("xy"),
                                   Dst_Vector  => 12));
      P.Binaries.Insert
        (Key      => To_Unbounded_String ("s2"),
         New_Item => (Path   => To_Unbounded_String ("path/to/s2"),
                      Format => Elf));

      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("s2"),
                                   others => 0),
            Trap_Table         => T_Table,
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Undefined destination subject 'xy' in trap "
                 & "table entry EXCEPTION_OR_NMI",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Trap_Dst;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Trap_Dst_CPU
   is
      T_Table : Trap_Table_Type;
      P       : Policy_Type;
   begin
      T_Table.Insert (Key      => Exception_Or_NMI,
                      New_Item => (Kind        => Exception_Or_NMI,
                                   Dst_Subject => To_Unbounded_String ("s2"),
                                   Dst_Vector  => 12));
      P.Binaries.Insert
        (Key      => To_Unbounded_String ("bin"),
         New_Item => (Path   => To_Unbounded_String ("path/to/bin"),
                      Format => Elf));

      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Id                 => 0,
            Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("bin"),
                                   others => 0),
            Trap_Table         => T_Table,
            others             => <>));
      P.Subjects.Insert
        (New_Item =>
           (Id                 => 1,
            Name               => To_Unbounded_String ("s2"),
            CPU                => 1,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("bin"),
                                   others => 0),
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Invalid destination subject 's2' in trap table"
                 & " - runs on different CPU",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Trap_Dst_CPU;

   -------------------------------------------------------------------------

   procedure Invalid_Subj_Trap_Self_Ref
   is
      T_Table : Trap_Table_Type;
      P       : Policy_Type;
   begin
      T_Table.Insert (Key      => Exception_Or_NMI,
                      New_Item => (Kind        => Exception_Or_NMI,
                                   Dst_Subject => To_Unbounded_String ("s1"),
                                   Dst_Vector  => 12));
      P.Binaries.Insert
        (Key      => To_Unbounded_String ("s2"),
         New_Item => (Path   => To_Unbounded_String ("path/to/s2"),
                      Format => Elf));

      P.Hardware.Processor.Logical_CPUs := 1;
      P.Subjects.Insert
        (New_Item =>
           (Name               => To_Unbounded_String ("s1"),
            CPU                => 0,
            Pml4_Address       => 0,
            IO_Bitmap_Address  => 0,
            MSR_Bitmap_Address => 0,
            Binary             => (Name   => To_Unbounded_String ("s2"),
                                   others => 0),
            Trap_Table         => T_Table,
            others             => <>));
      Validators.Validate_Subjects (P => P);
      Fail (Message => "Exception expected");

   exception
      when E : Validators.Validation_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject s1: Reference to self in trap table entry "
                 & "EXCEPTION_OR_NMI",
                 Message   => "Exception message mismatch");
   end Invalid_Subj_Trap_Self_Ref;

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
