with Ada.Exceptions;
with Ada.Strings.Unbounded;
with Ada.Containers;

with SK;

with Skp.Xml;
with Skp.Xml.Util;

package body Xml_Tests
is

   use Ahven;
   use Skp;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "XML tests");
      T.Add_Test_Routine
        (Routine => Load_Nonexistent_Xsd'Access,
         Name    => "Load nonexistent XSD");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Xsd'Access,
         Name    => "Load invalid XSD");
      T.Add_Test_Routine
        (Routine => Load_Nonexistent_Xml'Access,
         Name    => "Load nonexistent XML");
      T.Add_Test_Routine
        (Routine => Load_Non_Xml_File'Access,
         Name    => "Load non-XML file");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Xml'Access,
         Name    => "Load invalid XML file");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Device'Access,
         Name    => "Load invalid subject device");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Trap_Table'Access,
         Name    => "Load invalid subject trap table");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Signal_Table'Access,
         Name    => "Load invalid subject signal table");
      T.Add_Test_Routine
        (Routine => Load_Policy_Xml'Access,
         Name    => "Load policy from XML");
      T.Add_Test_Routine
        (Routine => Xml_To_Policy'Access,
         Name    => "Deserialize Ada policy type");
      T.Add_Test_Routine
        (Routine => String_To_Memory_Size'Access,
         Name    => "Convert string to memory size");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Device
   is
      D : Xml.XML_Data_Type;
      P : Policy_Type;
      pragma Unreferenced (P);
   begin
      Xml.Parse (Data   => D,
                 File   => "data/invalid_device_ref.xml",
                 Schema => "schema/system.xsd");

      P := Xml.To_Policy (Data => D);
      Fail (Message => "Exception expected");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject tau0: No hardware device with name 'nonexistent'",
                 Message   => "Exception message mismatch");
   end Load_Invalid_Device;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Signal_Table
   is
      D : Xml.XML_Data_Type;
      P : Policy_Type;
      pragma Unreferenced (P);
   begin
      Xml.Parse (Data   => D,
                 File   => "data/invalid_signal_table_ref.xml",
                 Schema => "schema/system.xsd");

      P := Xml.To_Policy (Data => D);
      Fail (Message => "Exception expected");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject tau0: Duplicate entry for signal 2",
                 Message   => "Exception message mismatch");
   end Load_Invalid_Signal_Table;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Trap_Table
   is
      D : Xml.XML_Data_Type;
      P : Policy_Type;
      pragma Unreferenced (P);
   begin
      Xml.Parse (Data   => D,
                 File   => "data/invalid_trap_table_ref.xml",
                 Schema => "schema/system.xsd");

      P := Xml.To_Policy (Data => D);
      Fail (Message => "Exception expected");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                 = "Subject tau0: Duplicate trap entry for 'exception_or_nmi'",
                 Message   => "Exception message mismatch");
   end Load_Invalid_Trap_Table;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Xml
   is
      Data : Xml.XML_Data_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/invalid.xml",
                 Schema => "schema/system.xsd");
      Fail (Message => "Exception expected");

   exception
      when Xml.Processing_Error => null;
   end Load_Invalid_Xml;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Xsd
   is
      Data    : Xml.XML_Data_Type;
      Ref_Msg : constant String := "Error reading XSD file 'data/invalid' - "
        & "data/invalid:1:1: Non-white space found at top level";
   begin
      Xml.Parse (Data   => Data,
                 File   => "examples/test_policy1.xml",
                 Schema => "data/invalid");
      Fail (Message => "Exception expected");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Invalid_Xsd;

   -------------------------------------------------------------------------

   procedure Load_Non_Xml_File
   is
      Data    : Xml.XML_Data_Type;
      Ref_Msg : constant String := "Error reading XML file 'data/invalid' - "
        & "data/invalid:1:1: Non-white space found at top level";
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/invalid",
                 Schema => "schema/system.xsd");
      Fail (Message => "Exception expected");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Non_Xml_File;

   -------------------------------------------------------------------------

   procedure Load_Nonexistent_Xml
   is
      Data    : Xml.XML_Data_Type;
      Ref_Msg : constant String
        := "Error reading XML file 'nonexistent' - Could not open nonexistent";
   begin
      Xml.Parse (Data   => Data,
                 File   => "nonexistent",
                 Schema => "schema/system.xsd");
      Fail (Message => "Exception expected");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Nonexistent_Xml;

   -------------------------------------------------------------------------

   procedure Load_Nonexistent_Xsd
   is
      Data    : Xml.XML_Data_Type;
      Ref_Msg : constant String
        := "Error reading XSD file 'nonexistent' - Could not open nonexistent";
   begin
      Xml.Parse (Data   => Data,
                 File   => "examples/test_policy1.xml",
                 Schema => "nonexistent");
      Fail (Message => "Exception expected");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Nonexistent_Xsd;

   -------------------------------------------------------------------------

   procedure Load_Policy_Xml
   is
      Data : Xml.XML_Data_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      --  Must not raise an exception.

   end Load_Policy_Xml;

   -------------------------------------------------------------------------

   procedure String_To_Memory_Size
   is
      use type SK.Word64;
   begin
      Assert (Condition => Xml.Util.To_Memory_Size (Str => "4k") = 4096,
              Message   => "Size mismatch (1)");
      Assert (Condition => Xml.Util.To_Memory_Size (Str => "1K") = 1024,
              Message   => "Size mismatch (2)");
      Assert (Condition => Xml.Util.To_Memory_Size (Str => "4m") = 4194304,
              Message   => "Size mismatch (3)");
      Assert (Condition => Xml.Util.To_Memory_Size (Str => "1M") = 1048576,
              Message   => "Size mismatch (4)");
      Assert (Condition => Xml.Util.To_Memory_Size (Str => "1g") = 1073741824,
              Message   => "Size mismatch (5)");
      Assert (Condition => Xml.Util.To_Memory_Size (Str => "2G") = 2147483648,
              Message   => "Size mismatch (6)");

      declare
         Dummy : SK.Word64;
         pragma Unreferenced (Dummy);
      begin
         begin
            Dummy := Xml.Util.To_Memory_Size (Str => "abc");
            Fail (Message => "Exception expected");

         exception
            when Xml.Util.Conversion_Error => null;
         end;

         begin
            Dummy := Xml.Util.To_Memory_Size (Str => "12f");
            Fail (Message => "Exception expected");

         exception
            when Xml.Util.Conversion_Error => null;
         end;
      end;
   end String_To_Memory_Size;

   -------------------------------------------------------------------------

   procedure Xml_To_Policy
   is
      use Ada.Strings.Unbounded;
      use type Ada.Containers.Count_Type;

      D : Xml.XML_Data_Type;
      P : Policy_Type;
   begin
      Xml.Parse (Data   => D,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");
      P := Xml.To_Policy (Data => D);
      Assert (Condition => P.Subjects.Length = 3,
              Message   => "Subject count mismatch");

      declare
         use type SK.Word16;
         use type SK.Word32;
         use type SK.Word64;

         S  : constant Subject_Type := P.Subjects.First_Element;
         R  : Memory_Region_Type;
         PR : IO_Port_Range;
      begin

         --  System

         Assert (Condition => P.Vmxon_Address = 0,
                 Message   => "VMXON address mismatch");
         Assert (Condition => P.Vmcs_Start_Address = 16#1000#,
                 Message   => "VMCS start address mismatch");

         --  Hardware

         Assert (Condition => P.Hardware.Devices.Length = 4,
                 Message   => "Device count mismatch");

         Assert (Condition => P.Hardware.Processor.Logical_CPUs = 2,
                 Message   => "Logical CPUs mismatch");
         Assert (Condition => P.Hardware.Processor.Speed = 2900,
                 Message   => "Speed mismatch");
         Assert (Condition => P.Hardware.Processor.VMX_Timer_Rate = 5,
                 Message   => "VMX preemption timer rate mismatch");

         declare
            Dev1 : constant Device_Type := P.Hardware.Devices.First_Element;
            Dev2 : constant Device_Type := P.Hardware.Devices.Last_Element;
         begin
            Assert (Condition => Dev1.Name = To_Unbounded_String ("multiport"),
                    Message   => "Device name mismatch");
            Assert (Condition => Dev1.Memory_Layout.Is_Empty,
                    Message   => "Device memory not empty");
            Assert (Condition => Dev1.IO_Ports.Length = 2,
                    Message   => "Device ports mismatch");
            Assert (Condition => Dev1.IRQ = 12,
                    Message   => "Device IRQ mismatch (1)");
            Assert (Condition => Dev1.Owners.Length = 1,
                    Message   => "Device owners count mismatch (1)");
            Assert (Condition => Dev2.IRQ = -1,
                    Message   => "Device IRQ mismatch (2)");
            Assert (Condition => Dev2.Owners.Length = 1,
                    Message   => "Device owners count mismatch (2)");
            Assert (Condition => Dev2.Owners.First_Element = 1,
                    Message   => "Device owner mismatch");
         end;

         --  Kernel

         Assert (Condition => P.Kernel.Stack_Address = 16#112000#,
                 Message   => "Kernel stack address mismatch");
         Assert (Condition => P.Kernel.Pml4_Address = 16#200000#,
                 Message   => "Kernel PML4 address mismatch");
         Assert (Condition => P.Kernel.CPU_Page_Address = 16#114000#,
                 Message   => "Kernel CPU storage address mismatch");
         Assert (Condition => P.Kernel.Memory_Layout.Length = 3,
                 Message   => "Kernel memory region count mismatch");

         --  Binaries

         declare
            Name : constant Unbounded_String := To_Unbounded_String ("tau0");
            Path : constant Unbounded_String := To_Unbounded_String
              ("subjects/tau0/obj/tau0");
         begin
            Assert (Condition => P.Binaries.Length = 3,
                    Message   => "Binary count mismatch");
            Assert (Condition => P.Binaries.Element (Key => Name) = Path,
                    Message   => "Binary path mismatch");
         end;

         --  Scheduling

         Assert (Condition => P.Scheduling.Tick_Rate = 1000,
                 Message   => "Tick rate mismatch");

         Assert (Condition => P.Scheduling.Major_Frames.Length = 2,
                 Message   => "Major frame count mismatch");
         Assert
           (Condition => P.Scheduling.Major_Frames.Last_Element.Length = 2,
            Message   => "CPU element count mismatch");

         declare
            CPU_0   : constant CPU_Type
              := P.Scheduling.Major_Frames.Last_Element.First_Element;
            Minor_1 : constant Minor_Frame_Type := CPU_0.First_Element;
            Minor_2 : constant Minor_Frame_Type := CPU_0.Last_Element;
         begin
            Assert (Condition => CPU_0.Length = 4,
                    Message   => "Minor frame count mismatch");
            Assert (Condition => Minor_1.Subject_Id = 0,
                    Message   => "Minor frame subject id mismatch (1)");
            Assert (Condition => Minor_1.Ticks = 500,
                    Message   => "Minor frame ticks mismatch (1)");
            Assert (Condition => Minor_2.Subject_Id = 2,
                    Message   => "Minor frame subject id mismatch (2)");
            Assert (Condition => Minor_2.Ticks = 200,
                    Message   => "Minor frame ticks mismatch (2)");
         end;

         --  Subjects

         Assert (Condition => S.Id = 0,
                 Message   => "Id mismatch");
         Assert (Condition => S.Name = "tau0",
                 Message   => "Name mismatch");
         Assert (Condition => S.CPU = 0,
                 Message   => "CPU mismatch");
         Assert (Condition => S.Pml4_Address = 16#1f0000#,
                 Message   => "PML4 address mismatch");
         Assert (Condition => S.IO_Bitmap_Address = 16#1a0000#,
                 Message   => "I/O bitmap address mismatch");
         Assert (Condition => S.MSR_Bitmap_Address = 16#900000#,
                 Message   => "MSR bitmap address mismatch");

         Assert (Condition => S.Init_State.Stack_Address = 16#120000#,
                 Message   => "Subject stack mismatch");
         Assert (Condition => S.Init_State.Entry_Point = 16#abc#,
                 Message   => "Subject RIP mismatch");

         Assert (Condition => S.Memory_Layout.Length = 2,
                 Message   => "Memory region count mismatch");

         R := S.Memory_Layout.First_Element;
         Assert (Condition => R.Physical_Address = 16#240000#,
                 Message   => "Physical address mismatch (1)");
         Assert (Condition => R.Virtual_Address = 0,
                 Message   => "Virtual address mismatch (1)");
         Assert (Condition => R.Size = 65536,
                 Message   => "Memory size mismatch (1)");
         Assert (Condition => R.Alignment = 4096,
                 Message   => "Memory alignment mismatch (1)");
         Assert (Condition => R.Writable,
                 Message   => "Writable mismatch (1)");
         Assert (Condition => R.Executable,
                 Message   => "Executable mismatch (1)");
         Assert (Condition => R.Memory_Type = UC,
                 Message   => "Memory type mismatch (1)");

         R := S.Memory_Layout.Last_Element;
         Assert (Condition => R.Physical_Address = 16#100000#,
                 Message   => "Physical address mismatch (2)");
         Assert (Condition => R.Virtual_Address = 16#100000#,
                 Message   => "Virtual address mismatch (2)");
         Assert (Condition => R.Size = 4096,
                 Message   => "Memory size mismatch (2)");
         Assert (Condition => not R.Writable,
                 Message   => "Writable mismatch (2)");
         Assert (Condition => not R.Executable,
                 Message   => "Executable mismatch (2)");
         Assert (Condition => R.Memory_Type = WC,
                 Message   => "Memory type mismatch (2)");

         PR := S.IO_Ports.First_Element;
         Assert (Condition => PR.Start_Port = 16#50b0#,
                 Message   => "Port start mismatch");
         Assert (Condition => PR.End_Port = 16#50b0#,
                 Message   => "Port end mismatch");

         --  Subject MSRs

         Assert (Condition => S.MSRs.Length = 2,
                 Message   => "MSR length mismatch");

         declare
            Msr : constant MSR_Type := S.MSRs.First_Element;
         begin
            Assert (Condition => Msr.Start_Addr = 16#800#,
                    Message   => "MSR start address mismatch");
            Assert (Condition => Msr.End_Addr = 16#802#,
                    Message   => "MSR end address mismatch");
            Assert (Condition => Msr.Mode = MSR_Read,
                    Message   => "MSR mode mismatch");
         end;

         --  Subject traps

         Assert (Condition => S.Trap_Table.Length = 2,
                 Message   => "Trap count mismatch");
         Assert (Condition => S.Trap_Table.First_Element.Kind
                 = Exception_Or_NMI,
                 Message   => "Trap type mismatch");
         Assert (Condition => S.Trap_Table.First_Element.Dst_Subject
                 = "subject2",
                 Message   => "Trap dst subject mismatch");
         Assert (Condition => S.Trap_Table.First_Element.Dst_Vector = 256,
                 Message   => "Trap dst vector mismatch (1)");
         Assert (Condition => S.Trap_Table.Last_Element.Dst_Vector = 12,
                 Message   => "Trap dst vector mismatch (2)");

         --  Subject signals

         Assert (Condition => S.Signal_Table.Length = 2,
                 Message   => "Signal count mismatch");
         Assert (Condition => S.Signal_Table.First_Element.Kind
                 = Asynchronous,
                 Message   => "Signal kind mismatch");
         Assert (Condition => S.Signal_Table.First_Element.Signal = 17,
                 Message   => "Signal mismatch");
         Assert (Condition => S.Signal_Table.First_Element.Dst_Subject
                 = "subject1",
                 Message   => "Signal dst subject mismatch");
         Assert (Condition => S.Signal_Table.First_Element.Dst_Vector = 32,
                 Message   => "Signal dst vector mismatch (1)");
         Assert (Condition => S.Signal_Table.Last_Element.Dst_Vector = 256,
                 Message   => "Signal dst vector mismatch (2)");
      end;
   end Xml_To_Policy;

end Xml_Tests;
