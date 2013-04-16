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

         Assert (Condition => P.Hardware.Devices.Length = 3,
                 Message   => "Device count mismatch");

         Assert (Condition => P.Hardware.Processor.Logical_CPUs = 2,
                 Message   => "Logical CPUs mismatch");
         Assert (Condition => P.Hardware.Processor.Speed = 2900,
                 Message   => "Speed mismatch");
         Assert (Condition => P.Hardware.Processor.VMX_Timer_Rate = 5,
                 Message   => "VMX preemption timer rate mismatch");

         declare
            Dev : constant Device_Type := P.Hardware.Devices.First_Element;
         begin
            Assert (Condition => Dev.Name = To_Unbounded_String ("multiport"),
                    Message   => "Device name mismatch");
            Assert (Condition => Dev.Memory_Layout.Is_Empty,
                    Message   => "Device memory not empty");
            Assert (Condition => Dev.IO_Ports.Length = 2,
                    Message   => "Device ports mismatch");
         end;

         --  Kernel

         Assert (Condition => P.Kernel.Stack_Address = 16#112000#,
                 Message   => "Kernel stack address mismatch");
         Assert (Condition => P.Kernel.Pml4_Address = 16#200000#,
                 Message   => "Kernel PML4 address mismatch");
         Assert (Condition => P.Kernel.Memory_Layout.Length = 2,
                 Message   => "Kernel memory region count mismatch");

         --  Binaries

         Assert (Condition => P.Binaries.Length = 3,
                 Message   => "Binary count mismatch");
         Assert (Condition => P.Binaries.First_Element.Path
                 = To_Unbounded_String ("subjects/tau0/obj/tau0"),
                 Message   => "Binary path mismatch");
         Assert (Condition => P.Binaries.First_Element.Physical_Address
                 = 16#216000#,
                 Message   => "Binary address mismatch");

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
            Assert (Condition => Minor_2.Subject_Id = 4,
                    Message   => "Minor frame subject id mismatch (2)");
            Assert (Condition => Minor_2.Ticks = 200,
                    Message   => "Minor frame ticks mismatch (2)");
         end;

         --  Subjects

         Assert (Condition => S.Id = 0,
                 Message   => "Id mismatch");
         Assert (Condition => S.Name = To_Unbounded_String (Source => "tau0"),
                 Message   => "Name mismatch");
         Assert (Condition => S.Pml4_Address = 16#1f0000#,
                 Message   => "PML4 address mismatch");
         Assert (Condition => S.IO_Bitmap_Address = 16#12345678#,
                 Message   => "I/O bitmap address mismatch");

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

         PR := S.IO_Ports.First_Element;
         Assert (Condition => PR.Start_Port = 16#50b0#,
                 Message   => "Port start mismatch");
         Assert (Condition => PR.End_Port = 16#50b0#,
                 Message   => "Port end mismatch");
      end;
   end Xml_To_Policy;

end Xml_Tests;
