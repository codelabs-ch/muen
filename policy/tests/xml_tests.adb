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
        (Routine => Load_Invalid_Xml'Access,
         Name    => "Load invalid XML");
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

   procedure Load_Invalid_Xml
   is
      Data    : Xml.XML_Data_Type;
      Ref_Msg : constant String := "Error reading XML file 'data/invalid' - "
        & "data/invalid:1:1: Non-white space found at top level";
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/invalid",
                 Schema => "schema/system.xsd");

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
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

   exception
      when E : Xml.Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Invalid_Xsd;

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

         --  Kernel

         Assert (Condition => P.Kernel.Memory_Layout.Pml4_Address = 16#200000#,
                 Message   => "Kernel PML4 address mismatch");
         Assert (Condition => P.Kernel.Memory_Layout.Regions.Length = 2,
                 Message   => "Kernel memory region count mismatch");

         --  Subjects

         Assert (Condition => S.Id = 0,
                 Message   => "Id mismatch");
         Assert (Condition => S.Name = To_Unbounded_String (Source => "tau0"),
                 Message   => "Name mismatch");

         Assert (Condition => S.Memory_Layout.Pml4_Address = 16#1f0000#,
                 Message   => "PML4 address mismatch");
         Assert (Condition => S.Memory_Layout.Regions.Length = 2,
                 Message   => "Memory region count mismatch");

         R := S.Memory_Layout.Regions.First_Element;
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
         R := S.Memory_Layout.Regions.Last_Element;
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

         Assert (Condition => S.IO_Ports.IO_Bitmap_Address = 16#12345678#,
                 Message   => "I/O bitmap address mismatch");

         PR := S.IO_Ports.Ranges.First_Element;
         Assert (Condition => PR.Start_Port = 16#50b0#,
                 Message   => "Port start mismatch");
         Assert (Condition => PR.End_Port = 16#50b0#,
                 Message   => "Port end mismatch");
      end;
   end Xml_To_Policy;

end Xml_Tests;
