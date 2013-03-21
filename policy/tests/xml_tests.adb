with Ada.Exceptions;
with Ada.Strings.Unbounded;

with SK;

with Skp.Xml;
with Skp.Xml.Util;
with Skp.Test;

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
      T.Add_Test_Routine
        (Routine => String_To_Permission'Access,
         Name    => "Convert string to permission");
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

   procedure String_To_Permission
   is
      Dummy : Memory_Permission_Type;
      pragma Unreferenced (Dummy);
   begin
      Assert (Condition => Xml.Util.To_Permission (Str => "ro") = Read_Only,
              Message   => "Read_Only expected");
      Assert (Condition => Xml.Util.To_Permission (Str => "rw") = Read_Write,
              Message   => "Read_Write expected");

      begin
         Dummy := Xml.Util.To_Permission (Str => "perm");
         Fail (Message => "Exception expected");

      exception
         when Xml.Util.Conversion_Error => null;
      end;
   end String_To_Permission;

   -------------------------------------------------------------------------

   procedure Xml_To_Policy
   is
      use Ada.Strings.Unbounded;

      D : Xml.XML_Data_Type;
      P : Policy_Type;

      Iterate_Count : Natural := 0;

      --  Increment iteration counter.
      procedure Inc_Iterate_Counter (S : Subject_Type);

      procedure Inc_Iterate_Counter (S : Subject_Type)
      is
         pragma Unreferenced (S);
      begin
         Iterate_Count := Iterate_Count + 1;
      end Inc_Iterate_Counter;
   begin
      Xml.Parse (Data   => D,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");
      P := Xml.To_Policy (Data => D);
      Assert (Condition => Get_Subject_Count (Policy => P) = 3,
              Message   => "Subject count mismatch");

      Iterate (Policy  => P,
               Process => Inc_Iterate_Counter'Access);
      Assert (Condition => Iterate_Count = Get_Subject_Count (Policy => P),
              Message   => "Iterate count mismatch");

      declare
         use type SK.Word16;
         use type SK.Word64;

         S  : constant Subject_Type := Test.First (Policy => P);
         M  : Memory_Layout_Type;
         R  : Memory_Region_Type;
         I  : IO_Ports_Type;
         PR : IO_Port_Range;
      begin
         Assert (Condition => Get_Id (Subject => S) = 0,
                 Message   => "Id mismatch");
         Assert (Condition => Get_Name (Subject => S) = To_Unbounded_String
                 (Source => "tau0"),
                 Message   => "Name mismatch");

         M := Get_Memory_Layout (Subject => S);
         Assert (Condition => Get_Pml4_Address (Layout => M) = 16#1f0000#,
                 Message   => "PML4 address mismatch");
         Assert (Condition => Get_Region_Count (Layout => M) = 2,
                 Message   => "Memory region count mismatch");

         R := Test.First (Layout => M);
         Assert (Condition => Get_Physical_Address (Region => R) = 16#240000#,
                 Message   => "Physical address mismatch (1)");
         Assert (Condition => Get_Virtual_Address (Region => R) = 0,
                 Message   => "Virtual address mismatch (1)");
         Assert (Condition => Get_Size (Region => R) = 65536,
                 Message   => "Memory size mismatch (1)");
         Assert (Condition => Get_Permission (Region => R) = Read_Write,
                 Message   => "Permission mismatch (1)");
         Assert (Condition => Is_Executable (Region => R),
                 Message   => "Executable mismatch (1)");
         R := Test.Last (Layout => M);
         Assert (Condition => Get_Physical_Address (Region => R) = 16#200000#,
                 Message   => "Physical address mismatch (2)");
         Assert (Condition => Get_Virtual_Address (Region => R) = 16#200000#,
                 Message   => "Virtual address mismatch (2)");
         Assert (Condition => Get_Size (Region => R) = 4096,
                 Message   => "Memory size mismatch (2)");
         Assert (Condition => Get_Permission (Region => R) = Read_Only,
                 Message   => "Permission mismatch (2)");
         Assert (Condition => not Is_Executable (Region => R),
                 Message   => "Executable mismatch (2)");

         I := Get_IO_Ports (Subject => S);
         Assert (Condition => Get_Bitmap_Address (Ports => I) = 16#12345678#,
                 Message   => "I/O bitmap address mismatch");

         PR := Test.First (Ports => I);
         Assert (Condition => Get_Start (Port_Range => PR) = 16#50b0#,
                 Message   => "Port start mismatch");
         Assert (Condition => Get_End (Port_Range => PR) = 16#50b0#,
                 Message   => "Port end mismatch");
      end;
   end Xml_To_Policy;

end Xml_Tests;
