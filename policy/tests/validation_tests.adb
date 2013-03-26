with Ada.Exceptions;

with Skp.Xml;
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
   end Initialize;

   -------------------------------------------------------------------------

   procedure Invalid_Memregion_Addrs
   is
      D : Xml.XML_Data_Type;
      P : Policy_Type;
      pragma Unreferenced (P);
   begin
      Xml.Parse (Data   => D,
                 File   => "data/invalid_memregion_addr_phys.xml",
                 Schema => "schema/system.xsd");

      begin
         P := Xml.To_Policy (Data => D);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject tau0: Invalid memory region physical address "
                    & "0000000000000023 for specified alignment "
                    & "0000000000001000",
                    Message   => "Exception message mismatch");
      end;
   end Invalid_Memregion_Addrs;

   -------------------------------------------------------------------------

   procedure Invalid_Memregion_Size
   is
      D : Xml.XML_Data_Type;
      P : Policy_Type;
      pragma Unreferenced (P);
   begin
      Xml.Parse (Data   => D,
                 File   => "data/invalid_memregion_size.xml",
                 Schema => "schema/system.xsd");

      begin
         P := Xml.To_Policy (Data => D);
         Fail (Message => "Exception expected");

      exception
         when E : Validators.Validation_Error =>
            Assert (Condition => Ada.Exceptions.Exception_Message (X => E)
                    = "Subject tau0: Invalid memory region size "
                    & "0000000000010000 for specified alignment "
                    & "0000000000200000",
                    Message   => "Exception message mismatch");
      end;
   end Invalid_Memregion_Size;

end Validation_Tests;
