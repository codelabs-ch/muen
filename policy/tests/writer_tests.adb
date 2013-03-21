with Ada.Directories;

with Skp.Xml;
with Skp.Specs;

with Test_Utils;

package body Writer_Tests
is

   use Ahven;
   use Skp;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Writer tests");
      T.Add_Test_Routine
        (Routine => Write_Subjects'Access,
         Name    => "Write subject specifications");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Subjects
   is
      Out_File : constant String := "obj/skp-subjects.ads";
      Data     : Xml.XML_Data_Type;
      Policy   : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);

      Specs.Write_Subjects (File_Name    => Out_File,
                            Package_Name => "Skp.Subjects",
                            Policy       => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Out_File,
               Filename2 => "data/skp-subjects.ref"),
              Message   => "Output file mismatch");

      Ada.Directories.Delete_File (Name => Out_File);
   end Write_Subjects;

end Writer_Tests;
