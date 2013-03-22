with Ada.Directories;

with Skp.Xml;
with Skp.Writers;

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
      T.Add_Test_Routine
        (Routine => Write_Pagetables'Access,
         Name    => "Write pagetables");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Pagetables
   is
      T0_Pts : constant String := "obj/tau0.pt";
      S1_Pts : constant String := "obj/subject1.pt";
      S2_Pts : constant String := "obj/subject2.pt";
      Data   : Xml.XML_Data_Type;
      Policy : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Pagetables (Dir_Name => "obj",
                                Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => T0_Pts,
               Filename2 => "data/tau0.pt.ref"),
              Message   => "Tau0 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S1_Pts,
               Filename2 => "data/subject1.pt.ref"),
              Message   => "Subject1 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S2_Pts,
               Filename2 => "data/subject2.pt.ref"),
              Message   => "Subject2 pagetables mismatch");

      Ada.Directories.Delete_File (Name => T0_Pts);
      Ada.Directories.Delete_File (Name => S1_Pts);
      Ada.Directories.Delete_File (Name => S2_Pts);
   end Write_Pagetables;

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

      Writers.Write_Subjects (File_Name    => Out_File,
                              Package_Name => "Skp.Subjects",
                              Policy       => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Out_File,
               Filename2 => "data/skp-subjects.ref"),
              Message   => "Output file mismatch");

      Ada.Directories.Delete_File (Name => Out_File);
   end Write_Subjects;

end Writer_Tests;
