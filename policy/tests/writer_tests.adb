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
        (Routine => Write_Kernel'Access,
         Name    => "Write kernel policy files");
      T.Add_Test_Routine
        (Routine => Write_Subjects'Access,
         Name    => "Write subject policy files");
      T.Add_Test_Routine
        (Routine => Write_System'Access,
         Name    => "Write system policy files");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Kernel
   is
      Knl_Pts : constant String := "obj/kernel.pt";
      Knl_H   : constant String := "obj/policy.h";
      Data    : Xml.XML_Data_Type;
      Policy  : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Kernel (Dir_Name => "obj",
                            Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_Pts,
               Filename2 => "data/kernel.pt.ref"),
              Message   => "Kernel pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_H,
               Filename2 => "data/policy.h.ref"),
              Message   => "Policy asm include mismatch");

      Ada.Directories.Delete_File (Name => Knl_Pts);
      Ada.Directories.Delete_File (Name => Knl_H);
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Subjects
   is
      Out_File : constant String := "obj/skp-subjects.ads";
      T0_Pts   : constant String := "obj/tau0.pt";
      S1_Pts   : constant String := "obj/subject1.pt";
      S2_Pts   : constant String := "obj/subject2.pt";
      T0_Bm    : constant String := "obj/tau0.iobm";
      S1_Bm    : constant String := "obj/subject1.iobm";
      S2_Bm    : constant String := "obj/subject2.iobm";
      Data     : Xml.XML_Data_Type;
      Policy   : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);

      Writers.Write_Subjects (Dir_Name => "obj",
                              Subjects => Policy.Subjects);

      --  Specs

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Out_File,
               Filename2 => "data/skp-subjects.ref"),
              Message   => "Output file mismatch");

      --  Pagetables

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

      --  I/O bitmaps

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => T0_Bm,
               Filename2 => "data/tau0.iobm.ref"),
              Message   => "Tau0 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S1_Bm,
               Filename2 => "data/subject1.iobm.ref"),
              Message   => "Subject1 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S2_Bm,
               Filename2 => "data/subject2.iobm.ref"),
              Message   => "Subject2 I/O bitmap mismatch");

      Ada.Directories.Delete_File (Name => T0_Bm);
      Ada.Directories.Delete_File (Name => S1_Bm);
      Ada.Directories.Delete_File (Name => S2_Bm);
      Ada.Directories.Delete_File (Name => T0_Pts);
      Ada.Directories.Delete_File (Name => S1_Pts);
      Ada.Directories.Delete_File (Name => S2_Pts);
      Ada.Directories.Delete_File (Name => Out_File);
   end Write_Subjects;

   -------------------------------------------------------------------------

   procedure Write_System
   is
      Policy_H : constant String := "obj/policy.h";
      Data     : Xml.XML_Data_Type;
      Policy   : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_System (Dir_Name => "obj",
                            Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Policy_H,
               Filename2 => "data/policy.h.system.ref"),
              Message   => "Policy asm include mismatch");

      Ada.Directories.Delete_File (Name => Policy_H);
   end Write_System;

end Writer_Tests;
