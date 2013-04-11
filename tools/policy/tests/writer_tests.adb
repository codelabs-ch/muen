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
      T.Add_Test_Routine
        (Routine => Write_Binaries'Access,
         Name    => "Write binary information files");
      T.Add_Test_Routine
        (Routine => Write_Hardware'Access,
         Name    => "Write hardware policy files");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Write_Binaries
   is
      Bin_Spec : constant String := "obj/skp-binaries.ads";
      Data     : Xml.XML_Data_Type;
      Policy   : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Binaries (Dir_Name => "obj",
                              Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Bin_Spec,
               Filename2 => "data/skp-binaries.ref"),
              Message   => "Binary spec mismatch");

      Ada.Directories.Delete_File (Name => Bin_Spec);
   end Write_Binaries;

   -------------------------------------------------------------------------

   procedure Write_Hardware
   is
      Spec_File : constant String := "obj/skp-hardware.ads";
      Data      : Xml.XML_Data_Type;
      Policy    : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Hardware (Dir_Name => "obj",
                              Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Spec_File,
               Filename2 => "data/skp-hardware.ref"),
              Message   => "Spec file mismatch");

      Ada.Directories.Delete_File (Name => Spec_File);
   end Write_Hardware;

   -------------------------------------------------------------------------

   procedure Write_Kernel
   is
      Knl_Pts  : constant String := "obj/kernel_pt";
      Knl_H    : constant String := "obj/policy.h";
      Knl_Spec : constant String := "obj/skp-kernel.ads";
      Data     : Xml.XML_Data_Type;
      Policy   : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Kernel (Dir_Name => "obj",
                            Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_Pts,
               Filename2 => "data/kernel_pt.ref"),
              Message   => "Kernel pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_H,
               Filename2 => "data/policy.h.ref"),
              Message   => "Policy asm include mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_Spec,
               Filename2 => "data/skp-kernel.ref"),
              Message   => "Kernel spec mismatch");

      Ada.Directories.Delete_File (Name => Knl_Pts);
      Ada.Directories.Delete_File (Name => Knl_H);
      Ada.Directories.Delete_File (Name => Knl_Spec);
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Subjects
   is
      Out_File : constant String := "obj/skp-subjects.ads";
      T0_Pts   : constant String := "obj/tau0_pt";
      S1_Pts   : constant String := "obj/subject1_pt";
      S2_Pts   : constant String := "obj/subject2_pt";
      T0_Bm    : constant String := "obj/tau0_iobm";
      S1_Bm    : constant String := "obj/subject1_iobm";
      S2_Bm    : constant String := "obj/subject2_iobm";
      Data     : Xml.XML_Data_Type;
      Policy   : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);

      Writers.Write_Subjects (Dir_Name => "obj",
                              Policy   => Policy);

      --  Specs

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Out_File,
               Filename2 => "data/skp-subjects.ref"),
              Message   => "Output file mismatch");

      --  Pagetables

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => T0_Pts,
               Filename2 => "data/tau0_pt.ref"),
              Message   => "Tau0 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S1_Pts,
               Filename2 => "data/subject1_pt.ref"),
              Message   => "Subject1 pagetables mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S2_Pts,
               Filename2 => "data/subject2_pt.ref"),
              Message   => "Subject2 pagetables mismatch");

      --  I/O bitmaps

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => T0_Bm,
               Filename2 => "data/tau0_iobm.ref"),
              Message   => "Tau0 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S1_Bm,
               Filename2 => "data/subject1_iobm.ref"),
              Message   => "Subject1 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S2_Bm,
               Filename2 => "data/subject2_iobm.ref"),
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
      Spec_File : constant String := "obj/skp.ads";
      Policy_H  : constant String := "obj/policy.h";
      Data      : Xml.XML_Data_Type;
      Policy    : Policy_Type;
   begin
      Ada.Directories.Copy_File (Source_Name => "templates/policy.h",
                                 Target_Name => Policy_H);

      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_System (Dir_Name => "obj",
                            Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Spec_File,
               Filename2 => "data/skp.ref"),
              Message   => "Spec file mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Policy_H,
               Filename2 => "data/policy.h.system.ref"),
              Message   => "Policy asm include mismatch");

      Ada.Directories.Delete_File (Name => Spec_File);
      Ada.Directories.Delete_File (Name => Policy_H);
   end Write_System;

end Writer_Tests;
