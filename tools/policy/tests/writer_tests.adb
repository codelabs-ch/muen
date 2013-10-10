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
        (Routine => Write_Packer_Config'Access,
         Name    => "Write packer configuration");
      T.Add_Test_Routine
        (Routine => Write_Hardware'Access,
         Name    => "Write hardware policy files");
      T.Add_Test_Routine
        (Routine => Write_Scheduling'Access,
         Name    => "Write scheduling policy files");
      T.Add_Test_Routine
        (Routine => Write_Interrupts'Access,
         Name    => "Write interrupt policy files");
   end Initialize;

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

   procedure Write_Interrupts
   is
      Spec_File : constant String := "obj/skp-interrupts.ads";
      Data      : Xml.XML_Data_Type;
      Policy    : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Interrupts (Dir_Name => "obj",
                                Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Spec_File,
               Filename2 => "data/skp-interrupts.ref"),
              Message   => "Spec file mismatch");

      Ada.Directories.Delete_File (Name => Spec_File);
   end Write_Interrupts;

   -------------------------------------------------------------------------

   procedure Write_Kernel
   is
      Knl_Pt0  : constant String := "obj/kernel_pt_0";
      Knl_Pt1  : constant String := "obj/kernel_pt_1";
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
              (Filename1 => Knl_Pt0,
               Filename2 => "data/kernel_pt_0.ref"),
              Message   => "Kernel pagetables mismatch (0)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_Pt1,
               Filename2 => "data/kernel_pt_1.ref"),
              Message   => "Kernel pagetables mismatch (1)");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_H,
               Filename2 => "data/policy.h.ref"),
              Message   => "Policy asm include mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Knl_Spec,
               Filename2 => "data/skp-kernel.ref"),
              Message   => "Kernel spec mismatch");

      Ada.Directories.Delete_File (Name => Knl_Pt0);
      Ada.Directories.Delete_File (Name => Knl_Pt1);
      Ada.Directories.Delete_File (Name => Knl_H);
      Ada.Directories.Delete_File (Name => Knl_Spec);
   end Write_Kernel;

   -------------------------------------------------------------------------

   procedure Write_Packer_Config
   is
      Bin_Spec : constant String := "obj/skp-packer_config.ads";
      Data     : Xml.XML_Data_Type;
      Policy   : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Packer_Config (Dir_Name => "obj",
                                   Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Bin_Spec,
               Filename2 => "data/skp-packer_config.ref"),
              Message   => "Packer config mismatch");

      Ada.Directories.Delete_File (Name => Bin_Spec);
   end Write_Packer_Config;

   -------------------------------------------------------------------------

   procedure Write_Scheduling
   is
      Spec_File : constant String := "obj/skp-scheduling.ads";
      Data      : Xml.XML_Data_Type;
      Policy    : Policy_Type;
   begin
      Xml.Parse (Data   => Data,
                 File   => "data/test_policy1.xml",
                 Schema => "schema/system.xsd");

      Policy := Xml.To_Policy (Data => Data);
      Writers.Write_Scheduling (Dir_Name => "obj",
                                Policy   => Policy);

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => Spec_File,
               Filename2 => "data/skp-scheduling.ref"),
              Message   => "Spec file mismatch");

      Ada.Directories.Delete_File (Name => Spec_File);
   end Write_Scheduling;

   -------------------------------------------------------------------------

   procedure Write_Subjects
   is
      Out_File : constant String := "obj/skp-subjects.adb";
      T0_Pts   : constant String := "obj/tau0_pt";
      S1_Pts   : constant String := "obj/subject1_pt";
      S2_Pts   : constant String := "obj/subject2_pt";
      T0_Ibm   : constant String := "obj/tau0_iobm";
      T0_Mbm   : constant String := "obj/tau0_msrbm";
      S1_Ibm   : constant String := "obj/subject1_iobm";
      S1_Mbm   : constant String := "obj/subject1_msrbm";
      S2_Ibm   : constant String := "obj/subject2_iobm";
      S2_Mbm   : constant String := "obj/subject2_msrbm";
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
              (Filename1 => T0_Ibm,
               Filename2 => "data/tau0_iobm.ref"),
              Message   => "Tau0 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S1_Ibm,
               Filename2 => "data/subject1_iobm.ref"),
              Message   => "Subject1 I/O bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S2_Ibm,
               Filename2 => "data/subject2_iobm.ref"),
              Message   => "Subject2 I/O bitmap mismatch");

      --  MSR bitmaps

      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => T0_Mbm,
               Filename2 => "data/tau0_msrbm.ref"),
              Message   => "Tau0 MSR bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S1_Mbm,
               Filename2 => "data/subject1_msrbm.ref"),
              Message   => "Subject1 MSR bitmap mismatch");
      Assert (Condition => Test_Utils.Equal_Files
              (Filename1 => S2_Mbm,
               Filename2 => "data/subject1_msrbm.ref"),
              Message   => "Subject2 MSR bitmap mismatch");

      Ada.Directories.Delete_File (Name => T0_Mbm);
      Ada.Directories.Delete_File (Name => S1_Mbm);
      Ada.Directories.Delete_File (Name => S2_Mbm);
      Ada.Directories.Delete_File (Name => T0_Ibm);
      Ada.Directories.Delete_File (Name => S1_Ibm);
      Ada.Directories.Delete_File (Name => S2_Ibm);
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
