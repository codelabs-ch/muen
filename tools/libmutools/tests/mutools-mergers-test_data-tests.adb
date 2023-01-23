--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mutools.Mergers.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only
with Test_Utils;
with DOM.Core;
with DOM.Core.Documents;
with DOM.Core.Elements;
with DOM.Core.Nodes;
with Muxml.Utils;
--  begin read only
--  end read only
package body Mutools.Mergers.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Merge_Config_Section (Gnattest_T : in out Test);
   procedure Test_Merge_Config_Section_d6f1b2 (Gnattest_T : in out Test) renames Test_Merge_Config_Section;
--  id:2.2/d6f1b28e7c4ea535/Merge_Config_Section/1/0/
   procedure Test_Merge_Config_Section (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);
      Policy, New_Policy : Muxml.XML_Data_Type;
      Config : DOM.Core.Node;
      Infile : constant String
         := "data/test_policy_src_evalStringConcatCase.xml";
      Infile_Indented : constant String
         := "data/output_test_policy_src_evalStringConcatCase.xml";
      Outfile : constant String
         := "obj/output_merged_config_section.xml";
      Ref_Outfile_Merged : constant String
         := "data/output_merged_config_section.xml";
      Ref_Outfile_Inserted : constant String
         := "data/output_inserted_config_section.xml";
      New_Config_File : constant String
         := "data/test_config_merge.xml";

      procedure Insert_Empty_Section
      is

      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => Infile);
         Muxml.Parse (Data => New_Policy,
                      Kind => Muxml.None,
                      File => New_Config_File);
         Muxml.Utils.Remove_Child
            (Node       => Muxml.Utils.Get_Element
                (Doc   => New_Policy.Doc,
                 XPath => "/system"),
             Child_Name => "config");
         Config := DOM.Core.Documents.Create_Element
            (Doc      => New_Policy.Doc,
             Tag_Name => "config");
         Config := DOM.Core.Nodes.Append_Child
            (N         => Muxml.Utils.Get_Element
                (Doc   => New_Policy.Doc,
                 XPath => "/system"),
             New_Child => Config);
         Merge_Config_Section
            (Policy     => Policy,
             New_Config => Config,
             Clone      => True);
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Outfile);
         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => Outfile,
                     Filename2 => Infile_Indented),
                 Message   => "Policy mismatch: " & Outfile);
      end Insert_Empty_Section;

      procedure Insert_Normal_Section
      is
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => Infile);
         Muxml.Parse (Data => New_Policy,
                      Kind => Muxml.None,
                      File => New_Config_File);
         Merge_Config_Section
            (Policy     => Policy,
             New_Config => Muxml.Utils.Get_Element
                (Doc   => New_Policy.Doc,
                 XPath => "/system/config"),
             Clone      => True);
         Muxml.Write (Data => Policy,
                      Kind => Muxml.None,
                      File => Outfile);
         Assert (Condition => Test_Utils.Equal_Files
                    (Filename1 => Outfile,
                     Filename2 => Ref_Outfile_Merged),
                 Message   => "Policy mismatch: " & Outfile);
      end Insert_Normal_Section;

      procedure Policy_Without_Config
      is
      begin
         Muxml.Parse (Data => Policy,
                      Kind => Muxml.None,
                      File => Infile);
         Muxml.Parse (Data => New_Policy,
                      Kind => Muxml.None,
                      File => New_Config_File);
         Muxml.Utils.Remove_Child
            (Node       => Muxml.Utils.Get_Element
                (Doc   => Policy.Doc,
                 XPath => "/system"),
             Child_Name => "config");
        Merge_Config_Section
            (Policy     => Policy,
             New_Config =>  Muxml.Utils.Get_Element
                (Doc   => New_Policy.Doc,
                 XPath => "/system/config"),
             Clone      => True);
        Muxml.Write (Data => Policy,
                     Kind => Muxml.None,
                     File => Outfile);
        Assert (Condition => Test_Utils.Equal_Files
                   (Filename1 => Outfile,
                    Filename2 => Ref_Outfile_Inserted),
                 Message   => "Policy mismatch: " & Outfile);
      end Policy_Without_Config;
   begin
      Insert_Normal_Section;
      Insert_Empty_Section;
      Policy_Without_Config;
--  begin read only
   end Test_Merge_Config_Section;
--  end read only

--  begin read only
--  id:2.2/02/
--
--  This section can be used to add elaboration code for the global state.
--
begin
--  end read only
   null;
--  begin read only
--  end read only
end Mutools.Mergers.Test_Data.Tests;
