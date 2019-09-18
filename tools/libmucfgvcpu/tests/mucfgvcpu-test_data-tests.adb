--  This package has been generated automatically by GNATtest.
--  You are allowed to add your code to the bodies of test routines.
--  Such changes will be kept during further regeneration of this file.
--  All code placed outside of test routine bodies will be lost. The
--  code intended to set up and tear down the test environment should be
--  placed into Mucfgvcpu.Test_Data.

with AUnit.Assertions; use AUnit.Assertions;
with System.Assertions;

--  begin read only
--  id:2.2/00/
--
--  This section can be used to add with clauses if necessary.
--
--  end read only

--  begin read only
--  end read only
package body Mucfgvcpu.Test_Data.Tests is

--  begin read only
--  id:2.2/01/
--
--  This section can be used to add global variables and other elements.
--
--  end read only

--  begin read only
--  end read only

--  begin read only
   procedure Test_Set_VCPU_Profile (Gnattest_T : in out Test);
   procedure Test_Set_VCPU_Profile_d4ed23 (Gnattest_T : in out Test) renames Test_Set_VCPU_Profile;
--  id:2.2/d4ed23397aebc638/Set_VCPU_Profile/1/0/
   procedure Test_Set_VCPU_Profile (Gnattest_T : in out Test) is
--  end read only

      pragma Unreferenced (Gnattest_T);

      ----------------------------------------------------------------------

      procedure Merge_User_VCPU_Profile
      is
         Data : Muxml.XML_Data_Type;
         Node : DOM.Core.Node;
      begin
         Muxml.Parse (Data => Data,
                      Kind => Muxml.None,
                      File => "data/user_profile.xml");
         Node := DOM.Core.Documents.Get_Element (Doc => Data.Doc);
         Set_VCPU_Profile
           (Profile => VM,
            Node    => Node);

         Muxml.Write (Data => Data,
                      Kind => Muxml.VCPU_Profile,
                      File => "obj/merged_user_profile.xml");

         Assert (Condition => Test_Utils.Equal_Files
                 (Filename1 => "data/merged_user_profile.xml",
                  Filename2 => "obj/merged_user_profile.xml"),
                 Message   => "Merged VCPU profile differs");
         Ada.Directories.Delete_File (Name => "obj/merged_user_profile.xml");
      end Merge_User_VCPU_Profile;

      ----------------------------------------------------------------------

      procedure Set_VCPU_Profile
      is
      begin
         for Profile in Profile_Type loop
            declare
               Data : Muxml.XML_Data_Type;
               Impl : DOM.Core.DOM_Implementation;
               Node : DOM.Core.Node;

               Profile_Str : constant String
                 := Ada.Strings.Fixed.Translate
                   (Source  => Profile'Img,
                    Mapping => Ada.Strings.Maps.Constants.Lower_Case_Map);
               Filename    : constant String
                 := "vcpu_profile_" & Profile_Str & ".xml";
            begin
               Data.Doc := DOM.Core.Create_Document
                 (Implementation => Impl);
               Node := DOM.Core.Documents.Create_Element
                 (Doc      => Data.Doc,
                  Tag_Name => "vcpu");
               Muxml.Utils.Append_Child
                 (Node      => Data.Doc,
                  New_Child => Node);
               Set_VCPU_Profile
                 (Profile => Profile,
                  Node    => Node);

               Muxml.Write (Data => Data,
                            Kind => Muxml.VCPU_Profile,
                            File => "obj/" & Filename);

               Assert (Condition => Test_Utils.Equal_Files
                       (Filename1 => "data/" & Filename,
                        Filename2 => "obj/" & Filename),
                       Message   => "VCPU profile differs: " & Profile_Str);
               Ada.Directories.Delete_File (Name => "obj/" & Filename);
            end;
         end loop;
      end Set_VCPU_Profile;
   begin
      Set_VCPU_Profile;
      Merge_User_VCPU_Profile;
--  begin read only
   end Test_Set_VCPU_Profile;
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
end Mucfgvcpu.Test_Data.Tests;
