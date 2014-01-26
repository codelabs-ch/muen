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

with Ada.Exceptions;

with Muxml;
with Test_Utils;

package body Muxml_Tests
is

   use Ahven;
   use Muxml;

   -------------------------------------------------------------------------

   procedure Initialize (T : in out Testcase)
   is
   begin
      T.Set_Name (Name => "Load XML files");
      T.Add_Test_Routine
        (Routine => Load_Nonexistent_Xml'Access,
         Name    => "Load nonexistent XML");
      T.Add_Test_Routine
        (Routine => Load_Non_Xml_File'Access,
         Name    => "Load non-XML file");
      T.Add_Test_Routine
        (Routine => Load_Invalid_Xml'Access,
         Name    => "Load invalid XML file");
      T.Add_Test_Routine
        (Routine => Load_Policy_Xml'Access,
         Name    => "Load policy from XML");
      T.Add_Test_Routine
        (Routine => Load_And_Store'Access,
         Name    => "Load and store XML");
   end Initialize;

   -------------------------------------------------------------------------

   procedure Load_And_Store
   is
      Data : XML_Data_Type;
   begin
      Parse (Data => Data,
             File => "data/load_and_store.xml");
      Write (Data => Data,
             File => "obj/load_and_store.xml");
      Assert (Condition => Test_Utils.Equal_Files
                  (Filename1 => "data/load_and_store.xml",
                   Filename2 => "obj/load_and_store.xml"),
              Message => "Stored XML differs from loaded one");
   end Load_And_Store;

   -------------------------------------------------------------------------

   procedure Load_Invalid_Xml
   is
      Data : XML_Data_Type;
      pragma Unreferenced (Data);
   begin
      Parse (Data => Data,
             File => "data/invalid.xml");
      Fail (Message => "Exception expected");

   exception
      when Processing_Error => null;
   end Load_Invalid_Xml;

   -------------------------------------------------------------------------

   procedure Load_Non_Xml_File
   is
      Ref_Msg : constant String := "Error reading XML file 'data/invalid' - "
        & "data/invalid:1:1: Non-white space found at top level";
      Data    : XML_Data_Type;
      pragma Unreferenced (Data);
   begin
      Parse (Data => Data,
             File => "data/invalid");
      Fail (Message => "Exception expected");

   exception
      when E : Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Non_Xml_File;

   -------------------------------------------------------------------------

   procedure Load_Nonexistent_Xml
   is
      Ref_Msg : constant String
        := "Error reading XML file 'nonexistent' - Could not open nonexistent";
      Data    : XML_Data_Type;
      pragma Unreferenced (Data);
   begin
      Parse (Data => Data,
             File => "nonexistent");
      Fail (Message => "Exception expected");

   exception
      when E : Processing_Error =>
         Assert (Condition => Ada.Exceptions.Exception_Message
                 (X => E) = Ref_Msg,
                 Message   => "Exception message mismatch");
   end Load_Nonexistent_Xml;

   -------------------------------------------------------------------------

   procedure Load_Policy_Xml
   is
      Data : XML_Data_Type;
      pragma Unreferenced (Data);
   begin
      Parse (Data => Data,
             File => "data/test_policy.xml");

      --  Must not raise an exception.

   end Load_Policy_Xml;

end Muxml_Tests;
