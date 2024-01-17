--
--  Copyright (C) 2023 secunet Security Networks AG
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

with Mulog;

with Muxml;
with Muxml.Grammar;
with Muxml.Grammar_Tools;

with Ada.Text_IO;
with Ada.Strings.Unbounded;

package body Xmlfilter
is

   procedure Run
     (Input_Xml_Path     : String;
      Input_Schema_Name  : String;
      Output_Xml_Path    : String;
      Output_Schema_Name : String;
      Output_Schema_Path : String)
   is
      package ASU renames Ada.Strings.Unbounded;
      use all type ASU.Unbounded_String;

      -- Maps the given string to the internal schema identifier
      function Get_Schema_Type (Schema_Name : String) return Muxml.Schema_Kind;

      ----------------------------------------------------------------------

      -- Return schema definition of given schema
      function Get_Schema_String
        (Schema_Type         : Muxml.Schema_Kind;
         Path_To_Schema_File : String := "")
        return String;

      ----------------------------------------------------------------------

      function Get_Schema_String
        (Schema_Type         : Muxml.Schema_Kind;
         Path_To_Schema_File : String := "")
        return String
      is
         use type Muxml.Schema_Kind;

         File_Handle   : Ada.Text_IO.File_Type;
         Schema_String : ASU.Unbounded_String;
      begin
         if Path_To_Schema_File = "" and Schema_Type /= Muxml.None then
            return Muxml.Grammar.Schema_Map (Schema_Type).XSD.all;
         elsif Path_To_Schema_File /= "" then
            Ada.Text_IO.Open (File => File_Handle,
                              Mode => Ada.Text_IO.In_File,
                              Name => Output_Schema_Path);
            while not Ada.Text_IO.End_Of_File (File_Handle) loop
               Schema_String := Schema_String & ASU.To_Unbounded_String
                 (Ada.Text_IO.Get_Line (File_Handle));
            end loop;
            Ada.Text_IO.Close (File_Handle);
            return ASU.To_String (Schema_String);
         else
            raise Validation_Error with
              "Output schema must not be 'None'";
         end if;
      end Get_Schema_String;

      ----------------------------------------------------------------------

      function Get_Schema_Type (Schema_Name : String) return Muxml.Schema_Kind
      is
      begin
         if Schema_Name = "" then
            return Muxml.None;
         else
            return Muxml.Schema_Kind'Value (Schema_Name);
         end if;
      exception
         when Constraint_Error =>
            raise Validation_Error with
              "Unknown schema name '" & Schema_Name & "'";
      end Get_Schema_Type;

      XML_Document       : Muxml.XML_Data_Type;
      Output_Schema_Type : constant Muxml.Schema_Kind
        := Get_Schema_Type (Schema_Name => Output_Schema_Name);

   begin
      Mulog.Log (Msg => "Parsing XML-data at '" & Input_Xml_Path & "'");
      Muxml.Parse (Data => XML_Document,
                   Kind => Get_Schema_Type
                     (Schema_Name => Input_Schema_Name),
                   File => Input_Xml_Path);

      Mulog.Log (Msg => "Initializing schema information");
      Muxml.Grammar_Tools.Init_Order_Information
        (Schema_XML_Data => Get_Schema_String
           (Schema_Type         => Output_Schema_Type,
            Path_To_Schema_File => Output_Schema_Path));

      Mulog.Log (Msg => "Filtering XML-data");
      Muxml.Grammar_Tools.Filter_XML (XML_Data => XML_Document);

      Muxml.Write
        (File => Output_Xml_Path,
         Kind => Output_Schema_Type,
         Data => XML_Document);
      Mulog.Log (Msg => "Successfully wrote filtered XML data to '"
                   & Output_Xml_Path & "'");
   end Run;
end Xmlfilter;
