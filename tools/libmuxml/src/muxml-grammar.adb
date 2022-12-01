--
--  Copyright (C) 2013  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2014  Alexander Senier <mail@senier.net>
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

with Input_Sources.Strings;
with Schema.Schema_Readers;
with Unicode.CES.Utf8;

with Muxml.system_src_schema;
with Muxml.system_a_schema;
with Muxml.system_b_schema;
with Muxml.hardware_config_schema;
with Muxml.system_config_schema;
with Muxml.vcpu_profile_schema;
with Muxml.component_schema;

package body Muxml.Grammar
is

   type Schema_Info_Type is record
      Id, XSD : access constant String;
   end record;

   Schema_Map : constant array (Valid_Schema_Kind) of Schema_Info_Type
     := (Format_Src      => (Id  => system_src_schema.Id'Access,
                             XSD => system_src_schema.Data'Access),
         Format_A        => (Id  => system_a_schema.Id'Access,
                             XSD => system_a_schema.Data'Access),
         Format_B        => (Id  => system_b_schema.Id'Access,
                             XSD => system_b_schema.Data'Access),
         Hardware_Config => (Id  => hardware_config_schema.Id'Access,
                             XSD => hardware_config_schema.Data'Access),
         System_Config   => (Id  => system_config_schema.Id'Access,
                             XSD => system_config_schema.Data'Access),
         VCPU_Profile    => (Id  => vcpu_profile_schema.Id'Access,
                             XSD => vcpu_profile_schema.Data'Access),
         Component       => (Id  => component_schema.Id'Access,
                             XSD => component_schema.Data'Access));

   -------------------------------------------------------------------------

   function Get_Grammar
     (Kind : Valid_Schema_Kind)
      return Schema.Validators.XML_Grammar
   is
      Reader      : Schema.Schema_Readers.Schema_Reader;
      Str_Input   : Input_Sources.Strings.String_Input;
      Schema_Info : constant Schema_Info_Type := Schema_Map (Kind);
   begin
      Str_Input.Set_Public_Id (Id => Schema_Info.Id.all);
      Input_Sources.Strings.Open
        (Str      => Schema_Info.XSD,
         Encoding => Unicode.CES.Utf8.Utf8_Encoding,
         Input    => Str_Input);

      begin
         Schema.Schema_Readers.Parse
           (Parser => Reader,
            Input  => Str_Input);
      exception
         when others =>
            Input_Sources.Strings.Close (Input => Str_Input);
            Reader.Free;
            raise;
      end;

      Input_Sources.Strings.Close (Input => Str_Input);

      return Reader.Get_Grammar;

   exception
      when Schema.Validators.XML_Validation_Error =>
         raise Validation_Error with "XML validation error - "
           & Reader.Get_Error_Message;
      when E : others =>
         raise Validation_Error with "Error validating " & Kind'Img
           & " XSD - " & Ada.Exceptions.Exception_Message (X => E);
   end Get_Grammar;

end Muxml.Grammar;
