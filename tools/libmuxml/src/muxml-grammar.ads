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

with Schema.Validators;

with Muxml.system_src_schema;
with Muxml.system_src_extended_schema;
with Muxml.system_a_schema;
with Muxml.system_a_extended_schema;
with Muxml.system_b_schema;
with Muxml.system_b_extended_schema;
with Muxml.hardware_config_schema;
with Muxml.hardware_config_extended_schema;
with Muxml.system_config_schema;
with Muxml.system_config_extended_schema;
with Muxml.vcpu_profile_schema;
with Muxml.vcpu_profile_extended_schema;
with Muxml.component_schema;
with Muxml.component_extended_schema;

package Muxml.Grammar
is

   type Schema_Info_Type is record
      Id, XSD : access constant String;
   end record;

   Schema_Map : constant array (Valid_Schema_Kind) of Schema_Info_Type
     := (Format_Src          => (Id  => system_src_schema.Id'Access,
                                 XSD => system_src_schema.Data'Access),
         Format_Src_Ext      => (Id  => system_src_extended_schema.Id'Access,
                                 XSD => system_src_extended_schema.Data'Access),
         Format_A            => (Id  => system_a_schema.Id'Access,
                                 XSD => system_a_schema.Data'Access),
         Format_A_Ext        => (Id  => system_a_extended_schema.Id'Access,
                                 XSD => system_a_extended_schema.Data'Access),
         Format_B            => (Id  => system_b_schema.Id'Access,
                                 XSD => system_b_schema.Data'Access),
         Format_B_Ext        => (Id  => system_b_extended_schema.Id'Access,
                                 XSD => system_b_extended_schema.Data'Access),
         Hardware_Config     => (Id  => hardware_config_schema.Id'Access,
                                 XSD => hardware_config_schema.Data'Access),
         Hardware_Config_Ext => (Id  => hardware_config_extended_schema.Id'Access,
                                 XSD => hardware_config_extended_schema.Data'Access),
         System_Config       => (Id  => system_config_schema.Id'Access,
                                 XSD => system_config_schema.Data'Access),
         System_Config_Ext   => (Id  => system_config_extended_schema.Id'Access,
                                 XSD => system_config_extended_schema.Data'Access),
         VCPU_Profile        => (Id  => vcpu_profile_schema.Id'Access,
                                 XSD => vcpu_profile_schema.Data'Access),
         VCPU_Profile_Ext    => (Id  => vcpu_profile_extended_schema.Id'Access,
                                 XSD => vcpu_profile_extended_schema.Data'Access),
         Component           => (Id  => component_schema.Id'Access,
                                 XSD => component_schema.Data'Access),
         Component_Ext       => (Id  => component_extended_schema.Id'Access,
                                 XSD => component_extended_schema.Data'Access));

   -------------------------------------------------------------------------

   --  Return built-in system policy grammar of given kind.
   function Get_Grammar
     (Kind : Valid_Schema_Kind)
      return Schema.Validators.XML_Grammar;

end Muxml.Grammar;
