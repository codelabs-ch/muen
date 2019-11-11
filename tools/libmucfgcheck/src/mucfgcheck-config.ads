--
--  Copyright (C) 2016  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2016  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

with Muxml;

package Mucfgcheck.Config
is

   --  Validate config variable name uniqueness.
   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Check that all expression config variable references are valid.
   procedure Expression_Config_Var_Refs (XML_Data : Muxml.XML_Data_Type);

   --  Check that all integers defined in expressions contain a valid value.
   procedure Expression_Integer_Values (XML_Data : Muxml.XML_Data_Type);

   --  Check that all booleans defined in expressions contain a valid value.
   procedure Expression_Boolean_Values (XML_Data : Muxml.XML_Data_Type);

   --  Check that all conditional config variable references are valid.
   procedure Conditional_Config_Var_Refs (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Config;
