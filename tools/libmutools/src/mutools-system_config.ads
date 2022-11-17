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

package Mutools.System_Config
is

   --  Returns True if a boolean config value with specified name exists.
   function Has_Boolean
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean;

   --  Returns True if an integer config value with specified name exists.
   function Has_Integer
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean;

   --  Returns True if a string config value with specified name exists.
   function Has_String
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean;

   --  Returns True if a config value of any type with specified name exists.
   function Has_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean;

   --  Return boolean config value specified by name. An exception is raised if
   --  no boolean config option with the given name exists.
   function Get_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Boolean;

   --  Return integer config value specified by name. An exception is raised if
   --  no integer config option with the given name exists.
   function Get_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return Integer;

   --  Return string config value specified by name. An exception is raised if
   --  no string config option with the given name exists.
   function Get_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return String;

   --  Return raw config value specified by name as string. An exception is
   --  raised if no config option with the given name exists.
   function Get_Raw_Value
     (Data : Muxml.XML_Data_Type;
      Name : String)
      return String;

   --  Set value of boolean config variable specified by name to given value. A
   --  config entry is created if it does not yet exist.
   procedure Set_Value
     (Data  : Muxml.XML_Data_Type;
      Name  : String;
      Value : Boolean);

   --  Set value of integer config variable specified by name to given value. A
   --  config entry is created if it does not yet exist.
   procedure Set_Value
     (Data  : Muxml.XML_Data_Type;
      Name  : String;
      Value : Integer);

   --  Set value of string config variable specified by name to given value. A
   --  config entry is created if it does not yet exist.
   procedure Set_Value
     (Data  : Muxml.XML_Data_Type;
      Name  : String;
      Value : String);

   Not_Found : exception;

end Mutools.System_Config;
