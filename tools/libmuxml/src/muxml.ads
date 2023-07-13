--
--  Copyright (C) 2013, 2014  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2013, 2014  Adrian-Ken Rueegsegger <ken@codelabs.ch>
--  Copyright (C) 2014        Alexander Senier <mail@senier.net>
--  Copyright (C) 2023        secunet Security Networks AG

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

with Ada.Finalization;

with Ada.Containers.Indefinite_Vectors;

with DOM.Core;

package Muxml
is
   use type DOM.Core.Node;

   type Schema_Kind is
     (None,
      Component,
      Component_Ext,
      Format_A,
      Format_A_Ext,
      Format_B,
      Format_B_Ext,
      Format_Src,
      Format_Src_Ext,
      Hardware_Config,
      Hardware_Config_Ext,
      System_Config,
      System_Config_Ext,
      VCPU_Profile,
      VCPU_Profile_Ext);

   subtype Valid_Schema_Kind is Schema_Kind range
     Schema_Kind'Succ (Schema_Kind'First) .. Schema_Kind'Last;

   --  DOM tree of XML document.
   type XML_Data_Type is new Ada.Finalization.Limited_Controlled with record
      Doc : DOM.Core.Document;
   end record;

   --  Parse the contents of given file into the DOM data structure. The XML
   --  data is validated against the built-in system policy XML schema.
   --  If Add_Location is True, the parser adds an attribute "originOfNode"
   --  stating filename and location within the file to each element-node.
   procedure Parse
     (Data         : out XML_Data_Type;
      Kind         :     Schema_Kind;
      File         :     String;
      Add_Location :     Boolean := False);

   --  Parse the given XML string into the DOM data structure. The XML data is
   --  validated against the specified built-in XML schema.
   procedure Parse_String
     (Data : out XML_Data_Type;
      Kind :     Schema_Kind;
      XML  :     String);

   --  Write the given DOM data structure to an XML file.
   procedure Write
     (Data : XML_Data_Type;
      Kind : Schema_Kind;
      File : String);

   Validation_Error : exception;
   XML_Input_Error  : exception;

   package String_Vector is new Ada.Containers.Indefinite_Vectors
      (Element_Type => String,
       Index_Type   => Natural);

   package Node_Vector is new Ada.Containers.Indefinite_Vectors
         (Index_Type   => Natural,
          Element_Type => DOM.Core.Node);

private

   --  Free XML document.
   overriding
   procedure Finalize (Object : in out XML_Data_Type);

end Muxml;
