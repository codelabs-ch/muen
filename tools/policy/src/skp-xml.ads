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

private with DOM.Core;
private with Ada.Finalization;

package Skp.Xml
is

   --  DOM tree of XML document.
   type XML_Data_Type is private;

   --  Parse the contents of given file into the DOM data structure. The XML
   --  data is validated against the given XML schema.
   procedure Parse
     (Data   : in out XML_Data_Type;
      File   :        String;
      Schema :        String);

   --  Create SK system policy from given XML document.
   function To_Policy (Data : XML_Data_Type) return Policy_Type;

   Processing_Error : exception;

private

   type XML_Data_Type is new Ada.Finalization.Controlled with record
      Doc : DOM.Core.Document;
   end record;

   --  Free XML document.
   overriding
   procedure Finalize (Object : in out XML_Data_Type);

end Skp.Xml;
