--
--  Copyright (C) 2022 secunet Security Networks AG
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

package Mucfgcheck.Templates
is

   --  Validate the uniqueness of template names.
   procedure Name_Uniqueness (XML_Data : Muxml.XML_Data_Type);

   --  Check if each template in the document has a unique body
   --  and a unique parameter block.
   procedure Template_Integrity (XML_Data : Muxml.XML_Data_Type);

end Mucfgcheck.Templates;
