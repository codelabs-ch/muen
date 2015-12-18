--
--  Copyright (C) 2015  Reto Buerki <reet@codelabs.ch>
--  Copyright (C) 2015  Adrian-Ken Rueegsegger <ken@codelabs.ch>
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

package Expanders.Platform
is

   --  Add section skeleton.
   procedure Add_Section_Skeleton (Data : in out Muxml.XML_Data_Type);

   --  Resolve device aliases including resource references.
   procedure Resolve_Device_Aliases (Data : in out Muxml.XML_Data_Type);

   --  Add all alias resources to subject devices that do not explicitly
   --  specify any resources.
   procedure Add_Subject_Device_Resources (Data : in out Muxml.XML_Data_Type);

end Expanders.Platform;
